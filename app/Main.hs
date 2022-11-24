module Main (main) where

import Control.Monad.Catch              (handle, Exception (displayException))
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Control.Lens                     ((.~))
import Data.Aeson                       ((.=), encode, object)
import Data.Function                    ((&))
import Data.Functor                     ((<&>))
import Data.Text                        (Text)
import Options.Applicative
import System.IO                        (hPutStrLn, stdout, stderr)
import Text.Printf                      (printf)
import Terra                            (Environment, WorkspaceName(..))

import qualified Data.ByteString.Lazy.UTF8          as LBS
import qualified Data.ByteString.UTF8               as BS
import qualified Database.MySQL.Rawls               as Rawls
import qualified Database.MySQL.Simple              as MySQL
import qualified Database.MySQL.Simple.Types        as MySQL
import qualified Database.Vault                     as Vault
import qualified Streaming.Prelude                  as S
import qualified Network.Google                     as Google
import qualified Network.Google.Metrics             as Google
import qualified Network.Google.Monitoring.Types    as Google

main :: IO ()
main = execParser (info (config <**> helper) description) >>= run

run :: Configuration -> IO ()
run Config{..} = runResourceT $ do
    -- The vault environment encodes all we need to connect to a
    -- hashicorp vault server.
    vEnv <- Vault.newEnv
    connectInfo <- Vault.runVault vEnv (Rawls.readRawlsConnectInfo environment)

    glogger <- flip Google.newLogger stdout $ case verbosity of
        Normal -> Google.Info
        Verbose -> Google.Debug

    -- The google environment encodes the scopes and credentials used to call
    -- Google APIs, as well as how we log those requests. Incidently, gogol
    -- logging is fantastic (relative to JVM ecoystems) - one only wishes
    -- gogol's `logger` internals  where exposed.
    gEnv <- Google.newEnv
        <&> (Google.envLogger .~ glogger)
        <&> (Google.envScopes .~ Google.monitoringReadScope)

    let (query, params) = case input of
            Workspace w ->
                (mkQuery "AND NAMESPACE = ? AND NAME = ?",
                [namespace w, name w])
            AllWorkspaces -> (mkQuery "", [])

    liftIO $ printf "namespace,name,objectCount,totalBytes\n"
    runResourceT . Google.runGoogle gEnv
        $ Rawls.stream connectInfo query params
        & S.mapM (\r -> handle (liftIO . (*> pure Nothing) . logGoogleError r) (getMetrics r))
        & S.catMaybes
        & S.map formatCsv
        & S.stdoutLn
 where
    mkQuery :: String -> MySQL.Query
    mkQuery params = MySQL.Query . BS.fromString . unwords $ filter (not . null)
        [ "SELECT NAMESPACE,NAME,GOOGLE_PROJECT_ID,BUCKET_NAME FROM WORKSPACE"
        , "WHERE WORKSPACE_TYPE = 'rawls'"
        , params
        , "ORDER BY NAMESPACE, NAME"
        ]

    logGoogleError :: (Text, Text, Google.ProjectId, Google.BucketName) -> Google.Error -> IO ()
    logGoogleError (namespace, name, project, bucket) err =
        hPutStrLn stderr . LBS.toString . encode $ object
            [ "namespace" .= namespace
            , "name" .= name
            , "project" .= project
            , "bucket" .= bucket
            , "error" .= displayException err
            ]

    getMetrics (namespace, name, project, bucket) =
        Google.getBucketMetrics project bucket
            <&> liftA2 ((Just .) . (namespace, name,,)) Google.objectCount Google.totalBytes

    formatCsv (namespace, name, objectCount, totalBytes) =
        printf "%s,%s,%d,%.f"
            (namespace :: Text)
            (name :: Text)
            objectCount
            totalBytes


data Configuration = Config
    { environment :: !Environment
    , verbosity :: !Verbosity
    , input :: !Input
    }
    deriving stock Show


data Input = Workspace !WorkspaceName
           | AllWorkspaces
    deriving stock Show


data Verbosity = Normal | Verbose
    deriving stock Show


config :: Parser Configuration
config = Config
    <$> option auto
        ( help "The Terra Environment to use"
       <> long "environment"
       <> short 'e'
       <> metavar "ENVIRONMENT"
        )
    <*> flag Normal Verbose (long "verbose" <> short 'v' <> help "Be verbose.")
    <*> (allWorkspaces <|> oneWorkspace)
  where
    allWorkspaces = flag' AllWorkspaces
        ( long "all"
       <> help ("Print metrics for all workspaces in ENVIRONMENT. "
             ++ "This may take a very long time.")
        )

    oneWorkspace = (Workspace .) . WorkspaceName
        <$> argument str (metavar "NAMESPACE" <> help "Workspace namespace or billing project")
        <*> argument str (metavar "NAME" <> help "Workspace name")


description :: InfoMod a
description = fullDesc
    <> progDesc "Print Google Cloud Storage metrics for Terra Workspaces in ENVIRONMENT"
    <> footer "Copyright (C) 2022 Edmund Higham <edhigham@gmail.com>"

module Main (main) where

import Control.Exception                (handle)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Control.Lens                     ((.~))
import Data.Functor                     ((<&>))
import Data.Text                        (Text)
import Options.Applicative
import Network.Google.Monitoring.Types  (monitoringReadScope)
import Network.Google.Metrics           (BucketMetrics(..), getBucketMetrics)
import System.IO                        (hPrint, stdout, stderr)
import Text.Printf                      (printf)
import Terra                            (Environment, WorkspaceName(..))

import qualified Data.ByteString.UTF8         as BS
import qualified Database.MySQL.Rawls         as Rawls
import qualified Database.MySQL.Simple        as MySQL
import qualified Database.MySQL.Simple.Types  as MySQL
import qualified Database.Vault               as Vault
import qualified Network.Google               as Google

main :: IO ()
main = execParser (info (config <**> helper) description) >>= run

run :: Configuration -> IO ()
run Config{..} = runResourceT $ do
    vEnv <- Vault.newEnv
    connectInfo <- Vault.runVault vEnv (Rawls.readRawlsConnectInfo environment)

    glogger <- flip Google.newLogger stdout $ case verbosity of
        Normal -> Google.Info
        Verbose -> Google.Debug

    gEnv <- Google.newEnv
        <&> (Google.envLogger .~ glogger)
        <&> (Google.envScopes .~ monitoringReadScope)

    conn <- Rawls.openConnection connectInfo

    let (query, params) = case input of
            Workspace w ->
                (queryBase <> " AND NAMESPACE = ? AND NAME = ?",
                [namespace w, name w])
            AllWorkspaces -> (queryBase, [])

    liftIO $ printf "namespace,name,objectCount,totalBytes\n"
    liftIO . MySQL.forEach conn (query <> ordering) params $
        \(namespace, name, googleProjectId, bucketName) ->
            let printMetrics = do
                BucketMetrics{..} <- runResourceT . Google.runGoogle gEnv $
                    getBucketMetrics googleProjectId bucketName
                printf "%s,%s,%d,%.f\n"
                    (namespace :: Text)
                    (name :: Text)
                    objectCount
                    totalBytes
            in handle (hPrint stderr :: Google.Error -> IO ()) printMetrics
 where
    queryBase, ordering :: MySQL.Query
    queryBase = MySQL.Query . BS.fromString $ unwords
        [ "SELECT NAMESPACE,NAME,GOOGLE_PROJECT_ID,BUCKET_NAME FROM WORKSPACE"
        , "WHERE WORKSPACE_TYPE = 'rawls'"
        ]
    ordering = " ORDER BY NAMESPACE, NAME"


data Configuration = Config
    { environment :: !Environment
    , verbosity :: !Verbosity
    , input :: !Input
    }
    deriving stock Show


data Input = Workspace WorkspaceName
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

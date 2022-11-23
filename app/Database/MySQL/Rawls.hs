{-# LANGUAGE InstanceSigs #-}
module Database.MySQL.Rawls
    ( localRawlsConnectInfo
    , openConnection
    , readRawlsConnectInfo
    ) where

import Data.Aeson
    ( (.:)
    , FromJSON
    , Value
    , parseJSON
    , withObject
    )
import Data.Aeson.Types                        (Parser)
import Control.Monad.Trans.Resource            (MonadResource, allocate)
import Control.Monad                           ((>=>))
import Data.String.Interpolate                 (i)
import qualified Database.MySQL.Base as MySQL
import Database.Vault.Class                    (MonadVault)
import Database.Vault.KVv1                     (readSecret)
import System.Directory                        (removeFile, removeDirectory)
import System.IO.Temp
    ( createTempDirectory
    , writeTempFile
    , getCanonicalTemporaryDirectory
    )
import Terra                                    (Environment)


localRawlsConnectInfo :: MySQL.ConnectInfo
localRawlsConnectInfo = MySQL.defaultConnectInfo
    { MySQL.connectHost = "127.0.0.1"
    , MySQL.connectPort = 3310
    , MySQL.connectDatabase = "testdb"
    , MySQL.connectUser = "rawls-test"
    , MySQL.connectPassword = "rawls-test"
    , MySQL.connectSSL = Nothing
    }


openConnection :: (MonadResource m)
    => MySQL.ConnectInfo
    -> m MySQL.Connection
openConnection info = snd <$> allocate (MySQL.connect info) MySQL.close


readRawlsConnectInfo :: (MonadResource m, MonadVault m)
    => Environment
    -> m MySQL.ConnectInfo
readRawlsConnectInfo env = do
    MySQLUser{..} <- readSecret [i|secret/dsde/firecloud/#{env}/rawls/rawls.conf|]
    sslConfig <- readSecret [i|secret/dsde/firecloud/#{env}/rawls/rawls-mysql|]
    (caFile, certFile, keyFile) <- writeSSL sslConfig
    return $ MySQL.defaultConnectInfo
        { MySQL.connectHost = [i|rawls-mysql.dsde-#{env}.broadinstitute.org|]
        , MySQL.connectDatabase = "rawls"
        , MySQL.connectUser = username
        , MySQL.connectPassword = password
        , MySQL.connectSSL = Just $ MySQL.defaultSSLInfo
            { MySQL.sslKey = keyFile
            , MySQL.sslCert = certFile
            , MySQL.sslCA = caFile
            }
        }
  where
    writeSSL SSLConfig{..} = do
        tmp <- mkTempDir ()
        caFile <- writeTmpFile tmp clientCA
        certFile <- writeTmpFile tmp clientCertificate
        keyFile <- writeTmpFile tmp clientKey
        return (caFile, certFile, keyFile)

    mkTempDir () = snd <$> allocate
        (getCanonicalTemporaryDirectory >>= \tmp -> createTempDirectory tmp "rawls")
        removeDirectory

    writeTmpFile folder contents = snd <$> allocate
        (writeTempFile folder "" contents)
        removeFile


data MySQLUser = MySQLUser
    { username :: !String
    , password :: !String
    }


instance FromJSON MySQLUser where
    parseJSON = parseData >=> withObject "MySQLUser" (\v -> MySQLUser
        <$> v .: "slick_db_user"
        <*> v .: "slick_db_password")


data SSLConfig = SSLConfig
    { clientCA :: !String
    , clientCertificate :: !String
    , clientKey :: !String
    }


instance FromJSON SSLConfig where
    parseJSON = parseData >=> withObject "SSLConfig" (\v -> SSLConfig
        <$> v .: "server-ca"
        <*> v .: "client-cert"
        <*> v .: "client-key")


parseData :: Value -> Parser Value
parseData = withObject "VaultData" (.: "data")

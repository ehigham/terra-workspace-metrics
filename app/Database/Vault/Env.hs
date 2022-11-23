module Database.Vault.Env
    ( Env(..)
    , MalformedVaultAddressException(..)
    , newEnv
    ) where


import Control.Exception                        (Exception)
import Control.Monad.Catch                      (throwM)
import Control.Monad.IO.Class                   (MonadIO, liftIO)
import Data.ByteString.UTF8                     (ByteString, fromString)
import Data.Text                                (Text, pack)
import Network.HTTP.Conduit
    ( Manager
    , newManager
    , tlsManagerSettings
    )
import System.Environment                       (getEnv)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.ParserCombinators.ReadP             (char, munch, string)
import Text.Read                                (readMaybe, readPrec)


-- | Environment containing configuration for communicating with a vault server
data Env = Env { envManager :: !Manager
               , envVaultHost :: !ByteString
               , envVaultPort :: !Int
               , envVaultToken :: !ByteString
               }


instance Show Env where
    show = ("Env " ++) . show . envVaultHost


newEnv :: MonadIO m => m Env
newEnv = liftIO $ do
    m <- newManager tlsManagerSettings
    (host, port) <- vaultAddr >>= parseVaultPort
    Env m host port <$> vaultToken
  where
    vaultAddr = getEnv "VAULT_ADDR"
    vaultToken = fromString <$> (getEnv "HOME" >>= readFile . (++ "/.vault-token"))
    parseVaultPort addr
        | Just (Host "https" host port) <- readMaybe addr = pure (fromString host, port)
        | otherwise = throwM $ MalformedVaultAddressException (pack addr)


newtype MalformedVaultAddressException =
    MalformedVaultAddressException { address :: Text }
    deriving stock (Show, Read, Eq, Ord)


instance Exception MalformedVaultAddressException


-- | Helper for parsing hostname and port from VAULT_ADDR
--
data Host = Host !String !String !Int


instance Read Host where
    readPrec = do
        protocol <- ReadPrec.lift $
            munch (/=':') <*
            string "://"
        hostname <- ReadPrec.lift $
            munch (/=':') <*
            char ':'
        Host protocol hostname <$> readPrec

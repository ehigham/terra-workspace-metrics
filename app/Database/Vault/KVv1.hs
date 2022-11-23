module Database.Vault.KVv1 (readSecret) where


import Control.Monad.Reader                     (ask)
import Data.Aeson                               (FromJSON)
import Data.Function                            ((&))
import qualified Data.Text as T
import Data.Text.Encoding                       (encodeUtf8)
import qualified Network.HTTP.Simple as Client
import Database.Vault.Class                     (MonadVault, liftVault)
import Database.Vault.Env                       (Env(..))
import Database.Vault.Types                     (SecretPath(SecretPath))


readSecret :: (FromJSON a, MonadVault m) => SecretPath -> m a
readSecret (SecretPath path) = liftVault $ do
    env <- ask
    response <- Client.httpJSON (mkRequest env)
    return $ Client.getResponseBody response
  where
    mkRequest Env{..} = Client.defaultRequest
        & Client.setRequestManager envManager
        & Client.setRequestHost envVaultHost
        & Client.setRequestPort envVaultPort
        & Client.setRequestPath (encodeUtf8 $ T.intercalate "/" ["", "v1", path])
        & Client.addRequestHeader "X-Vault-Token" envVaultToken
        & Client.setRequestSecure True

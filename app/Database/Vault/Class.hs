module Database.Vault.Class
    ( Vault (Vault, unVault)
    , MonadVault
    , liftVault
    , runVault
    ) where


import Control.Applicative              (Alternative)
import Control.Monad.Catch              (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.IO.Unlift          (MonadUnliftIO, withRunInIO)
import Control.Monad.Reader             (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Resource     (MonadResource, ResourceT, liftResourceT)
import Control.Monad                    (MonadPlus)
import Database.Vault.Env               (Env)


-- | The vault monad containing configuration for communicating with a vault
-- server and resource allocation via 'ResourceT'.
newtype Vault a = Vault { unVault :: ReaderT Env (ResourceT IO) a }
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadReader Env
        , MonadResource
        )


runVault :: MonadResource m => Env -> Vault a -> m a
runVault e m = liftResourceT $ runReaderT (unVault m) e


class (MonadIO m, MonadThrow m) => MonadVault m where
    liftVault :: Vault a -> m a


instance MonadVault Vault where
    liftVault = id


instance MonadUnliftIO Vault where
    withRunInIO inner =
        Vault $ withRunInIO $ \run ->
            inner (run . unVault)

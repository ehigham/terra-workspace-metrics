module Database.Vault.Types
    ( SecretPath(..)
    ) where

import Data.String                      (IsString)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)


newtype SecretPath = SecretPath { path :: Text }
    deriving stock   (Generic)
    deriving newtype (Show, Read, IsString, Eq, Ord)

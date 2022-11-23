module Terra
    ( Environment(..)
    , WorkspaceName(..)
    ) where

import Data.Text                 (Text)
import GHC.Generics              (Generic)
import GHC.Read                  (Read(readPrec), choose)


data Environment = Production
                 | Staging
                 | Alpha
                 | Develop
    deriving stock (Eq, Ord, Generic)


instance Show Environment where
    show Production = "prod"
    show Staging = "staging"
    show Alpha = "alpha"
    show Develop = "dev"


instance Read Environment where
    readPrec = choose
        [ ("prod", pure Production)
        , ("staging", pure Staging)
        , ("alpha", pure Alpha)
        , ("dev", pure Develop)
        ]


data WorkspaceName = WorkspaceName
    { namespace :: !Text
    , name :: !Text
    }
    deriving stock (Eq, Ord, Show, Read, Generic)

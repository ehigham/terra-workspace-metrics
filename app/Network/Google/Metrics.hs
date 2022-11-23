module Network.Google.Metrics
    ( getBucketMetrics
    , BucketMetrics(..)
    , GoogleProjectId
    , BucketName
    ) where

import Control.Lens                 (Lens', (&), (?~), set, view)
import Control.Monad                ((>=>))
import Control.Monad.IO.Class
import Data.Coerce
import Data.Functor.Of
import Data.Int
import Data.Maybe
import Data.String                  (IsString)
import Data.Text                    (Text)
import Data.Time
import Data.String.Interpolate      (i, iii)
import Database.MySQL.Simple.Result (Result)
import GHC.Generics
import Network.Google               hiding (Stream)
import Network.Google.Monitoring
import Streaming                    (lift)
import Streaming.Prelude            (Stream)

import qualified Streaming.Prelude as S


getBucketMetrics :: (HasScope s ProjectsTimeSeriesList, MonadGoogle s m)
    => GoogleProjectId
    -> BucketName
    -> m BucketMetrics
getBucketMetrics projectId bucketName =
    BucketMetrics <$> objectCount <*> totalBytes
  where
    objectCount =
        sumFirstTimeSeriesPoints projectId bucketName objectCountMetricType

    totalBytes =
        sumFirstTimeSeriesPoints projectId bucketName totalBytesMetricType

    objectCountMetricType = MetricType
        "storage.googleapis.com/storage/object_count"
        900
        tvInt64Value

    totalBytesMetricType = MetricType
        "storage.googleapis.com/storage/total_bytes"
        900
        tvDoubleValue


sumFirstTimeSeriesPoints ::
    ( Num a
    , HasScope s ProjectsTimeSeriesList
    , MonadGoogle s m
    )
    => GoogleProjectId
    -> BucketName
    -> MetricType a
    -> m a
sumFirstTimeSeriesPoints projectId bucketName MetricType{..} =
    S.sum_ $ S.map
        (fromMaybe 0 . (firstPoint >=> view pValue >=> view lvalue))
        streamTimeSeries
  where
    firstPoint = listToMaybe . view tsPoints

    streamTimeSeries = do
        endTime <- liftIO getCurrentTime
        let startTime = addSeconds (negate sampleRate) endTime
        stream $ projectsTimeSeriesList [i|projects/#{coerce projectId :: Text}|]
               & ptslIntervalStartTime ?~ startTime
               & ptslIntervalEndTime ?~ endTime
               & ptslFilter ?~ [iii|
                    metric.type="#{metricName}"
                    AND resource.labels.bucket_name="#{coerce bucketName :: Text}"
                |]

    addSeconds s time = addUTCTime (fromIntegral s) time


stream :: (HasScope s ProjectsTimeSeriesList, MonadGoogle s m)
    => ProjectsTimeSeriesList
    -> Stream (Of TimeSeries) m ()
stream request = do
    response <- lift $ send request
    S.each (view ltsrTimeSeries response) <>
        maybe
            mempty
            (\token -> stream $ set ptslPageToken (Just token) request)
            (view ltsrNextPageToken response)


data BucketMetrics = BucketMetrics
    { objectCount :: !Int64
    , totalBytes :: !Double
    }
    deriving stock (Eq, Ord, Show, Read)


data MetricType a = MetricType
    { metricName :: !Text
    , sampleRate :: !Seconds
    , lvalue :: !(Lens' TypedValue (Maybe a))
    }


-- | Id of a Google Cloud Project
newtype GoogleProjectId = GoogleProjectId Text
    deriving stock (Generic)
    deriving newtype
        ( Eq
        , Ord
        , Show
        , Read
        , IsString
        , Result
        )

-- | Name of a Google Cloud Storage Bucket
newtype BucketName = BucketName Text
    deriving stock (Generic)
    deriving newtype
        ( Eq
        , Ord
        , Show
        , Read
        , IsString
        , Result
        )

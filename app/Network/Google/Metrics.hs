module Network.Google.Metrics
    ( getBucketMetrics
    , BucketMetrics(..)
    , ProjectId
    , BucketName
    ) where


import Control.Lens                 (Lens', (&), (?~), set, view)
import Control.Monad                ((>=>))
import Control.Monad.IO.Class
import Data.Aeson                   (ToJSON, FromJSON)
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
    => ProjectId
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
    , MonadGoogle s m
    , HasScope s ProjectsTimeSeriesList
    )
    => ProjectId
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
        -- An interesting "feature" of google monitoring requests - the
        -- timeSeries startTime defaults to the endTime so we need to request
        -- the interval (now - sampleTime, now] explicitly to guarantee at
        -- least one sample (if these metrics are enabled, of course).
        let startTime = addSeconds (negate sampleRate) endTime
        stream $ projectsTimeSeriesList [i|projects/#{coerce projectId :: Text}|]
               & ptslIntervalStartTime ?~ startTime
               & ptslIntervalEndTime ?~ endTime
               -- filtering by the workspace bucket name explude sother buckets
               -- in the google project like the stoage logs bucket or other
               -- leonardo-related buckets.
               & ptslFilter ?~ [iii|
                    metric.type="#{metricName}"
                    AND resource.labels.bucket_name="#{coerce bucketName :: Text}"
                |]

    addSeconds s time = addUTCTime (fromIntegral s) time

-- Google paginates their responses like sensible peopple - hide fetching the
-- next page behind the `Stream` interface.
stream :: (MonadGoogle s m, HasScope s ProjectsTimeSeriesList)
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

-- | The timeSeries we support (like total_butes or object_count) can be
-- aggregated according to storage class. Perhaps this is a common trait between
-- them - we're only interested in the latest data point for each storage class
-- and we want to aggregate these. It'll br noce to understand more use cases
-- and remove this record.
data MetricType a = MetricType
    { metricName :: !Text
    , sampleRate :: !Seconds
    , lvalue :: !(Lens' TypedValue (Maybe a))
    }


-- | Id of a Google Cloud Project
newtype ProjectId = ProjectId Text
    deriving stock (Generic)
    deriving newtype
        ( Eq
        , Ord
        , Show
        , Read
        , IsString
        , Result
        , ToJSON
        , FromJSON
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
        , ToJSON
        , FromJSON
        )

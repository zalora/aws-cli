{-# LANGUAGE RecordWildCards #-}

import qualified Network.AWS as A
import qualified Network.AWS.CloudWatch as C
import qualified Options.Applicative as O

import Control.Lens (set)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import Options.Applicative ((<*>), (<$>))
import System.Exit (exitFailure)

import Common


data PutMetric = PutMetric
    { pmRegion :: A.Region
    , pmNamespace :: Text
    , pmMetricName :: Text
    , pmValue :: Double
    , pmDimensions :: [C.Dimension]
    , pmUnit :: Maybe C.StandardUnit
    } deriving (Show)


putMetricData :: PutMetric -> C.PutMetricData
putMetricData pm@PutMetric{..} = set C.pmdMetricData [metricData] $
                                 C.putMetricData pmNamespace
    where metricData = set C.mdDimensions pmDimensions $
                       set C.mdUnit pmUnit $
                       set C.mdValue (Just pmValue) $
                       C.metricDatum pmMetricName


putMetric :: PutMetric -> IO (Either (A.ServiceError A.RESTError) C.PutMetricDataResponse)
putMetric pm@PutMetric{..} = do
    env <- A.getEnv pmRegion A.Discover
    runResourceT $ A.send env $ putMetricData pm


putMetricParser :: O.Parser PutMetric
putMetricParser = PutMetric
    <$> O.option O.auto (makeOption "region")
    <*> O.option text (makeOption "namespace")
    <*> O.option text (makeOption "metricName")
    <*> O.option O.auto (makeOption "value")
    <*> O.many (O.option O.auto (makeOption "dimension"))
    <*> O.optional (O.option O.auto (makeOption "unit"))


main :: IO ()
main = do
    pm <- O.execParser opts
    result <- putMetric pm
    case result of
        Right _ -> return ()
        Left error -> do
            print error
            exitFailure
    where opts = O.info (O.helper <*> putMetricParser)
                        (O.header "Put a metric to AWS CloudWatch")

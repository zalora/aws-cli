{-# LANGUAGE RecordWildCards
           , StandaloneDeriving #-}

import qualified Network.AWS as A
import qualified Network.AWS.CloudWatch as C
import qualified Options.Applicative as O

import Control.Lens (set)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text, split, pack)
import Options.Applicative ((<*>), (<$>))
import System.Exit (exitFailure)

import Common


data CreateAlarm = CreateAlarm
    { caAlarmName :: Text
    , caMetricName :: Text
    , caRegion :: A.Region
    , caNamespace :: Text
    , caStatistic :: C.Statistic
    , caPeriod :: Integer
    , caEvaluationPeriods :: Integer
    , caThreshold :: Double
    , caComparisonOperator :: C.ComparisonOperator
    , caTopicArn :: Text
    , caDimensions :: [C.Dimension]
    , caUnit :: Maybe C.StandardUnit
    } deriving (Show)


createAlarm :: CreateAlarm -> IO (Either (A.ServiceError A.RESTError) C.PutMetricAlarmResponse)
createAlarm ca@CreateAlarm{..} = do
    env <- A.getEnv caRegion A.Discover
    runResourceT $ A.send env pma
    where pma = set C.pmaAlarmActions [caTopicArn] $
                set C.pmaDimensions caDimensions $
                set C.pmaInsufficientDataActions [caTopicArn] $
                set C.pmaOKActions [caTopicArn] $
                set C.pmaUnit caUnit $
                C.putMetricAlarm caAlarmName
                                 caMetricName
                                 caNamespace
                                 caStatistic
                                 (fromInteger caPeriod)
                                 (fromInteger caEvaluationPeriods)
                                 caThreshold
                                 caComparisonOperator


toDimesion :: String -> Either String C.Dimension
toDimesion v = case split (== '=') $ pack v of
    [x, y] -> Right $ C.dimension x y
    _      -> Left $ "cannot parse value `" ++ v ++ "'"

dimension :: O.ReadM C.Dimension
dimension = O.eitherReader toDimesion


createAlarmParser :: O.Parser CreateAlarm
createAlarmParser = CreateAlarm
    <$> O.option text (makeOption "alarmName")
    <*> O.option text (makeOption "metricName")
    -- TODO: Region's Read doesn't like "ap-southeast-1"
    <*> O.option O.auto (makeOption "region")
    <*> O.option text (makeOption "namespace")
    <*> O.option O.auto (makeOption "statistic")
    <*> O.option O.auto (makeOption "period")
    <*> O.option O.auto (makeOption "evaluationPeriods")
    <*> O.option O.auto (makeOption "threshold")
    <*> O.option O.auto (makeOption "comparisonOperator")
    <*> O.option text (makeOption "topicArn")
    <*> O.many (O.option dimension (makeOption "dimension"))
    <*> O.optional (O.option O.auto (makeOption "unit"))


main :: IO ()
main = do
    ca <- O.execParser opts
    result <- createAlarm ca
    case result of
        Right _ -> return ()
        Left error -> do
            print error
            exitFailure
    where opts = O.info (O.helper <*> createAlarmParser)
                        (O.header "Create an AWS CloudWatch alarm")

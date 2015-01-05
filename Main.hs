{-# LANGUAGE RecordWildCards
           , StandaloneDeriving #-}

module Main where

import qualified Network.AWS as A
import qualified Network.AWS.CloudWatch as C
import qualified Options.Applicative as O

import Control.Lens (set)
import Control.Monad (liftM)
import Control.Monad.Trans.AWS (Error)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text, pack, split)
import Options.Applicative ((<>), (<*>), (<$>))
import System.Exit (exitFailure)


deriving instance Read C.ComparisonOperator
deriving instance Read C.Statistic
deriving instance Read C.StandardUnit

instance Read C.Dimension where
    readsPrec _ v = case split (== '=') $ pack v of
        [x, y] -> [(C.dimension x y, "")]
        _ -> []


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


text :: O.ReadM Text
text = liftM pack O.str


makeOption :: String -> O.Mod O.OptionFields a
makeOption name = O.long name <> O.metavar ("<" ++ name ++ ">")


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
    <*> O.many (O.option O.auto (makeOption "dimension"))
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

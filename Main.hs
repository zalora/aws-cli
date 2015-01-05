{-# LANGUAGE RecordWildCards
           , StandaloneDeriving #-}

module Main where

import qualified Network.AWS as A
import qualified Network.AWS.CloudWatch as C
import qualified Options.Applicative as O

import Control.Monad (liftM)
import Control.Monad.Trans.AWS (Error)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text, pack, split)
import Options.Applicative ((<>), (<*>), (<$>))


deriving instance Read C.ComparisonOperator
deriving instance Read C.Statistic
deriving instance Read C.StandardUnit

instance Read C.Dimension where
    readsPrec _ v = case split (== '=') $ pack v of
        [x, y] -> [(C.dimension x y, "")]
        _ -> []


data CreateAlarm = CreateAlarm
    { caComparisonOperator :: C.ComparisonOperator
    , caDimensions :: [C.Dimension]
    , caEvaluationPeriods :: Integer
    , caMetricName :: Text
    , caAlarmName :: Text
    , caNamespace :: Text
    , caPeriod :: Integer
    , caRegion :: A.Region
    , caStatistic :: C.Statistic
    , caThreshold :: Double
    , caUnit :: Maybe C.StandardUnit
    , caTopicArn :: Text
    } deriving (Show)


text :: O.ReadM Text
text = liftM pack O.str


makeOption :: String -> O.Mod O.OptionFields a
makeOption name = O.long name <> O.metavar ("<" ++ name ++ ">")


createAlarm :: CreateAlarm -> IO (Either Error ())
createAlarm ca@CreateAlarm{..} = do
    env <- A.getEnv caRegion A.Discover
    result <- runResourceT $ A.send env pma
    print result
    return $ Right ()
    where pma = C.putMetricAlarm caAlarmName
                                 caMetricName
                                 caNamespace
                                 caStatistic
                                 (fromInteger caPeriod)
                                 (fromInteger caEvaluationPeriods)
                                 caThreshold
                                 caComparisonOperator


createAlarmParser :: O.Parser CreateAlarm
createAlarmParser = CreateAlarm
    <$> O.option O.auto (makeOption "comparisonOperator")
    <*> O.many (O.option O.auto (makeOption "dimension"))
    <*> O.option O.auto (makeOption "evaluationPeriods")
    <*> O.option text (makeOption "metricName")
    <*> O.option text (makeOption "alarmName")
    <*> O.option text (makeOption "namespace")
    <*> O.option O.auto (makeOption "period")
    -- TODO: Region's Read doesn't like "ap-southeast-1"
    <*> O.option O.auto (makeOption "region")
    <*> O.option O.auto (makeOption "statistic")
    <*> O.option O.auto (makeOption "threshold")
    <*> O.optional (O.option O.auto (makeOption "unit"))
    <*> O.option text (makeOption "topicArn")


main :: IO (Either Error ())
main = O.execParser opts >>= createAlarm
    where
        opts = O.info
            (O.helper <*> createAlarmParser)
            (O.header "Create an AWS CloudWatch alarm")

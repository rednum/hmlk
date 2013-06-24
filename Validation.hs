{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Validation where

import Classifiers (trainClassifier, Classifier, Trained)
import Control.Lens
import Control.Monad
import Control.Monad.Random hiding (fromList, split)
import Data.Either (partitionEithers)
import Data.List (partition)
import DataSet
import DataSetRaw (fromAttribute, Decision, Label)


type Metric d = Decision d => [d] -> [d] -> Double


-- VERIFICATION

percentageSplit :: Double -> DataSet -> Rand StdGen ([Row], [Row])
percentageSplit ratio ds = do 
    sp <- forM (ds ^. rows) (\d -> do
        x::Double <- getRandom
        return $ if x > ratio
          then Left d
          else Right d
        )
    return $ partitionEithers sp
 

-- splits DataSet into two disjoint DataSets
split :: DataSet -> Rand StdGen (DataSet, DataSet)
split ds = do
    rs <- getRandomRs (0::Int, 1::Int)
    let
      remakeSet x = ds & rows .~ (snd $ unzip x)
      (a, b) = partition (\x -> fst x == 0) $ zip rs (ds ^. rows)
    return (remakeSet a, remakeSet b)


-- splits DataSet into two disjoint, not empty DataSets
splitNe :: DataSet -> Rand StdGen (DataSet, DataSet)
splitNe ds = if (length $ ds ^. rows) < 2
  then error "Set too small"
  else do
    (dsa, dsb) <- split ds
    if any (\x -> null $ x ^. rows) [dsa, dsb]
      then splitNe ds
      else return (dsa, dsb)



crossValidate :: (Decision d) => DataSet -> Label -> Metric d -> Classifier d -> IO Double
crossValidate ds dname me cl = do
    (dsa, dsb) <- evalRandIO $ splitNe ds
    let
      dsa' = map fromAttribute $ dsa ^.. rows . traversed . attr dname
      dsb' = dsb `dropCols` (== dname)
    tcl <- trainClassifier cl dsa dname
    return $ me dsa' (tcl dsb')


{-
-- METRICS

type DecisionPredicate = Attribute -> Attribute -> Bool
type DecisionCounter = DataSet -> DataSet -> Int
data Result = Positive | Negative deriving (Eq, Show)

countSatisfied :: DataSet -> DataSet -> DecisionPredicate -> Int
countSatisfied dsa dsb f = length $ filter (\(x, y) -> f x y) $ zippedDecisions dsa dsb


-- E.g. countBR True Positive == countTruePositive
countBR :: Bool -> Result -> DecisionCounter
countBR b r ex re = countSatisfied ex re f where
    f x y = (fromBoolean y) == rb && (x == y) == b
    rb = r == Positive


countBR2 :: Bool -> Result -> Bool -> Result -> DecisionCounter
countBR2 b1 r1 b2 r2 ex re = countBR b1 r1 ex re + countBR b2 r2 ex re


countHit :: DecisionCounter
countHit = countBR2 True Positive True Negative


countMiss :: DecisionCounter
countMiss = countBR2 False Positive False Negative


countPositive :: DecisionCounter
countPositive = countBR2 True Positive False Positive


countNegative :: DecisionCounter
countNegative = countBR2 True Negative False Negative


fromBoolean :: Attribute -> Bool
fromBoolean (Boolean x) = x
fromBoolean _ = error "Not boolean attribute"


fromNumeric :: Attribute -> Double
fromNumeric (Numeric x) = x
fromNumeric _ = error "Not numeric attribute"


zippedDecisions :: DataSet -> DataSet -> [(Attribute, Attribute)]
zippedDecisions dsa dsb = zip (attrs dsa) (attrs dsb) where
    attrs ds = concat $ ds ^.. rows . traversed . attributes


-- Simple hit-rate metric.
recall :: Metric
recall ex re = hits / (hits + misses) where
    hits = fromIntegral $ countHit ex re
    misses = fromIntegral $ countMiss ex re


mse :: Metric
mse ex re = sum / (fromIntegral $ length dec) where
  sum = sum $ map (\(x, y) -> (fromNumeric x - fromNumeric y)^2) dec
  dec = zippedDecisions ex re

-}

{-# LANGUAGE FlexibleContexts #-}
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



crossValidate :: (Classifier c, Decision d) => DataSet -> Label -> Metric d -> c d -> IO Double
crossValidate ds dname me cl = do
    (dsa, dsb) <- evalRandIO $ splitNe ds
    let
      dsa' = map fromAttribute $ dsa ^.. rows . traversed . attr dname
      dsb' = dsb `dropCols` (== dname)
    tcl <- trainClassifier cl dsa dname
    return $ me dsa' (tcl dsb')



-- METRICS

type DecisionPredicate d = Decision d =>  d -> d -> Bool
type DecisionCounter d = Decision d => [d] -> [d] -> Int
data Result = Positive | Negative deriving (Eq, Show)

countSatisfied :: DecisionPredicate d -> DecisionCounter d
countSatisfied f da db = length . filter (uncurry f) $ zip da db


-- E.g. confusionMatrix True Positive ~= countTruePositive
confusionMatrix :: Bool -> Result -> DecisionCounter Bool
confusionMatrix b r ex re = countSatisfied f ex re where
    f x y = y == rb && (x == y) == b
    rb = r == Positive 


confusionMatrixS :: [(Bool, Result)] -> DecisionCounter Bool
confusionMatrixS s da db =  sum . map (\x -> x da db) $ map (uncurry confusionMatrix) s


countTrue :: Eq d => DecisionCounter d
countTrue = countSatisfied (==)


countFalse :: Eq d => DecisionCounter d
countFalse = countSatisfied (/=)


countPositive :: DecisionCounter Bool
countPositive = confusionMatrixS [(True, Positive), (False, Positive)]
  

countNegative :: DecisionCounter Bool
countNegative = confusionMatrixS [(True, Negative), (False, Negative)]


-- Simple hit-rate metric.
recall :: Eq d => Metric d
recall ex re = hits / (hits + misses) where
    hits = fromIntegral $ countTrue ex re
    misses = fromIntegral $ countFalse ex re


mse :: Metric Double
mse ex re = s / (fromIntegral $ length ex) where
  s = sum [ (x - y)^2 | (x, y) <- zip ex re]

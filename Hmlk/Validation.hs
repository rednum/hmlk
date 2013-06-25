{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hmlk.Validation where

import Control.Lens
import Control.Monad
import Control.Monad.Random hiding (fromList, split)
import Data.Either (partitionEithers)
import Data.List (partition)
import Hmlk.Classifiers (trainClassifier, Classifier, Trained, fromAttribute, Decision, Label)
import Hmlk.DataSet


type Metric d = Decision d => [d] -> [d] -> Double



percentageSplit :: Double -> DataSet -> Rand StdGen (DataSet, DataSet)
percentageSplit ratio ds = do 
    sp <- forM (ds ^. rows) (\d -> do
        x::Double <- getRandom
        return $ if x > ratio
          then Left d
          else Right d
        )
    let 
      (l, r) = partitionEithers sp
    
    if length l == 0 || length r == 0
      then percentageSplit ratio ds
      else return $ (buildRows l, buildRows r)
 

crossValidate1 :: (Classifier c, Decision d) => Label -> Metric d -> DataSet -> Double -> c d -> IO Double
crossValidate1 dname me ds percents cl = do
    (dsa, dsb) <- evalRandIO $ percentageSplit percents ds
    let
      dsa' = map fromAttribute $ dsa ^.. rows . traversed . attr dname
      dsb' = dsb `dropCols` (== dname)
    tcl <- trainClassifier cl dsa dname
    return $ me dsa' (tcl dsb')

crossValidate :: (Classifier c, Decision d) => Label -> Metric d -> DataSet -> Double -> Int -> c d -> IO [Double]
crossValidate dname me ds percents folds cl = do
    forM [1..folds] (\_ -> crossValidate1 dname me ds percents cl)

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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Classifiers where

import Control.Lens
import Control.Monad
import Control.Monad.Random hiding (fromList, split)
import Data.Function (on)
import Data.IntMap (fromList)
import Data.Ord
import DataSet
import DataSetRaw
import Data.List (sort, sortBy, group, maximumBy)
import Data.Array.IArray (amap, elems, listArray)
import Control.Monad.Reader (asks)

type Trained d = DataSet -> [d]
type Vote a d = (Decision d) => [a] -> d
type Distance d = [d] -> [d] -> Double

--type Classifier d = CM d (Trained d)
class Classifier a where
    trainClassifier :: Decision d => a d -> DataSet -> Label -> IO (Trained d)


newtype UnlabeledClassifier d = UnlabeledClassifier { getUnlabeledClassifier :: CM (Storage d) (Trained d) }
instance Classifier UnlabeledClassifier where
    trainClassifier (UnlabeledClassifier c) ds l = evalCM c (makeRawDataSet ds l)

newtype LabeledClassifier d = LabeledClassifier { getLabeledClassifier :: CM DataSet (Trained d) }
instance Classifier LabeledClassifier where
    trainClassifier (LabeledClassifier c) ds l = evalCM c ds -- TODO : use label


-- Generic lazy KNN, using given vote and distance functions.
lazyKNNG :: (Ord d, Show d, Decision d) =>
    Vote (Double, d) d -> Distance Double -> Int -> UnlabeledClassifier d
lazyKNNG v d k = UnlabeledClassifier $ do
  nums <- liftM elems . asks $ amap numerics
  decisions::[d] <- liftM elems . asks $ amap decision
  let 
    train' :: [([Double], d)]
    train' = zip nums decisions
    predict :: DataSet -> [d] -- czyli Trained d
    predict test = map bestFit (numericsOf test)
    bestFit x = v . take k $ sort [ (d x a, b) | (a, b) <- train']
  return predict


lazyKNN :: (Ord d, Show d, Decision d) => Int -> UnlabeledClassifier d
lazyKNN = lazyKNNG (majority . map snd) (pnormDist 2.0)


exampleKNN :: (Ord d, Show d, Decision d) => Int -> UnlabeledClassifier d
exampleKNN = lazyKNNG weightedMajority (pnormDist 5.0)

euclidean :: Distance Double
euclidean = pnormDist 2.0

pnormDist :: Double -> Distance Double
pnormDist p x y = let
    s = sum [(a - b)**p | (a, b) <- zip x y]
  in s**(1.0/p)


majority :: (Ord d) => Vote d d
majority = head . maximumBy (comparing length) . group . sort


weightedMajority :: (Ord d) => Vote (Double, d) d
weightedMajority x = let
    grouped = group $ sortBy (comparing snd) x
    reduce l = (sum $ map ((\(x, _) -> 1 / x)) l, snd $ head l)
  in snd . maximumBy (comparing fst) $ map reduce grouped


simpleVote :: Decision d => [Trained d] -> Trained d
simpleVote l = do
  return undefined


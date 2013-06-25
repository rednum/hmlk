{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Hmlk.Classifiers.Lazy (genericKNN, simpleKNN, majority, weightedMajority, euclidean, pnormDist) where

import Control.Monad.Reader (asks)
import Control.Monad (liftM)
import Data.List (sortBy, maximumBy, group, sort)
import Data.Ord
import Data.Array.IArray

import Hmlk.DataSet
import Hmlk.Classifiers

-- Generic lazy KNN, using given vote and distance functions.
genericKNN :: (Ord d, Show d, Decision d) =>
    Vote (Double, d) d -> Distance Double -> Int -> UnlabeledClassifier d
genericKNN v d k = UnlabeledClassifier $ do
  nums <- liftM elems . asks $ amap numerics
  decisions::[d] <- liftM elems . asks $ amap decision
  let 
    train' :: [([Double], d)]
    train' = zip nums decisions
    predict :: DataSet -> [d] 
    predict test = map bestFit (numericsOf test)
    bestFit x = v . take k $ sort [ (d x a, b) | (a, b) <- train']
  return predict


simpleKNN :: (Ord d, Show d, Decision d) => Int -> UnlabeledClassifier d
simpleKNN = genericKNN (majority . map snd) (pnormDist 2.0)

majority :: (Ord d) => Vote d d
majority = head . maximumBy (comparing length) . group . sort

weightedMajority :: (Ord d) => Vote (Double, d) d
weightedMajority x = let
    grouped = group $ sortBy (comparing snd) x
    reduce l = (sum $ map ((\(x, _) -> 1 / x)) l, snd $ head l)
  in snd . maximumBy (comparing fst) $ map reduce grouped

euclidean :: Distance Double
euclidean = pnormDist 2.0

pnormDist :: Double -> Distance Double
pnormDist p x y = let
    s = sum [(a - b)**p | (a, b) <- zip x y]
  in s**(1.0/p)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Classifiers where

import Control.Lens
import Control.Monad.Random hiding (fromList, split)
import Data.MultiSet (insert, empty, findMax)
import Data.IntMap (fromList)
import DataSet
import DataSetRaw
import Data.List (sortBy)
import Data.Array.IArray (amap, elems)
import Control.Monad.Reader (asks)

type Label = String -- czyli nazwa kolumny z decyzja
type Classifier d = CM d (Trained d)
type Trained d = DataSet -> [d]


trainClassifier :: Decision d => Classifier d -> DataSet -> Label -> IO (Trained d)
trainClassifier c ds l = evalCM c ds' -- wrzuc label i train do stanu
  where
    ds' = undefined

lazyKNN :: (Decision d) => Int -> Classifier d
lazyKNN k = do
  nums <- (asks $ amap numerics >>= return . elems)
  decisions::[d] <- (asks $ amap decision >>= return . elems)
  let 
    train' :: [([Double], d)]
    train' = zip nums decisions
    predict :: DataSet -> [d] -- czyli Trained d
    predict test = map bestFit (numericsOf test)
    bestFit x = majority . take k . sortBy (dist x) $ train'
    majority = undefined
    dist x (y, _) = undefined
  return predict


simpleVote :: Decision d => [Trained d] -> Trained d
simpleVote l = do
  return undefined


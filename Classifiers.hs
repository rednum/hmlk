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
type Classifier d = DataSet -> Label -> CM d (Trained d)
type Trained d = DataSet -> [d]


lazyKNN :: (Decision d) => Int -> Classifier d
lazyKNN k train label = do
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


simpleVote :: Decision d => [Classifier d] -> Classifier d
simpleVote l = do
  return undefined


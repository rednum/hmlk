{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Hmlk.Classifiers where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Random hiding (fromList, split)
import Data.Array.IArray
import Data.Function (on)
import Data.IntMap (fromList)
import Data.Ord
import Hmlk.DataSet
import Data.List (find, sort, sortBy, group, maximumBy, nub)
import Data.Array.IArray (amap, elems, listArray)
import Control.Monad.Reader (ask, asks)


type Trained d = DataSet -> [d]
type Vote a d = (Decision d) => [a] -> d
type Distance d = [d] -> [d] -> Double

--type Classifier d = CM d (Trained d)
class Classifier a where
    trainClassifier :: Decision d => a d -> DataSet -> Label -> IO (Trained d)


newtype UnlabeledClassifier d = UnlabeledClassifier { getUnlabeledClassifier :: CM (Storage d) (Trained d) }
instance Classifier UnlabeledClassifier where
    trainClassifier (UnlabeledClassifier c) ds l = evalCM c (makeRawDataSet ds l)

newtype LabeledClassifier d = LabeledClassifier { getLabeledClassifier :: CM (DataSet, Label) (Trained d) }
instance Classifier LabeledClassifier where
    trainClassifier (LabeledClassifier c) ds l = evalCM c (ds, l) -- TODO : use label


class Decision a where
    fromAttribute :: Attribute -> a

instance Decision Double where
    fromAttribute (Numeric n) = n

instance Decision Bool where
    fromAttribute (Numeric n) = n /= 0
    fromAttribute (Boolean b) = b

instance Decision String where
    fromAttribute (Nominal n) = n

data RawRow d = RawRow {numerics :: [Double], nominals :: [String], decision :: d} deriving (Show)
type Storage d = Array Int (RawRow d)
type CM s a = ReaderT s (Rand StdGen) a
type Label = String -- czyli nazwa kolumny z decyzja


evalCM :: CM s a -> s -> IO a
evalCM cm store = 
  evalRandIO $ runReaderT cm store


makeRawDataSet :: Decision d => DataSet -> Label -> Storage d
makeRawDataSet ds l = listArray (0, length decs - 1) $ zipWith3 makeRow (numericsOf ds') (nominalsOf ds') decs
  where
    decs = map fromAttribute $ ds ^.. rows . traverse . attr l
    ds' = ds & rows . traverse %~ rmAttr l
    makeRow nu no de = RawRow {numerics = nu, nominals = no, decision = de}

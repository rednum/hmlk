{-# LANGUAGE ScopedTypeVariables #-}

module Classifiers where

import Control.Lens
import Control.Monad
import Control.Monad.Random hiding (fromList, split)
import Data.List (partition)
import Data.MultiSet (insert, empty, findMax)
import Data.IntMap (fromList)
import DataSet

-- tak naprawde m bedzie po prostu monada random
-- byc moze inny generator, nie wiem w sumie jezcze
type Trained = DataSet -> Rand StdGen DataSet 
type Classifier = DataSet -> Trained 

-- tutaj jeszcze cos z tymi typami
-- jakie jesszcze klasyfikatory?
-- unsuperverised?
--


simpleVote :: [Classifier] -> Classifier
simpleVote l = do
  return undefined

-- fejkowy KNN
-- wybiera pare egzemplarzy ze zbioru treningowego i zapuszcza na nim 1-kNN
-- prawdopodobnie bardzo kiepski klasyfikator, ale ma za zadanie zaprezentowac monade random
fakeKNN :: Classifier
fakeKNN training test = undefined

-- zignoruj input i podaj dwie losowe liczby!
example :: Classifier
example training test = do
  x <- getRandomR (0, 10)
  y <- getRandomR (0, 10)
  return $ DataSet {_names' = ["decision"], 
                    _rows = fromList . zip [1..] $ [[Numeric x], [Numeric y]]} 


-- chyba zrobimy to inaczje ale zostawima stary kod
-- CLASSIFIERS
type ClassifierC = DataSet -> [Attribute]

majorityFactory :: String -> DataSet -> ClassifierC
majorityFactory an ts = \ds -> map (\row -> result) $ ds ^. rows where
    result = majority $ ts ^.. rows . traversed . attr an
    majority vals = findMax $ foldl (flip insert) empty vals


-- splits DataSet into two disjoint DataSets
split :: (RandomGen g) => DataSet -> Rand g (DataSet, DataSet)
split ds = do
    rs <- getRandomRs (0::Int, 1::Int)
    (a, b) <- return $ partition (\x -> fst x == 0) $ zip rs (ds ^. rows)
    return (remakeSet a, remakeSet b)
  where
    remakeSet x = ds & rows .~ (snd $ unzip x)


-- splits DataSet into two disjoint, not empty DataSets
splitNe :: (RandomGen g) => DataSet -> Rand g (DataSet, DataSet)
splitNe ds = if (length $ ds ^. rows) < 2
  then error "Set too small"
  else do
    (dsa, dsb) <- split ds
    if any (\x -> null $ x ^. rows) [dsa, dsb]
      then splitNe ds
      else return (dsa, dsb)



type Metric = [Attribute] -> [Attribute] -> Double

crossValidate :: DataSet -> Metric -> Classifier -> Double
crossValidate = undefined


stupidMetric :: Metric
stupidMetric ex re = mean where
    mean = (foldl (+) 0.0 diffs) / (fromIntegral $ length diffs) 
    diffs = [ (a - b) | (Numeric a, Numeric b) <- zip ex re]


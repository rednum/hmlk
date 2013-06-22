{-# LANGUAGE ScopedTypeVariables #-}

module Classifiers where

import Control.Lens
import Control.Monad.Random hiding (fromList, split)
import Data.MultiSet (insert, empty, findMax)
import Data.IntMap (fromList)
import DataSet

-- tak naprawde m bedzie po prostu monada random
-- byc moze inny generator, nie wiem w sumie jezcze
type Trained = DataSet -> Rand StdGen DataSet 
type Classifier = Label -> DataSet -> Trained 
type Label = String -- czyli nazwa kolumny z decyzja

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
fakeKNN dlabel training test = undefined

-- zignoruj input i podaj dwie losowe liczby!
example :: Classifier
example dname training test = do
  x <- getRandomR (0, 10)
  y <- getRandomR (0, 10)
  return $ DataSet {_names' = [dname], 
                    _rows = fromList . zip [1..] $ [[Numeric x], [Numeric y]]} 


-- chyba zrobimy to inaczje ale zostawima stary kod
-- CLASSIFIERS
type ClassifierC = DataSet -> [Attribute]

majorityFactory :: String -> DataSet -> ClassifierC
majorityFactory an ts = \ds -> map (\row -> result) $ ds ^. rows where
    result = majority $ ts ^.. rows . traversed . attr an
    majority vals = findMax $ foldl (flip insert) empty vals

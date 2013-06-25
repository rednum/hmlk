{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Hmlk.Classifiers.Clustering where

import Control.Lens
import Control.Monad (forM, liftM)
import Control.Monad.Reader (asks)
import Control.Monad.Random
import Data.Array.IArray (amap, elems, listArray, Array)
import Data.List (groupBy, sortBy, minimumBy)
import Data.Ord (comparing)
import Data.Function (on)

import Hmlk.Classifiers hiding (numerics, nominals, Storage)
import Hmlk.DataSet

kMeans = genericKMeans 100 1 euclidean

-- data HMatrixStore = HMatrixStore { numerics :: Matrix Double, decisions :: Vector Double }
newtype UnsupervisedClassifier d = UnsupervisedClassifier { getHMatrixClassifier :: CM Storage (Trained d) } 
type Storage = Array Int UnlabeledRow
data UnlabeledRow = UnlabeledRow {numerics :: [Double], nominals :: [String]} deriving (Show)
instance Classifier UnsupervisedClassifier where
    trainClassifier c ds _ = trainUnsupervised c ds

trainUnsupervised (UnsupervisedClassifier c) ds = evalCM c (makeUnsupervised ds)

makeUnsupervised :: DataSet -> Storage
makeUnsupervised ds = listArray (0, length (numericsOf ds) - 1) $ zipWith makeRow (numericsOf ds) (nominalsOf ds)
  where
    makeRow nu no = UnlabeledRow {numerics = nu, nominals = no}


--- TODO nowe train bez podawania decyzji!
genericKMeans :: Int -> Double -> ([Double] -> [Double] -> Double) -> Int -> UnsupervisedClassifier String
genericKMeans maxIter epsilon d k = UnsupervisedClassifier $ do
    train <- liftM elems . asks $ amap numerics
    initial <- forM [1..k] (\_ -> mapM getRandomR (limits train))
    let 
      loop i means 
        | i >= maxIter = means
        | epsilon >= (sum $ zipWith d (update means) means) = means
        | otherwise = loop (i + 1) (update means)
      update :: [[Double]] -> [[Double]]
      update means = --undefined
          -- wyglada na to, ze czasem jakis klaster znika - czy wobec tego chcemy dodawac zawsze tez i punkty z klastrow?
          map (recenter . map snd) . groups $ [(minimumIndexByKey (d t) means, t) | t <- train]
      predict means test = [show $ minimumIndexByKey (d t) means | t <- numericsOf test]
    return $ predict (loop 0 initial)


groups = groupBy ((==) `on` fst) . sortBy (comparing fst)

minimumIndexByKey f = fst . minimumBy (comparing (f . snd)) . zip [1..] 

recenter :: [[Double]] -> [Double]
recenter l = map (/ (fromIntegral $ length l)) (foldl1 (zipWith (+)) l)

limits :: [[Double]] -> [(Double, Double)]
limits (h:t) = foldl f [(x, x) | x <- h] t
    where
      f = zipWith (\(mx, mn) x -> (max mx x, min mn x))

euclidean :: Distance Double
euclidean = pnormDist 2.0

pnormDist :: Double -> Distance Double
pnormDist p x y = let
    s = sum [(a - b)**p | (a, b) <- zip x y]
  in s**(1.0/p)

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataSetRaw where

import Control.Monad.Reader
import Control.Monad.Random
import Data.Array.IArray
import Data.RVar
import DataSet

class Decision a where
    fromAttribute :: Attribute -> a

instance Decision Double where
    fromAttribute (Numeric n) = n

instance Decision Bool where
    fromAttribute (Numeric n) = n /= 0

instance Decision String where
    fromAttribute (Nominal n) = n

data RawRow d = RawRow {numerics :: [Double], nominals :: [String], decision :: d} deriving (Show)
type Storage d = Array Int (RawRow d)
type CM d a = Decision d => ReaderT (Storage d) (Rand StdGen) a


evalCM :: Decision d => CM d a -> Storage d -> IO a
evalCM cm store = 
  evalRandIO $ runReaderT cm store


ex1 :: CM Double [Double]
ex1 = do
  store::[Double] <- asks $ (amap (head . numerics)) >>= return . elems
  forM store (\x -> (liftM $ (trunc 2) . (+ x)) (getRandomR (-1, 1)))



trunc p f = let pp = 10 ** p in (fromIntegral $ truncate (f * pp)) / pp

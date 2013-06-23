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

class Decision a

instance Decision Double
instance Decision Bool
instance Decision String

data RawRow d = Decision d => Storage {numerics :: [Double], nominals :: [String], decision :: d}
type Storage d = Array Int (RawRow d)
type CM d a = Decision d => ReaderT (Storage d) (Rand StdGen) a


evalCM :: Decision d => CM d a -> Storage d -> IO a
evalCM cm store = 
  evalRandIO $ runReaderT cm store


ex1 :: CM Double [Double]
ex1 = do
  ss <- (asks $ amap (head . numerics) >>= return . elems)
  return ss
  --  liftM head $ forM (elems store) (\(x:_) -> (liftM $ (trunc 2) . (+ x)) (getRandomR (-1, 1)))




trunc p f = let pp = 10 ** p in (fromIntegral $ truncate (f * pp)) / pp

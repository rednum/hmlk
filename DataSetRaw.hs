{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataSetRaw where

import Control.Lens
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


ex1 :: CM (Storage Double) [Double]
ex1 = do
  store::[Double] <- asks $ (amap (head . numerics)) >>= return . elems
  forM store (\x -> (liftM $ (trunc 2) . (+ x)) (getRandomR (-1, 1)))



trunc p f = let pp = 10 ** p in (fromIntegral $ truncate (f * pp)) / pp

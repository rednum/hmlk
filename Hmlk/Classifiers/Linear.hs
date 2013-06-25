module Hmlk.Classifiers.Linear (ridge, linear) where

import Control.Monad.Reader
import Control.Lens

import Data.Packed.Matrix hiding (rows)
import Numeric.LinearAlgebra hiding (rows)
import Numeric.LinearAlgebra.Algorithms

import Hmlk.DataSet 
import Hmlk.Classifiers hiding (numerics)

data HMatrixStore = HMatrixStore { numerics :: Matrix Double, decisions :: Vector Double, label :: Label }
newtype HMatrixClassifier d = HMatrixClassifier { getHMatrixClassifier :: CM HMatrixStore (Trained d) } 

instance Classifier HMatrixClassifier where
    trainClassifier (HMatrixClassifier m) ds l = evalCM m (HMatrixStore {numerics = nums, decisions = decs, label = l})
      where 
          decs = fromList . map fromAttribute $ ds ^.. rows . traverse . attr l
          nums = fromLists . numericsOf $ removeLabel l ds 

ridge :: Double -> HMatrixClassifier Double
ridge gamma = HMatrixClassifier $ do
    a <- asks numerics
    b <- asks decisions
    l <- asks label
    let 
      n = cols a
      g = scale gamma (ident n)
      x = pinv ((trans a) <> a + (trans g) <> g) <> (trans a) <> b
      predict = map (dot x . fromList) . numericsOf . removeLabel l
    return predict

removeLabel l ds = ds & rows . traverse %~ rmAttr l

linear = ridge 0

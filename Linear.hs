module Linear where

import Control.Monad.Reader
import Data.Packed.Matrix hiding (rows)
import Numeric.LinearAlgebra hiding (rows)
import Numeric.LinearAlgebra.Algorithms
import DataSetRaw hiding (numerics)
import Classifiers
import Control.Lens
import DataSet 

linear = ridge 0
--type Classifier = Reader Storage 
a::Matrix Double
a = fromLists [[1,2], [2,1], [3,3]]
b::Vector Double
b = fromList [12, 21, 30]

data HMatrixStore = HMatrixStore { numerics :: Matrix Double, decisions :: Vector Double }
newtype HMatrixClassifier d = HMatrixClassifier { getHMatrixClassifier :: CM HMatrixStore (Trained d) } 
instance Classifier HMatrixClassifier where
    trainClassifier (HMatrixClassifier m) ds l = evalCM m (HMatrixStore {numerics = nums, decisions = decs})
      where 
          decs = fromList . map fromAttribute $ ds ^.. rows . traverse . attr l
          nums = fromLists $ numericsOf (ds & rows . traverse %~ rmAttr l)

ridge :: Matrix Double -> Vector Double -> Double -> HMatrixClassifier Double
ridge a b gamma = HMatrixClassifier $ do
    a <- asks numerics
    b <- asks decisions
    let 
      n = cols a
      g = scale gamma (ident n)
      x = pinv ((trans a) <> a + (trans g) <> g) <> (trans a) <> b
      predict = map (dot x . fromList) . numericsOf 
    return predict


-- normalizacja
-- costam costma

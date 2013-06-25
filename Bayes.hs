module Bayesian where

import Debug.Trace
import DataSetRaw
import Classifiers
import Control.Lens hiding (rmap)
import DataSet 
import qualified Data.MultiSet as MS
import qualified Data.Map as DM
import Data.Array.IArray
import Control.Monad.Reader

-- note: those will work only for discrete decision (ie. decision is not double)
-- mapa decyzja -> (nazwa atrybutu, wartosc) -> liczba; wystapienia decyzji; wystapienia atrybutow
type RawPriors d = (DM.Map d (MS.MultiSet (String, String)), MS.MultiSet d, MS.MultiSet (String, String))
-- naive bayes
naiveBayes :: (Show d, Ord d, Decision d) => LabeledClassifier d
naiveBayes = LabeledClassifier $ do
    label <- asks snd
    let 
      emptyPriors :: RawPriors d
      emptyPriors = (DM.empty, MS.empty, MS.empty)
      countPriors :: (Show d, Decision d, Ord d) => RawPriors d -> Row -> RawPriors d
      countPriors (occ, dCnt, aCnt) r = 
        let
          dec = fromAttribute $ r ^. attr label
          noms = namedNominals $ r & rmAttr label
          dCnt' = MS.insert dec dCnt
          aCnt' = foldl (flip MS.insert) aCnt noms
          occ' = foldl addOccurence occ noms 
          addOccurence occ (ix, val) = DM.insertWith MS.union dec (MS.singleton (ix, val)) occ
        in
          (occ', dCnt', aCnt')
      predict1 :: Show d => RawPriors d -> Row -> d
      predict1 priors _ = trace (show priors) undefined
      --predict1 priors r = undefined --rmap (\n a -> if isNominal then (  ) 0)
    priors <- liftM (foldl countPriors emptyPriors) $ asks ((^. rows) . fst)
    return $ map (predict1 priors) . (^. rows)
-- gaussian bayes
-- albo i nie

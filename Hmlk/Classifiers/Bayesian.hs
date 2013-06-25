module Hmlk.Classifiers.Bayesian where

import Control.Lens hiding (rmap)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Array.IArray
import qualified Data.MultiSet as MS
import Control.Monad.Reader

import Hmlk.DataSet 
import Hmlk.Classifiers

-- note: those will work only for discrete decision (ie. decision is not double)
-- mapa decyzja -> (nazwa atrybutu, wartosc) -> liczba; wystapienia decyzji; wystapienia atrybutow
type RawPriors d = (MS.MultiSet (d, (String, String)), MS.MultiSet d, MS.MultiSet (String, String))
-- naive bayes
naiveBayes :: (Show d, Ord d, Decision d) => LabeledClassifier d
naiveBayes = LabeledClassifier $ do
    label <- asks snd
    let 
      emptyPriors :: RawPriors d
      emptyPriors = (MS.empty, MS.empty, MS.empty)
      countPriors :: (Show d, Decision d, Ord d) => RawPriors d -> Row -> RawPriors d
      countPriors (occ, dCnt, aCnt) r = 
        let
          dec = fromAttribute $ r ^. attr label
          noms = namedNominals $ r & rmAttr label
          dCnt' = MS.insert dec dCnt
          aCnt' = foldl (flip MS.insert) aCnt noms
          occ' = foldl addOccurence occ noms 
          addOccurence occ (ix, val) = MS.insert (dec, (ix, val)) occ
        in
          (occ', dCnt', aCnt')
      predict1 :: (Ord d, Show d) => RawPriors d -> Row -> d
      predict1 priors@(_, dCnt, _) row = maximumBy (comparing $ calcProbability priors row) (MS.distinctElems dCnt)
      calcProbability :: (Ord d, Show d) => RawPriors d -> Row -> d -> Double
      calcProbability (occ, dCnt, aCnt) row dec = 
        let 
          row' = namedNominals $ row & rmAttr label
          numerator = MS.occur dec dCnt * product [MS.occur (dec, r) occ | r <- row']
          denominator = MS.size dCnt * (product . map (flip MS.occur aCnt) $ row')
        in fromIntegral numerator / fromIntegral denominator
    priors <- liftM (foldl countPriors emptyPriors) $ asks ((^. rows) . fst)
    return $ map (predict1 priors) . (^. rows)


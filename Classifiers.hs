{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Classifiers where

import Control.Lens
import Control.Monad
import Control.Monad.Random hiding (fromList, split)
import Data.Function (on)
import Data.IntMap (fromList)
import Data.Ord
import DataSet
import DataSetRaw
import Data.List (find, sort, sortBy, group, maximumBy, nub)
import Data.Array.IArray (amap, elems, listArray)
import Control.Monad.Reader (ask, asks)


type Trained d = DataSet -> [d]
type Vote a d = (Decision d) => [a] -> d
type Distance d = [d] -> [d] -> Double

--type Classifier d = CM d (Trained d)
class Classifier a where
    trainClassifier :: Decision d => a d -> DataSet -> Label -> IO (Trained d)


newtype UnlabeledClassifier d = UnlabeledClassifier { getUnlabeledClassifier :: CM (Storage d) (Trained d) }
instance Classifier UnlabeledClassifier where
    trainClassifier (UnlabeledClassifier c) ds l = evalCM c (makeRawDataSet ds l)

newtype LabeledClassifier d = LabeledClassifier { getLabeledClassifier :: CM (DataSet, Label) (Trained d) }
instance Classifier LabeledClassifier where
    trainClassifier (LabeledClassifier c) ds l = evalCM c (ds, l) -- TODO : use label


-- Generic lazy KNN, using given vote and distance functions.
lazyKNNG :: (Ord d, Show d, Decision d) =>
    Vote (Double, d) d -> Distance Double -> Int -> UnlabeledClassifier d
lazyKNNG v d k = UnlabeledClassifier $ do
  nums <- liftM elems . asks $ amap numerics
  decisions::[d] <- liftM elems . asks $ amap decision
  let 
    train' :: [([Double], d)]
    train' = zip nums decisions
    predict :: DataSet -> [d] -- czyli Trained d
    predict test = map bestFit (numericsOf test)
    bestFit x = v . take k $ sort [ (d x a, b) | (a, b) <- train']
  return predict


lazyKNN :: (Ord d, Show d, Decision d) => Int -> UnlabeledClassifier d
lazyKNN = lazyKNNG (majority . map snd) (pnormDist 2.0)


exampleKNN :: (Ord d, Show d, Decision d) => Int -> UnlabeledClassifier d
exampleKNN = lazyKNNG weightedMajority (pnormDist 5.0)

euclidean :: Distance Double
euclidean = pnormDist 2.0

pnormDist :: Double -> Distance Double
pnormDist p x y = let
    s = sum [(a - b)**p | (a, b) <- zip x y]
  in s**(1.0/p)


majority :: (Ord d) => Vote d d
majority = head . maximumBy (comparing length) . group . sort


weightedMajority :: (Ord d) => Vote (Double, d) d
weightedMajority x = let
    grouped = group $ sortBy (comparing snd) x
    reduce l = (sum $ map ((\(x, _) -> 1 / x)) l, snd $ head l)
  in snd . maximumBy (comparing fst) $ map reduce grouped


simpleVote :: Decision d => [Trained d] -> Trained d
simpleVote l = do
  return undefined




-- DECISION TREES

data DecisionTree d = Decision d => Leaf d | Node Label [(Attribute -> Bool, DecisionTree d)]

instance (Show d) => Show (DecisionTree d) where
  show (Leaf x) = "Leaf " ++ (show x)
  show (Node l s) = "Node " ++ l ++ " " ++ (show $ map snd s)



decisionTree :: (Ord d, Decision d) => LabeledClassifier d
decisionTree = LabeledClassifier $ do
  (ds, l) <- ask
  let
    tree = buildTree l ds
  return $ runTree tree
    

buildTree :: (Ord d, Decision d) => Label -> DataSet -> DecisionTree d
buildTree l ds = let
    empty = length ( _names' ds) == 1 -- only class attribute
    best = snd $ maximum [ (gainRatio l ds a, a) | a <- _names' ds, a /= l]
    classified = (==1) . length $ attrVals ds l -- TODO : missings?
    numericAttr = any isNumeric $ ds ^.. rows . traversed . attr best
    piece p = fullfilling ds best $ p
    nominalChild f = (f, buildTree l $ dropCols (piece f) (==best))
    numericChild f = (f, buildTree l $ piece f)
    bestCut = snd $ maximum [ (cutGain l ds best v, v) | v <- attrVals ds best ]
  in if empty
    then Leaf $ majority [ fromAttribute x | x <- (ds ^.. rows . traversed . attr l) ]
    else case (classified, numericAttr) of
      (True, _)  -> Leaf . fromAttribute . head $ ds ^.. rows . traversed . attr l
      (_, True)  -> Node best [ numericChild (<bestCut), numericChild (>=bestCut) ]
      (_, False) -> Node best [ nominalChild (==x) | x <- attrVals ds best ]


runTree :: (Ord d, Decision d) => DecisionTree d -> DataSet -> [d]
runTree t ds = map (predictRow t) $ ds ^. rows where
   predictRow (Leaf v) _ = v
   predictRow (Node l ch) r = majority [ predictRow t' r | (p, t') <- ch,
                                                           let v = r ^. attr l,
                                                           p v || v == Missing ]


gainRatio :: Label -> DataSet -> Label -> Double
gainRatio l ds a = gain l ds a / splitInformation ds a


splitInformation :: DataSet -> Label -> Double
splitInformation ds a = sum [ - p * (logBase 2.0 p)  | v <- attrVals ds a,
                                    let ds' = fullfilling ds a (==v)
                                        p = dlen ds' / dlen ds ]


cutGain :: Label -> DataSet -> Label -> Attribute -> Double
cutGain l ds a v = let
    subsetE s = (dlen s / dlen ds) * entropy l s
    dsa = fullfilling ds a (<v)
    dsb = fullfilling ds a (>=v)
  in entropy l ds - subsetE dsa - subsetE dsb


gain :: Label -> DataSet -> Label -> Double
gain l ds a = let
    e = entropy l ds
    f v = (dlen ds' / dlen ds) * entropy l ds'
      where
        ds' = fullfilling ds a (==v)
  in e - (sum $ map f $ attrVals ds a)


dlen :: DataSet -> Double
dlen ds = fromIntegral . length $ ds ^. rows

attrVals :: DataSet -> Label -> [Attribute]
attrVals ds a = nub $ ds ^.. rows . traversed . attr a


fullfilling :: DataSet -> Label -> (Attribute -> Bool) -> DataSet
fullfilling ds a p = ds & rows .~ ds ^.. rows . traverse . filtered (\x -> p $ x ^. attr a)


entropy :: Label -> DataSet -> Double
entropy l ds = - sum [ p * (logBase 2.0 p)  | let occ = occurences l ds, (x, _) <- occ,
                                              let p = occurenceRatio x occ ]


occurenceRatio :: Ord a => a -> [(a, Int)] -> Double
occurenceRatio x occ = let
    all = sum $ map snd occ :: Int
    Just (_, xo) = find ((==x) . fst) occ
  in (fromIntegral xo) / (fromIntegral all)


occurences :: Label -> DataSet -> [(Attribute, Int)]
occurences l ds = let
    dec = ds ^.. rows . traversed . attr l
  in  map (\x -> (head x, length x)) . group $ sort dec

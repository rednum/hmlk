{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Hmlk.Classifiers.Trees where

-- DECISION TREES

import Control.Monad.Reader (ask)
import Control.Lens

import Data.List
import Data.Ord (comparing)

import Hmlk.Classifiers
import Hmlk.DataSet
import Hmlk.Validation


data DecisionTree d = Decision d => Leaf d | Node Label [(Attribute -> Bool, DecisionTree d)]

instance (Show d) => Show (DecisionTree d) where
  show (Leaf x) = "Leaf " ++ (show x)
  show (Node l s) = "Node " ++ l ++ " " ++ (show $ map snd s)


decisionTreeG :: (Ord d, Decision d) =>
  Vote d d -> AttributeGain -> CutGain -> TreeBuilder d -> LabeledClassifier d
decisionTreeG vf agf cgf builder = LabeledClassifier $ do
  (ds, l) <- ask
  let
    tree = builder vf agf cgf l ds
  return $ runTree tree


decisionTree :: (Ord d, Decision d) => TreeBuilder d -> LabeledClassifier d
decisionTree = decisionTreeG majority gainRatio cutGain


buildTree :: (Ord d, Decision d) => TreeBuilder d
buildTree vf agf cgf l ds = let
    empty = length ( _names' ds) == 1 -- only class attribute
    best = snd $ maximum [ (agf l ds a, a) | a <- _names' ds, a /= l]
    classified = (==1) . length $ attrVals ds l
    numericAttr = any isNumeric $ ds ^.. rows . traversed . attr best
    piece p = fullfilling ds best $ p
    nominalChild f = (f, buildTree vf agf cgf l $ dropCols (piece f) (==best))
    numericChild f = (f, buildTree vf agf cgf l $ piece f)
    bestCut = snd $ maximum [ (cgf l ds best v, v) | v <- attrVals ds best ]
  in if empty
    then Leaf $ vf [ fromAttribute x | x <- (ds ^.. rows . traversed . attr l) ]
    else case (classified, numericAttr) of
      (True, _)  -> Leaf . fromAttribute . head $ ds ^.. rows . traversed . attr l
      (_, True)  -> Node best [ numericChild (<bestCut), numericChild (>=bestCut) ]
      (_, False) -> Node best [ nominalChild (==x) | x <- attrVals ds best ]


buildCutTree :: (Ord d, Decision d) => Vote d d -> DataSet -> TreeBuilder d
buildCutTree cv cds vf agf cgf l ds = cutTree cv (buildTree vf agf cgf l ds) l cds


runTree :: (Ord d, Decision d) => DecisionTree d -> DataSet -> [d]
runTree t ds = map (predictRow t) $ ds ^. rows where
   predictRow (Leaf v) _ = v
   predictRow (Node l ch) r = let
       missing = not $ any id [ p v | (p, t') <- ch, let v = r ^. attr l ]
     in majority [ predictRow t' r | (p, t') <- ch, let v = r ^. attr l,
                                     p v || v == Missing || missing ]


cutTree :: (Ord d, Decision d) =>
  Vote d d -> DecisionTree d -> Label -> DataSet -> DecisionTree d
cutTree vf (Leaf v) _ _ = Leaf v
cutTree vf (Node l ch) d ds = let
    ch' = [ (p, cutTree vf t d $ fullfilling ds l p) | (p, t) <- ch ]
    isLeaf (Leaf _) = True
    isLeaf _ = False
  in if all (isLeaf . snd) ch'
    then let
        ex = [ fromAttribute x | x <- (ds ^.. rows . traversed . attr d) ]
        t1 = Leaf $ vf [ d | (_, Leaf d) <- ch' ]
        t2 = Node l ch'
        re1 = recall ex $ runTree t1 ds
        re2 = recall ex $ runTree t2 ds
      in if re1 > re2
        then t1
        else t2
    else Node l ch'


type CutGain = Label -> DataSet -> Label -> Attribute -> Double
type AttributeGain = Label -> DataSet -> Label -> Double
type TreeBuilder d = Vote d d -> AttributeGain -> CutGain -> Label -> DataSet -> DecisionTree d


gainRatio :: AttributeGain
gainRatio l ds a = gain l ds a / splitInformation ds a


splitInformation :: DataSet -> Label -> Double
splitInformation ds a = sum [ - p * (logBase 2.0 p)  | v <- attrVals ds a,
                                    let ds' = fullfilling ds a (==v)
                                        p = dlen ds' / dlen ds ]


cutGain :: CutGain
cutGain l ds a v = let
    subsetE s = (dlen s / dlen ds) * entropy l s
    dsa = fullfilling ds a (<v)
    dsb = fullfilling ds a (>=v)
  in entropy l ds - subsetE dsa - subsetE dsb


gain :: AttributeGain
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



majority :: (Ord d) => Vote d d
majority = head . maximumBy (comparing length) . group . sort

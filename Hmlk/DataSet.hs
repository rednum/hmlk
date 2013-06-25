{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Hmlk.DataSet where

import Debug.Trace
import Data.IntMap (IntMap, fromList, elems, size, toList)
import Data.Monoid
import Data.MultiSet (findMax, insert, empty)
import Data.List (maximumBy, elemIndex)
import Data.Function (on)
import Control.Lens hiding (rmap)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

data Attribute = Missing | Numeric Double | Nominal String | Boolean Bool deriving (Eq, Show, Ord)
data Row = Row {_attributes :: [Attribute], _names :: [String]} deriving (Show)
data DataSet = DataSet {_rows :: IntMap [Attribute], _names' :: [String]}

makeLenses ''Row 

rows :: Lens' DataSet [Row]
rows = lens getter (const buildRows)
  where
    getter ds = map (\x -> Row {_attributes = x, _names = _names' ds}) . elems . _rows $ ds

buildRows rs = DataSet {_rows = fromList . zip [1..] . map _attributes $ rs,
                       _names' = maybeNames rs} where
   maybeNames [] = []
   maybeNames rs = _names . head $ rs


instance Monoid Row where
  Row {_attributes = a1, _names = n1} `mappend` Row {_attributes = a2, _names = n2} = 
    Row {_attributes = a1 ++ a2, _names = n1 ++ n2} 
  mempty = Row {_attributes = [], _names = []}


rmap :: (String -> Attribute -> a) -> Row -> [a]
rmap f r = zipWith f (r ^. names) (r ^. attributes)

namedNominals :: Row -> [(String, String)]
namedNominals = map (\(x, Nominal n) -> (x, n)) . filter (isNominal . snd) . rmap (,) 

attr :: String -> Lens' Row Attribute
attr s = lens getter setter
  where
    getter Row {_attributes = a, _names = n} = 
      case s `elemIndex` n of 
        Just i -> a !! i
        Nothing -> error "Attribute doesn't exist"
    setter (Row {_attributes = a, _names = n}) x =
      case s `elemIndex` n of 
        Just i -> (Row {_names = n, _attributes = (a & (element i) .~ x)})
        Nothing -> error "Attribute doesn't exist"


dropCols :: DataSet -> (String -> Bool) -> DataSet
dropCols ds f = ds & rows .~ newRows 
  where
    newRows = map dropCols' (ds ^. rows)
    dropCols' row = row & names .~ n & attributes .~ a 
      where
        a = filterW c $ row ^. attributes
    n = filterW c $ _names' ds
    c = map (\x -> not $ f x) $ _names' ds


filterW :: [Bool] -> [a] -> [a]
filterW ps xs = [x | (x, p) <- zip xs ps, p]


numeric :: String -> Lens' Row Double
numeric s = lens getter setter
  where
    getter r = case r ^. attr s of
       Numeric n -> n
       _ -> 0 
    setter r x = case r ^. attr s of
       Numeric n -> r & attr s .~ Numeric x
       _ -> r
     

nominal :: String -> Lens' Row String
nominal s = lens getter setter
  where
    getter r = case r ^. attr s of
      Nominal n -> n
      _ -> ""
    setter r x = case r ^. attr s of
      Nominal n -> r & attr s .~ Nominal x
      _ -> r


addAttr :: String -> (Row -> Attribute) -> Row -> Row
addAttr name f r = r & attributes <>~ [(f r)] & names <>~ [name]

rmAttr :: String -> Row -> Row
rmAttr name r = r & attributes %~ remove & names %~ remove
  where
    i = case name `elemIndex` (r ^. names) of
      Just i -> i + 1
      Nothing -> error $ "Attribute " ++ name ++ " does not exist"
    remove l = [x | (j, x) <- zip [1..] l, j /= i]
    

instance Show DataSet where
  show d@DataSet {_rows = r, _names' = n} 
    | size r > 20 = header ++ "\n(too many rows too show: " ++ (show $ size r) ++ " - use \"dumpData\" to see them)"
    | otherwise = dumpData' d
      where
        paddingSize = 16 --fixme
        padding x = (replicate (paddingSize - length x + 1) ' ') ++ x ++ " |"
        header = unwords . map padding $ n

dumpData' DataSet {_rows = r, _names' = n} = unlines $ header:(replicate (length header) '-'):datas
      where 
        paddingSize = 16 --fixme
        padding x = (replicate (paddingSize - length x + 1) ' ') ++ x ++ " |"
        header = unwords . map padding $ n
        datas = [unwords . map (padding . showAttribute) $ x | (_, x) <- toList r] 
        showAttribute (Numeric n) = show n
        showAttribute (Nominal n) = n
        showAttribute (Boolean n) = show n
        showAttribute Missing = "?"

dumpData ds = putStrLn $ dumpData' ds

numericsOf :: DataSet -> [[Double]]
numericsOf ds = map (map strip . filter isNumeric) (ds ^.. rows . traverse . attributes)
  where
    strip (Numeric n) = n

nominalsOf :: DataSet -> [[String]]
nominalsOf ds = map (map strip . filter isNominal) (ds ^.. rows . traverse . attributes)
  where
    strip (Nominal n) = n

isNumeric :: Attribute -> Bool
isNumeric (Numeric n) = True
isNumeric _ = False

isNominal :: Attribute -> Bool
isNominal (Nominal n) = True
isNominal _ = False

isMissing :: Attribute -> Bool
isMissing Missing = True
isMissing _ = False

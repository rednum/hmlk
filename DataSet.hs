{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module DataSet where

import Debug.Trace
import Data.IntMap (IntMap, fromList, elems, size, toList)
import Data.Monoid
import Data.MultiSet (findMax, insert, empty)
import Data.List (maximumBy, elemIndex)
import Data.Function (on)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

data Attribute = Missing | Numeric Double | Nominal String deriving (Eq, Show, Ord)
data Row = Row {_attributes :: [Attribute], _names :: [String]} deriving (Show)
data DataSet = DataSet {_rows :: IntMap [Attribute], _names' :: [String]}



makeLenses ''Row 

rows :: Lens' DataSet [Row]
rows = lens getter setter 
  where
    getter ds = map (\x -> Row {_attributes = x, _names = _names' ds}) . elems . _rows $ ds
    setter _ rs = DataSet {_rows = fromList . zip [1..] . map _attributes $ rs,
                           _names' = maybeNames rs} where
       maybeNames [] = []
       maybeNames rs = _names . head $ rs


instance Monoid Row where
  Row {_attributes = a1, _names = n1} `mappend` Row {_attributes = a2, _names = n2} = 
    Row {_attributes = a1 ++ a2, _names = n1 ++ n2} 
  mempty = Row {_attributes = [], _names = []}


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

-- lens do wyciagania numerycznych atrybutow - sprawdz examples zeby zobaczyc do czego sluzy
numeric :: String -> Lens' Row Double
numeric s = lens getter setter
  where
    getter r = case r ^. attr s of
       Numeric n -> n
       _ -> 0 
    setter r x = case r ^. attr s of
       Numeric n -> r & attr s .~ Numeric x
       _ -> r
     
-- analogicznie do numeric, ale na atrybutach nominalnych
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
addAttr name f (r@Row {_attributes=a, _names=n}) = Row {_names = name:n, _attributes=(f r):a}

-- TODO
readCSV :: FilePath -> IO DataSet
readCSV _ = undefined

-- nominal "color" ds1 
-- zmien zbior postaci
-- color |    x |  decision
-- red   |    1 |  1
-- blue  |    2 |  0
-- red   |    4 |  1
-- red   |   10 |  0
--
-- na:
--
-- colorRed | colorBlue |    x |  decision
--        1 |         0 |    1 |  1
--        0 |         1 |    2 |  0
--        1 |         0 |    4 |  1
--        1 |         0 |   10 |  0
--
-- TODO
nominalToNumerics :: String -> DataSet -> DataSet
nominalToNumerics _ _ = undefined


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
        showAttribute Missing = "?"

dumpData ds = putStrLn $ dumpData' ds





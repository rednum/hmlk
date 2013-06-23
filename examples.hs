import Control.Lens
import Data.IntMap (fromList)
import DataSet
import Classifiers
import Validation

ds :: DataSet
ds = DataSet {_rows = fromList . zip [1..] $ 
              [[Numeric 0, Numeric 0, Nominal "red", Numeric 0], 
               [Numeric 1, Numeric 1, Nominal "red", Numeric 3], 
               [Numeric 1, Numeric (-2), Nominal "blue", Numeric 4],
               [Numeric 1, Numeric 0, Nominal "red", Numeric 0], 
               [Numeric 0, Numeric 2, Nominal "red", Numeric 3], 
               [Numeric 5, Numeric 5, Nominal "blue", Numeric 4],
               [Numeric 0, Numeric 5, Nominal "red", Numeric 0], 
               [Numeric 1, Numeric 5, Nominal "red", Numeric 3], 
               [Numeric 5, Numeric 6, Nominal "blue", Numeric 4]],

              _names' = ["x", "y", "color", "value"]}

dsb = DataSet {_rows = fromList . zip [1..] $ [[Boolean True, Boolean False, Boolean True]],
               _names' = ["decision"]}

dsc = DataSet {_rows = fromList . zip [1..] $ [[Boolean True, Boolean True, Boolean False]],
               _names' = ["decision"]}


-- data set, lensowanie
sampleRow = Row {_attributes = [Numeric 10, Nominal "blue", Numeric 4], _names = ["x", "color", "decision"]}
ex00 = sampleRow ^. attr "x" -- pokaz wartosc X
ex01 = (ds ^. rows) !! 0  -- wez zerowy rzad
ex10 = (ds ^. rows) !! 0 ^. numeric "x" -- wez zerowy rzad i pokaz "x"
ex11 = sampleRow & numeric "x" .~ 100 -- dodaj 100 do X
ex2 = (ds ^. rows) !! 0 & numeric "x" +~ 100 -- wez zerowy rzad i dodaj 100 do wartosci atrybutu "x"
ex3 = ds ^.. rows . traversed . attr "x"  -- pokaz wszystkie wartosci atrybutu "x"
ex4 = ds & rows . traverse . numeric "x" +~ 1 -- dodaj do wszystkich atrybutow "x" wszysktich obiekotw 1
ex5 = ds & rows . traverse . numeric "x" %~ (\x -> if x > 1 then 2 * x else 0) -- tak jak wyzej, ale zamiast dodawania arbitralna funkcja (\x -> ...)
ex6 = ds ^.. rows . traverse . filtered (\x -> x ^. nominal "color" == "red") -- wez tylko te wiersze ktore maja "color" = red
ex6b = ds & rows .~ fr where
  fr = ds ^.. rows . traverse . filtered (\x -> x ^. nominal "color" == "red") -- jak wyżej, tylko jako DataSet
ex7 = ds & rows . traverse . filtered (\x -> x ^. nominal "color" == "red") . numeric "x" +~ 10 -- dodaj 10 do atrybutu "x" wierszy ktore maja "color" = red
-- 

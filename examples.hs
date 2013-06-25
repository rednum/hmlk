import Bayesian
import Control.Lens
import Data.IntMap (fromList)
import DataSet
import Classifiers
import Clustering
import Validation

ds :: DataSet
ds = DataSet {_rows = fromList . zip [1..] $ 
              [[Numeric 0, Numeric 0, Nominal "red", Numeric 0], 
               [Numeric 1, Numeric 1, Nominal "red", Numeric 3], 
               [Numeric 1, Numeric (-2), Nominal "blue", Numeric 4],
               [Numeric 1, Numeric 0, Nominal "red", Numeric 0], 
               [Numeric 0, Numeric 2, Nominal "red", Numeric 3], 
               [Numeric 16, Numeric 15, Nominal "blue", Numeric 4],
               [Numeric 15, Numeric 15, Nominal "blue", Numeric 4],
               [Numeric 0, Numeric 25, Nominal "red", Numeric 0], 
               [Numeric 1, Numeric 23, Nominal "red", Numeric 3]
              ],

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


-- przyklad do drzew
tennis = DataSet {_rows = fromList . zip [1..] $
                  [[Nominal "Sunny", Nominal "Hot", Nominal "High", Nominal "Weak", Boolean False],
                   [Nominal "Sunny", Nominal "Hot", Nominal "High", Nominal "Strong", Boolean False],
                   [Nominal "Overcast", Nominal "Hot", Nominal "High", Nominal "Weak", Boolean True],
                   [Nominal "Rain", Nominal "Mild", Nominal "High", Nominal "Weak", Boolean True],
                   [Nominal "Rain", Nominal "Cool", Nominal "Normal", Nominal "Weak", Boolean True],
                   [Nominal "Rain", Nominal "Cool", Nominal "Normal", Nominal "Strong", Boolean False],
                   [Nominal "Overcast", Nominal "Cool", Nominal "Normal", Nominal "Strong", Boolean True],
                   [Nominal "Sunny", Nominal "Mild", Nominal "High", Nominal "Weak", Boolean False],
                   [Nominal "Sunny", Nominal "Cool", Nominal "Normal", Nominal "Weak", Boolean True],
                   [Nominal "Rain", Nominal "Mild", Nominal "Normal", Nominal "Weak", Boolean True],
                   [Nominal "Sunny", Nominal "Mild", Nominal "Normal", Nominal "Strong", Boolean True],
                   [Nominal "Overcast", Nominal "Mild", Nominal "High", Nominal "Strong", Boolean True],
                   [Nominal "Overcast", Nominal "Hot", Nominal "Normal", Nominal "Weak", Boolean True],
                   [Nominal "Rain", Nominal "Mild", Nominal "High", Nominal "Strong", Boolean False]],

                  _names' = ["Outlook", "Temperature", "Humidity", "Wind", "PlayTennis"]}
                    


-- do bayesa
cars = DataSet {_rows = fromList . zip [1..] $
      [ [Nominal "Red", Nominal "Sports", Nominal "Domestic", Nominal "Yes"]
      , [Nominal "Red", Nominal "Sports", Nominal "Domestic", Nominal "No"]
      , [Nominal "Red", Nominal "Sports", Nominal "Domestic", Nominal "Yes"]
      , [Nominal "Yellow", Nominal "Sports", Nominal "Domestic", Nominal "No"]
      , [Nominal "Yellow", Nominal "Sports", Nominal "Imported", Nominal "Yes"]
      , [Nominal "Yellow", Nominal "SUV", Nominal "Imported", Nominal "No"]
      , [Nominal "Yellow", Nominal "SUV", Nominal "Imported", Nominal "Yes"]
      , [Nominal "Yellow", Nominal "SUV", Nominal "Domestic", Nominal "No"]
      , [Nominal "Red", Nominal "SUV", Nominal "Imported", Nominal "No"]
      , [Nominal "Red", Nominal "Sports", Nominal "Imported", Nominal "Yes"]],

      _names' = ["Color", "Type", "Origin", "Stolen"]}
cars' = DataSet {_rows = fromList . zip [1..] $
      [ [Nominal "Red", Nominal "SUV", Nominal "Imported", Nominal "No"]
      , [Nominal "Red", Nominal "Sports", Nominal "Domestic", Nominal "Yes"]],

      _names' = ["Color", "Type", "Origin", "Stolen"]}

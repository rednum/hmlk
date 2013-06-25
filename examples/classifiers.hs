{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Control.Monad (forM)
import Data.IntMap (fromList)

import Hmlk.Classifiers.Bayesian
import Hmlk.Classifiers.Lazy
import Hmlk.Classifiers.Linear
import Hmlk.Classifiers.Clustering
import Hmlk.Classifiers.Trees
import Hmlk.DataSet
import Hmlk.Classifiers
import Hmlk.Validation


runAll = do
    putStrLn "\n\nRunning clustering:"
    clustering 3
    putStrLn . take 80 $ repeat '='
    putStrLn "\n\nRunning KNN:"
    knns
    putStrLn . take 80 $ repeat '='
    putStrLn "\n\nRunning linear:"
    linearRegression
    putStrLn . take 80 $ repeat '='
    putStrLn "\n\nRunning trees:"
    trees
    putStrLn . take 80 $ repeat '='
    putStrLn "\n\nRunning bayes:"
    bayes
    putStrLn . take 80 $ repeat '='

ds :: DataSet
ds = DataSet {_rows = fromList . zip [1..] $ 
              [[Numeric 0, Numeric 0, Nominal "red"], 
               [Numeric 1, Numeric 1, Nominal "red"], 
               [Numeric 1, Numeric (-2), Nominal "blue"],
               [Numeric 1, Numeric 0, Nominal "red"], 
               [Numeric 0, Numeric 2, Nominal "red"], 
               [Numeric 16, Numeric 15, Nominal "blue"],
               [Numeric 15, Numeric 15, Nominal "blue"],
               [Numeric 0, Numeric 25, Nominal "red"], 
               [Numeric 1, Numeric 23, Nominal "red"]
              ],

              _names' = ["x", "y", "color"]}
-- clustering
clustering :: Int -> IO ()
clustering i = do
    print ds
    forM [1..i] $ \_ -> do
        c <- trainClassifier (Hmlk.Classifiers.Clustering.kMeans 3) ds "color" 
        putStrLn . unwords $ c ds 
    return ()

-- kNN
knns :: IO ()
knns = do
    print ds
    c <- trainClassifier (Hmlk.Classifiers.Lazy.simpleKNN 3) ds "color" 
    putStrLn . unwords $ c ds 
    return ()



-- linear
ds2 :: DataSet
ds2 = DataSet {_rows = fromList . zip [1..] $
                  [ [Numeric 1, Numeric 1, Numeric 11]
                  , [Numeric 2, Numeric 3, Numeric 24]
                  , [Numeric 5, Numeric 5, Numeric 50]
                  , [Numeric 4, Numeric 4, Numeric 45]],
                  _names' = ["x", "y", "value"]}

linearRegression :: IO ()
linearRegression = do
    print ds2
    c <- trainClassifier (Hmlk.Classifiers.Linear.linear) ds2 "value"
    putStrLn "Predictions: "
    putStrLn . unwords $ map show (c ds2) 

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


tennis' = DataSet {_rows = fromList . zip [1..] $
                  [[Nominal "Sunny", Nominal "Hot", Nominal "Normal", Nominal "Strong", Missing],
                   [Nominal "Rain", Nominal "Cool", Nominal "High", Nominal "Strong", Missing],
                   [Nominal "Rain", Nominal "Mild", Nominal "High", Nominal "Weak", Missing],
                   [Nominal "Overcast", Nominal "Mild", Nominal "Normal", Nominal "Weak", Missing]],

                  _names' = ["Outlook", "Temperature", "Humidity", "Wind", "PlayTennis"]}
                    

trees :: IO ()
trees =  do
    print tennis
    (c::Trained Bool) <- trainClassifier (Hmlk.Classifiers.Trees.decisionTree buildTree) tennis "PlayTennis"
    print tennis'
    print (c tennis')

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
      [ [Nominal "Red", Nominal "SUV", Nominal "Imported", Missing]
      , [Nominal "Red", Nominal "Sports", Nominal "Domestic", Missing]],

      _names' = ["Color", "Type", "Origin", "Stolen"]}


bayes :: IO ()
bayes = do
    print cars
    (c::Trained String) <- trainClassifier (Hmlk.Classifiers.Bayesian.naiveBayes) cars "Stolen"
    print cars'
    print (c cars')

import Control.Lens

import Data.IntMap (fromList)

import Hmlk.Classifiers
import Hmlk.Classifiers.Lazy
import Hmlk.Classifiers.Trees
import Hmlk.DataSet
import Hmlk.Validation


runAll = do
    putStrLn "\n\nRunning KNN:"
    knns
    putStrLn . take 80 $ repeat '='
    putStrLn "\n\nRunning trees:"
    trees
    putStrLn . take 80 $ repeat '='


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


-- kNN
knns :: IO ()
knns = do
    print ds
    let c = Hmlk.Classifiers.Lazy.simpleKNN 3 :: Hmlk.Classifiers.UnlabeledClassifier Double
    res <- crossValidate "value" recall ds 0.8 10 c
    putStrLn "Cross-validation using 80:20 split:"
    print res
    print $ mean res
    return ()



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
                    



-- decision tree
trees :: IO ()
trees = do
    let
      validate = crossValidate "PlayTennis" recall tennis 0.8 10
      c1 = Hmlk.Classifiers.Trees.decisionTreeG Hmlk.Classifiers.Trees.majority Hmlk.Classifiers.Trees.gain
                 Hmlk.Classifiers.Trees.cutGain Hmlk.Classifiers.Trees.buildTree :: Hmlk.Classifiers.LabeledClassifier Bool
      c2 = Hmlk.Classifiers.Trees.decisionTree Hmlk.Classifiers.Trees.buildTree :: Hmlk.Classifiers.LabeledClassifier Bool

    print tennis

    res1 <- validate c1
    putStrLn "* [normal gain function] Cross-validation using 80:20 split:"
    print res1
    print $ mean res1 

    res2 <- validate c2
    putStrLn "\n* [normalized gain function] Cross-validation using 80:20 split:"
    print res2
    print $ mean res2
    return ()


mean :: [Double] -> Double
mean x = sum x / (fromIntegral $ length x)

Haskell Machine Learning Kit
===============================


Installation and dependencies
--------------------------------

For HMatrix you will need to install libgsl - see [https://github.com/AlbertoRuiz/hmatrix/blob/master/INSTALL.md]

Than just run

    cabal install

in the main directory.



Examples
-------------

Examples of how to use classifiers and cross-validation can be found inside the `examples` directory.


### DataSet manipulation #####


* Query value of attribute `x` of row:

        row ^. attr "x"



* Query first row from the DataSet:

        (ds ^. rows) !! 0

* Query value of attribute `x` in first row:

        (ds ^. rows) !! 0 ^. numeric "x"
        
* Add 100 to attribute `x`:

        sampleRow & numeric "x" .~ 100
        
* Query first row and add 100 to attribute `x`:

        (ds ^. rows) !! 0 & numeric "x" +~ 100
        
* Query all values of attribute `x`:

        ds ^.. rows . traversed . attr "x"
        
* Add 1 to attribute `x` in all rows:
 
        ds & rows . traverse . numeric "x" +~ 1

* Map a function over attribute `x` values in all rows:

        ds & rows . traverse . numeric "x" %~ (\x -> if x > 1 then 2 * x else 0)
        
* Query rows which have `red` as a value of `color` attribute:

        ds & rows .~ fr where
              fr = ds ^.. rows . traverse . filtered (\x -> x ^. nominal "color" == "red")

* Add 10 to attribute `x` in rows which have `red` as a value of `color` attribute:

        ds & rows . traverse . filtered (\x -> x ^. nominal "color" == "red") . numeric "x" +~ 10

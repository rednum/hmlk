import mapaProsta

type Trained = Monad m => DataSet -> m DataSet -- tak naprawde m - random
type Classifier = DataSet -> Trained 

-- tutaj jeszcze cos z tymi typami
-- jakie jesszcze klasyfikatory?
-- unsuperverised
--


simpleVote :: [Classifier] -> Classifier
simpleVote l = do
  return undefined


-- fejkowy KNN
-- wybiera pare egzemplarzy ze zbioru treningowego i zapuszcza na nim 1-kNN
-- prawdopodobnie bardzo kiepski klasyfikator, ale ma za zadanie zaprezentowac monade random
fakeKNN :: Trainable
fakeKNN training test = do
  forM test (\t ->
    print "Wybierz typow z treningowego"
    print "klasyfkiuj patrzac na nich"
  )



-- CLASSIFIERS
type Classifier = DataSet -> [Attribute]

majorityFactory :: String -> DataSet -> Classifier
majorityFactory an ts = \ds -> map (\row -> result) $ ds ^. rows where
    result = majority $ ts ^.. rows . traversed . attr an
    majority vals = findMax $ foldl (flip insert) empty vals


-- splits DataSet into two disjoint DataSets
--
-- TODO : add randomness via some kind of Monad
split :: DataSet -> (DataSet, DataSet)
split = undefined



type Metric = [Attribute] -> [Attribute] -> Double

crossValidate :: DataSet -> Metric -> Classifier -> Double
crossValidate = undefined


stupidMetric :: Metric
stupidMetric ex re = mean where
    mean = (foldl (+) 0.0 diffs) / (fromIntegral $ length diffs) 
    diffs = [ (a - b) | (Numeric a, Numeric b) <- zip ex re]


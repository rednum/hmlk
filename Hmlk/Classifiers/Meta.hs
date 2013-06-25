module Hmlk.Classifiers.Meta where

import Hmlk.Classifiers
import Hmlk.DataSet

simpleVote :: Decision d => [Trained d] -> Trained d
simpleVote l = do
  return undefined

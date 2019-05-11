module Main where

import Euterpea
import Compositions
import Percussion
import Utils

main = do
  --play $ forever Percussion.groove
  play $ forever Compositions.composition


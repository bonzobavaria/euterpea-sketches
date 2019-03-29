module Utils (
  addDur
) where

import Euterpea

addDur :: Dur -> [(Dur -> Music a)] -> Music a
addDur d ns = line [n d | n <- ns]


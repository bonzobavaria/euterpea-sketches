module Scenes where

import Euterpea
import Percussion
import Scratch

main = do
  let pattern = forever Scratch.pattern1
      beat = Percussion.groove
  play $ (:=:) pattern beat

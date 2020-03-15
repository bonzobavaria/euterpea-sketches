module Composition.Scenes where

import Euterpea

import qualified Composition.Percussion as Percussion
import qualified Composition.Scratch as Scratch

main = do
  let pattern = forever Scratch.pattern1
      beat = Percussion.groove
  play $ (:=:) pattern beat

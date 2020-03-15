module Main where

import Euterpea

import Composition.Percussion (beat)
import Composition.Session (linksAdventure)
import qualified Utils.Utils as Utils

main =
 play $ chord $ map forever [beat, linksAdventure]

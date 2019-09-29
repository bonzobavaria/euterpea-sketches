module Main where

import Euterpea
import Compositions
import Percussion
import Session
import Utils

main =
 play $ chord $ map forever [beat, linksAdventure]

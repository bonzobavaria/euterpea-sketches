module Scales where

-- TODO: this module shouldn't need to import Euterpea, since this concept
-- could be meaningful in any project. Utilities that make scales are probably
-- part of Utils.hs
import Euterpea

type Scale = [Int]

minor = aeolian :: Scale
major = ionian
chromatic =       [0..11]
ionian =          [0,2,4,5,7,9,11]
dorian =          [0,2,3,5,7,9,10]
phrygian =        [0,1,3,5,7,8,10]
lydian =          [0,2,4,6,7,9,11]
mixolydian =      [0,2,4,5,7,9,10]
aeolian =         [0,2,3,5,7,8,10]
locrian =         [0,1,3,5,6,8,10]
harmonicMinor =   [0,2,3,5,7,8,11]
melodicMinor =    [0,2,3,5,7,9,11]
hungarianMinor =  [0,2,3,6,7,8,10]

-- Ex: scale D major = map (+2) major
scale :: PitchClass -> Scale -> Scale
scale key sc = map (+(absPitch(key,-1))) sc

-- get a list of every possible AbsPitch for a scale
mkScale :: Scale -> [AbsPitch]
mkScale scale = 
  let everyNote = concat $ take 11 $ iterate (map (+12)) scale
  in takeWhile (<= 127) everyNote


module Scales where

-- TODO: this module shouldn't need to import Euterpea, since this concept
-- could be meaningful in any project. Utilities that make scales are probably
-- part of Utils.hs
import Euterpea

type Scale = [Int]

-- FIXME: Scales they don't have a type annotation don't match the type `Scale`. Is there a better way to use types to define scales?

minor = aeolian                    :: Scale
major = ionian                     :: Scale
chromatic =       [0..11]          :: Scale
ionian =          [0,2,4,5,7,9,11] :: Scale
dorian =          [0,2,3,5,7,9,10] :: Scale
phrygian =        [0,1,3,5,7,8,10] :: Scale
lydian =          [0,2,4,6,7,9,11] :: Scale
mixolydian =      [0,2,4,5,7,9,10] :: Scale
aeolian =         [0,2,3,5,7,8,10] :: Scale
locrian =         [0,1,3,5,6,8,10] :: Scale
harmonicMinor =   [0,2,3,5,7,8,11] :: Scale
melodicMinor =    [0,2,3,5,7,9,11] :: Scale
hungarianMinor =  [0,2,3,6,7,8,10] :: Scale
harmonicMajor =   [0,2,4,5,7,8,10] :: Scale

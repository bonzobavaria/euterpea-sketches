module Utils.Scales where

type Scale = [Int]

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

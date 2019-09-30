module Utils (
  addDur,
  cheat,
  clip,
  extend,
  melody,
  mkScale,
  Motif,
  NoteSet,
  progress,
  render,
  sequence,
  unravel,
) where

import Euterpea
import Scales (Scale)
import Prelude hiding (sequence)
import Data.List (elemIndex)

addDur :: Dur -> [(Dur -> Music a)] -> Music a
addDur d ns = line [n d | n <- ns]

-- TODO: I don't think unravel is working at the level
-- of absPitch yet. It needs to be a part of a scale first.
-- type Pattern = [Int]
-- unravel :: [[Int]] -> Pattern
-- TODO: Figure out which fold you should use, foldr, foldl, or foldl'.
unravel :: [[AbsPitch]] -> [AbsPitch]
unravel [] = []
unravel (x:xs) =
  let func = ((<*>) . ((<$>) (+)))
  in foldl func x xs
--unravel = foldl ((<*>) . ((<$>) (+)))

-- Examples:
--   unravel [[0,3,7,12],[0..3],[0,12]]
-- Ascending minor arpeggios, going up by half-step, then
-- the same sequence up an octave. So the first value is the innermost
-- arpeggiated shape.
-- 
-- unravel $ reverse [[0..3],[0,5..15],[0,12]
-- = Octaves, moving up in quintal motion, with the pattern repeating 4 times
-- and moving up a half-step each time, so the first value is the outermost
-- shape, i.e. the one that completes only once and has the longest duration.

-- A motif is a medolic theme that is independent of key or intervals.
type Motif = [Int]
type NoteSet = [Int]

extend :: [Int] -> NoteSet
extend = (takeWhile (<= 127)) . concat . (iterate (map (+12)))

-- TODO: How's this going to work? We could just recurse intil we find a Pitch
-- we want.
render :: Motif -> NoteSet -> Pitch -> [Pitch]
render motif nSet p =
  case elemIndex (absPitch p) nSet of
    Nothing ->
      [(C,5)] 
    Just x ->
      map pitch $ map ((nSet !!) . (+ x)) motif

-- Ex: scale D major = map (+2) major
--scale :: PitchClass -> Scale -> Scale
--scale key sc = map (+(absPitch(key,-1))) sc

-- TODO: Deprecate, still in use by Scratch.hs
-- get a list of every possible AbsPitch for a scale
mkScale :: Scale -> [AbsPitch]
mkScale scale = 
  let everyNote = concat $ take 11 $ iterate (map (+12)) scale
  in takeWhile (<= 127) everyNote

-- TODO: Deprecate. use render to get a [Pitch]
-- This function is for mapping scale indices to a scale
-- TODO: Euterpea can make this easier, and it may not be meaningful once scale
-- and chords are all note sets across the entire midi range.
sequence :: Scale -> Octave -> Motif -> [AbsPitch]
sequence scale oct = map ((+(12*oct)) . (scale !!))

melody :: [Dur] -> [AbsPitch] -> [Music Pitch]
melody rhythm seq = zipWith note (cycle rhythm) $ map pitch seq


-- Should clip be responsible for using `line` to convert [[Music a] to Music a?
clip :: InstrumentName -> [Music Pitch] -> Music Pitch 
clip inst = (instrument inst) . line

-- Pipeline for quickly turning a motif into a (Music a)
-- TODO: Rename this function
cheat :: InstrumentName -> [AbsPitch] -> [Dur] -> Octave -> Motif -> Music Pitch 
cheat inst scale rhythm octave =
  (clip inst) . (melody rhythm) . (sequence scale octave)

-- This is one way to create a patterned motif that iteratively glides upward
-- through a motif. This function doesn't care it it's working on [AbsPitch] or
-- a motif
progress :: Motif -> Int -> Motif
progress p len
  | length p < len = []
  | otherwise = take len p ++ (progress (tail p) len)

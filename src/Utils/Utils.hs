module Utils.Utils (
  addDur,
  cheat,
  clip,
  extend,
  inject,
  melody,
  mkMelody,
  mkScale,
  mkScale',
  Motif,
  NoteSet,
  progress,
  render,
  renderMotif,
  scale,
  sequence,
  unravel,
) where

import Euterpea
import Utils.Scales (Scale)
import Prelude hiding (sequence)
import Data.List (elemIndex)

addDur :: Dur -> [(Dur -> Music a)] -> Music a
addDur d ns = line [n d | n <- ns]

unravel :: [[AbsPitch]] -> [AbsPitch]
unravel [] = []
unravel xs =
  let func = ((<*>) . ((<$>) (+)))
  in foldl1 func xs

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
type Rhythm = [Dur]

extend :: [Int] -> NoteSet
extend = (takeWhile (<= 127)) . concat . (iterate (map (+12)))

-- Render Motif into NoteSet, starting at nearest Pitch that is a member of the
-- NoteSet
renderMotif :: Motif -> NoteSet -> Pitch -> [AbsPitch]
renderMotif motif nSet p =
  case elemIndex (absPitch p) nSet of
    Nothing ->
        render motif nSet (trans 1 p)
    -- FIXME: This is a partial function. Use a safe index function instead.
    Just x -> map ((nSet !!) . (+ x)) motif

-- Deprecated name for this function, still in use
render = renderMotif

-- Ex: scale D major = map (+2) major
scale :: PitchClass -> Scale -> Scale
scale key sc = map (+(absPitch(key,-1))) sc

-- Get a list of every possible AbsPitch for a scale
mkScale scale = 
  let everyNote = concat $ take 11 $ iterate (map (+12)) scale
  in takeWhile (<= 127) everyNote

mkScale' :: PitchClass -> Scale -> NoteSet
mkScale' key scale = 
  let everyNote = concat $ take 11 $ iterate (map (+12)) scale
      adjustedScale = map (+ (absPitch (key, -1))) everyNote
  in takeWhile (<= 127) adjustedScale

-- TODO: Deprecate. use render to get a [Pitch]
-- This function is for mapping scale indices to a scale
-- TODO: Euterpea can make this easier, and it may not be meaningful once scale
-- and chords are all note sets across the entire midi range.
-- FIXME: This is a partial function
sequence :: Scale -> Octave -> Motif -> [AbsPitch]
sequence scale oct = map ((+(12*oct)) . (scale !!))

melody :: [Dur] -> [AbsPitch] -> [Music Pitch]
melody rhythm seq = zipWith note (cycle rhythm) $ map pitch seq

mkMelody = melody

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

-- Well anyway injecting probably isn't the nicest way to write a composition.
-- You'll probably be much happier with unravel, and rendering with a matching
-- rhythm
-- This will inject a motive into a sequence every n notes
inject :: [Pitch] -> Motif -> NoteSet -> Int -> [Pitch]
inject [] _ _ _ = []
inject (x:xs) motive nSet count
  | mod count 4 == 0 = (map pitch (render motive nSet x)) ++ (inject xs motive nSet (count + 1))
  | otherwise = [x] ++ (inject xs motive nSet (count + 1))

inject' [] _ _ = []
inject' (x:xs) motive count
  | mod count 4 == 0 = (map (\y -> trans y x) motive) ++ (inject' xs motive (count + 1))
  | otherwise = [x] ++ (inject' xs motive (count + 1))
-- TODO: How about an inject function that uses a conditional instead of a
-- counter?
-- TODO: Would it be nicer to only inject into [Music Pitch], instead of having
-- to build out a totally separate rhythm?


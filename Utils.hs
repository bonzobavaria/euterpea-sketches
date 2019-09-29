module Utils (
  addDur,
  cheat,
  clip,
  melody,
  unravel,
  mkScale,
  scale,
  seqToScale,
) where

import Euterpea
import Scales (Scale)

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

-- TODO: This is actually a Motif, but that word sucks
type Sequence = [Int]

-- Ex: scale D major = map (+2) major
scale :: PitchClass -> Scale -> Scale
scale key sc = map (+(absPitch(key,-1))) sc

-- get a list of every possible AbsPitch for a scale
mkScale :: Scale -> [AbsPitch]
mkScale scale = 
  let everyNote = concat $ take 11 $ iterate (map (+12)) scale
  in takeWhile (<= 127) everyNote

-- TODO: sequence :: Scale -> Octave -> Motif -> [AbsPitch]
seqToScale :: Scale -> Octave -> Sequence -> [AbsPitch]
seqToScale scale oct = map ((+(12*oct)) . (scale !!))

melody :: [Dur] -> [AbsPitch] -> [Music Pitch]
melody rhythm seq = zipWith note (cycle rhythm) $ map pitch seq


-- Should clip be responsible for using `line` to convert [[Music a] to Music a?
clip :: InstrumentName -> [Music Pitch] -> Music Pitch 
clip inst = (instrument inst) . line

-- What's the absolute quickest way to try out a seqeunce?
 --cheat :: Instrument -> Scale -> [Dur] -> Octave -> Sequence -> Music a
cheat :: InstrumentName -> [AbsPitch] -> [Dur] -> Octave -> Sequence -> Music Pitch 
cheat inst scale rhythm octave =
  (clip inst) . (melody rhythm) . (seqToScale scale octave)

module Scratch where

import Scales
import Euterpea
import Percussion
import qualified Utils

-- Patterns are ints describing melodic movement, 
-- but not tied to a particular scale or octave [Int]
-- Patterns can be applied to a specified scale, or just the 12-tone
-- scale, by default.
-- Rhythms are lines of durations [Dur]
-- Sequences are lines of notes [Pitch] // TODO: make this true
-- Melodies are lines with durations and notes [Music Pitch]
-- Melodies can be reversed and replicated and stuff
-- Clips are playable lines with instruments of type Music a
-- Scenes are groups of clips (Music a :=: Music a)
-- Compositions are sequences of scenes (Scene :+: Scene)

-- ex. 0 = whole note, 1 = half note, 2 = quarter note
-- useful for creating subdivisions, for example in self-similar
-- structures
toDur :: Int -> Dur
toDur x = toRational $ 1/2**(fromIntegral x)

type Scale = [Int]

-- This is a simple, shitty sounding take on the selfSim stuff from
-- Chapter 10.
comp :: Music Pitch
comp = 
  let getNote scaleDeg n = note (toDur n) (pitch (scaleDeg * n + 40))
      getNotes n = forever $ line $ map (\x -> getNote x n) aeolian
  in chord $ map getNotes [0..5]

comp2 :: Music Pitch
comp2 = 
  let indices = zipWith (*) [11,10..4] [5..9]
      scale' = Utils.mkScale $ Utils.scale C aeolian
      notes = map (scale' !!) indices
  in line $ map (note en . pitch) notes

upBy interval times start = take times $ iterate (+interval) start
downBy interval times start = take times $ iterate ((-)interval) start

octavesUp = upBy 12

dMinor = Utils.mkScale $ Utils.scale D minor

comp3 :: Music Pitch
comp3 =
  let notes = dMinor
      octaves x = take 4 $ iterate (+12) x
      pattern = concat $ map octaves (map (notes !!) [32..35])
  in (line $ map (note en . pitch) pattern) :+: (rest 1)

comp4 = 
  let notes = dMinor
      sweep = take 4 $ iterate (+1) (16 * 3)
      inner = concatMap (\x -> take 3 $ iterate (+1) x) sweep
      pattern = map (notes !!) inner
      pitches = map (note sn) (map pitch pattern)    
  in (line pitches) :+: (rest (3/4))

seq1 = 
  let up = concat $ take 4 (iterate (map (+1)) [0..4])
      down = reverse up
  in  up ++ down

-- here's a cool way to make melodic sequences
seq2 =
  let range = [0..3]
  in (+) <$> range <*> range

foldy :: [[AbsPitch]] -> [AbsPitch]
foldy [] = []
foldy (x:xs) =
  let func = ((<*>) . (fmap (+)))
  in foldl func x xs

-- Examples
-- foldy [[0,12],[0..3],[0,3,7,12]]
-- = Ascending minor arpeggios, going up by half-step, then
-- he same sequence up an octave. So the last value is the innermost
-- arpeggiated shape.
--
-- foldy $ reverse [[0,12],[0,5..15],[0..3]]
-- = Octaves, moving up in quintal motion, with the pattern repeating 4 times
-- and moving up a half-step each time, so the last value is the outermost
-- shape, i.e. the one that completes only once and has the longest duration.

seq3 =
  foldy $ reverse [[0,12],[0,5..15],[0..3]]

seq4 =
  let seq = foldy $ [[0,12],[0,7..21],[0,4,7,11]]
  in seq ++ (reverse seq)

seq5 =
  foldy [[0..3],[0,3,5,7],[0,5,8,5]]

gliss n xs = 
  if length xs < n then xs
  else take n xs ++ (gliss n (tail xs))

pattern5 = map pitch $ seqToScale seq5 dMinor 5
rhythm2 = concat $ zipWith replicate [2,4,8,2] [en,sn,(1/32),en]
melody5 = mel rhythm2 pattern5
clip5 = clip Lead4Chiff melody5

melody4 = line $ map (note sn . pitch . (+40)) seq4

pattern6 = foldy [[0..3],[5,0,3,8]]
melody6 = mel [qn] $ map pitch $ seqToScale pattern6 dMinor 3

clip6 = clip Koto (map (slice' 2) melody5)
clip7 = clip Lead8BassLead melody6

mel rhythm seq = zipWith note (cycle rhythm) seq

slice :: Int -> AbsPitch -> [Music Pitch]
slice x p = (replicate x) $ note (1/(fromIntegral x)) (pitch p) 

slice' :: Int -> Music a -> Music a
slice' x (Prim (Note d a)) = line $ replicate x $ Prim $ Note (toRational (d/(fromIntegral x))) a

clip inst melody = instrument inst $ line melody

seqToScale seq scale oct = map ((+(12*oct)) . (scale !!)) seq

rhythm1 = en : (replicate 4 sn)

marryRhythm = zipWith note 

pattern1 :: Music Pitch
pattern1 =
  let pitches = map pitch $ seqToScale seq1 dMinor 3
      rhythm = rhythm1
  in instrument Lead8BassLead $ line $ zipWith note (cycle rhythm) pitches

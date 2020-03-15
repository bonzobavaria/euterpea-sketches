module Composition.Compositions (
  t251,
  composition,
  wholeToneComp,
  wholeTonePercComp,
) where
-- Don't forget to start qjackctl and connect midi through to fluidsynth!
import Euterpea

import qualified Composition.Percussion as Perc
import qualified Utils.Utils as Utils

-- Cool stuff from Ch. 4
t251 :: Music Pitch
t251 = let dMinor = d 4 qn :=: f 4 qn :=: a 4 qn
           gMinor = g 4 qn :=: bf 4 qn :=: d 4 qn
       in dMinor :+: gMinor

-- Create a whole tone scale
wts :: Pitch -> [Music Pitch]
wts p = let f ap = note (1/32) (pitch (absPitch p + ap))
        in map f [0, 2, 4, 6, 8, 10]

-- example
wholeToneComp = 
  let scales = take 8 $ iterate (+4) 64 
      pattern = (concatMap (wts . pitch) scales)
      comp = pattern ++ (reverse pattern)
  in line $ map (instrument RhodesPiano) comp

wholeTonePercComp =
  (forever wholeToneComp) :=: Perc.groove

--abs1 = take 12 $ iterate (+5) 32
abs1 = take 16 $ iterate (\x -> if even x then x + 7 else x - 5) 32
abs2 = map (\x -> 127 - x) abs1

pitches1 = map pitch abs1
pitches2 = map pitch abs2
pitches3 = map (trans 7) pitches1

makeNotes ps = map (note en) ps

stream1 = map (instrument Celesta) $ makeNotes pitches1
stream2 = map (instrument OrchestralHarp) $ makeNotes pitches2
stream3 = map (instrument MusicBox) $ makeNotes pitches3

--composition = line $ (zipWith (:=:) stream1 stream3) ++ stream2
composition = line $ (zipWith (:=:) stream1 (reverse stream3))

-- Ch. 3 exercise
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = let as = map absPitch [p1, p2]
                  range = [(minimum as) .. (maximum as)]
                  order = if p1 < p2 then id else reverse
                  process = order . map (note qn) . map pitch
              in line $ process range

-- this will work if you specify scale as e.g. [0,2,3,5,7,8,10]
-- mkS p ints = line $ map (note qn) $ zipWith trans ints (repeat p)

-- take 7 $ iterate (trans 1) (C,4)
-- [(C,4),(Cs,4),(D,4),(Ds,4),(E,4),(F,4),(Fs,4)]

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p ints = let pitches = foldl (\acc x -> trans x (head acc) : acc) [p] ints
                     notify = reverse . map (note qn)
                 in line $ notify pitches

data ScaleName
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Lochrian

ionian = [2,2,1,2,2,2,1] :: [Int]

main = play $ forever composition
--main = play $ forever t251

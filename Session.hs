module Session where

import qualified Chords
import qualified Scales
import qualified Utils
import qualified Patterns
import Euterpea
import Data.Ratio ((%))

-- This is a C Dorian Scale, though it begs a refactoring to include the pitch class when making a scale
notes :: [AbsPitch]

notes = Utils.mkScale Scales.lydian

pat = Patterns.tertian 4
p1 = Utils.unravel $ reverse [[0,7],pat,[0..3] ++ [4,3..1],[0,4]]
thing = Utils.sequence notes 5 p1 
m1 = Utils.melody [sn] thing

-- sereneKoto :: Clip
c1 = Utils.clip Lead1Square m1

p2 = [0,4,2,5]
t2 = Utils.sequence notes 4 p2 
m2 = Utils.melody [(1/1)] t2

c2 = Utils.clip Pad4Choir m2

scene1 = c1 :=: (forever c2)

c3 = invert c1

-- Weird idea: you could use mMap to recursively coerce a Music Pitch to a scale by shifting non-scale conforming values down a half-step on each pass until all values conform to the scale
--octaves = mMap (\(p,o) -> if p == C then (

-- Inclusive arp
arpInc :: [Int] -> [Int]
arpInc x = x ++ (reverse x)

-- Exclusive arp
arpEx x = init x ++ (init $ reverse x)

q = Utils.cheat Lead1Square notes ([sn,sn] ++ (replicate 6 $ 1 % 24) ++ [sn,sn])  5

clip2 = q $ arpEx Scales.dorian
linksAdventure = q $ concatMap (\x -> [0,0] ++ (map (+ (x + 1)) $ take 6 $ arpEx $ Patterns.tertian 4) ++ [0,0]) $ [0..7]

q1 = Utils.cheat SynthBass1 notes [qn] 3

nSet :: Utils.NoteSet  
nSet = Utils.extend Scales.dorian

motif :: Utils.Motif
--motif = take 32 $ cycle $ arpEx (Patterns.quintal 5)
motif = [0..7]

pitches = Utils.render motif nSet (C,5) :: [Pitch]
rhythm = cycle [en] :: [Dur]

motif2 :: Utils.Motif
motif2 = let m = take 4 $ Utils.progress [0..15] 4 in (reverse m)

motif3 = [0,4,7]

-- Well anyway injecting probably isn't the nicest way to write a composition.
-- You'll probably be much happier with unravel, and rendering with a matching
-- rhythm
-- This will inject a motive into a sequence every n notes
inject :: [Pitch] -> Utils.Motif -> Utils.NoteSet -> Int -> [Pitch]
inject [] _ _ _ = []
inject (x:xs) motive nSet count
  | mod count 4 == 0 = (Utils.render motif nSet x) ++ (inject xs motive nSet (count + 1))
  | otherwise = [x] ++ (inject xs motive nSet (count + 1))

inject' [] _ _ = []
inject' (x:xs) motive count
  | mod count 4 == 0 = (map (\y -> trans y x) motive) ++ (inject' xs motive (count + 1))
  | otherwise = [x] ++ (inject' xs motive (count + 1))
-- TODO: How about an inject function that uses a conditional instead of a
-- counter?
-- TODO: Would it be nicer to only inject into [Music Pitch], instead of having
-- to build out a totally separate rhythm?

pitches2 = Utils.render motif2 nSet (C,6) :: [Pitch]

pitches033 = inject pitches motif3 nSet 1

song = zipWith note rhythm pitches :: [Music Pitch]
song2 = zipWith note (cycle [tn]) pitches2 :: [Music Pitch]


song3 :: Music Pitch 
song3 = line $ zipWith note (cycle $ concat [replicate 3 qn, replicate 3 (qn / 3)]) pitches033

-- TODO rename unravel as zoomIn?
key044 = Utils.extend Scales.dorian :: Utils.NoteSet
seq044 :: Utils.Motif
seq044 = concat $ map (\x -> [0,0,x,0]) $ Patterns.quintal 3
rhythm044 :: [Dur]
rhythm044 = concat [replicate 2 en,replicate 3 (en / 3),[en]]

ps044 = Utils.render seq044 key044 (C,4)

song044 :: Music Pitch
song044 = line $ zipWith note (cycle rhythm044) ps044



















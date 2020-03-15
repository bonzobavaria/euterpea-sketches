module Composition.Session (
    linksAdventure,
    song3,
    song044
) where

import qualified Utils.Chords as Chords
import qualified Utils.Scales as Scales
import qualified Utils.Utils as Utils
import qualified Utils.Patterns as Patterns
import Euterpea
import Data.Ratio ((%))

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

pitches2 = Utils.render motif2 nSet (C,6) :: [Pitch]

pitches033 = Utils.inject pitches motif3 nSet 1

song = zipWith note rhythm pitches :: [Music Pitch]
song2 = zipWith note (cycle [tn]) pitches2 :: [Music Pitch]

song3 :: Music Pitch 
song3 = line $ zipWith note (cycle $ concat [replicate 3 qn, replicate 3 (qn / 3)]) pitches033

-- Song 044

key044 = Utils.extend Scales.dorian :: Utils.NoteSet
seq044 :: Utils.Motif
seq044 = concat $ map (\x -> [0,0,x,0]) $ Patterns.quintal 3
rhythm044 :: [Dur]
rhythm044 = concat [replicate 2 en,replicate 3 (en / 3),[en]]

ps044 = Utils.render seq044 key044 (C,4)

song044 :: Music Pitch
song044 = line $ zipWith note (cycle rhythm044) ps044

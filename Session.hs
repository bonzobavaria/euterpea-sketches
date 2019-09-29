module Session where

import qualified Chords
import qualified Scales
import qualified Utils
import qualified Patterns
import Euterpea

-- This is a C Dorian Scale, though it begs a refactoring to include the pitch class when making a scale
notes :: [AbsPitch]
notes = Utils.mkScale Scales.dorian 

-- tertian :: [Integer], but we have to have [Int] to satify [AbsPitch]
pat = map fromIntegral $ Patterns.tertian 4
p1 = Utils.unravel $ reverse [[0,7],pat,[0..3] ++ [4,3..1],[0,4]]
thing = Utils.seqToScale notes 5 p1 
melody = Utils.mel [sn] thing

-- sereneKoto :: Clip
c1 = Utils.clip Lead1Square melody

p2 = [0,4,2,5]
t2 = Utils.seqToScale notes 4 p2 
m2 = Utils.mel [(1/1)] t2

c2 = Utils.clip Pad4Choir m2

scene1 = c1 :=: (forever c2)

c3 = invert c1

-- Weird idea: you could use mMap to recursively coerce a Music Pitch to a scale by shifting non-scale conforming values down a half-step on each pass until all values conform to the scale
--octaves = mMap (\(p,o) -> if p == C then (

-- Inclusive arp
arpInc x = x ++ (reverse x)

-- Exclusive arp
arpEx x = init x ++ (init $ reverse x)

--q = Utils.cheat Lead1Square notes [sn] 5

--seq2 = q $ arpEx Scales.dorian

module Composition.Percussion (
  beat,
  groove
) where

import Euterpea
import Data.Ratio ((%))

-- Rock beat
-- Maybe a clip shouldn't call forever on itself theoretically, but then I'll
-- need a way to express independent polyrhythmic lines so that they fill the
-- available space.
groove :: Music Pitch
groove =
  let bd = perc BassDrum1 (1/16) 
      p2 = perc RideCymbal1 (3/16) 
      sn = perc ElectricSnare (1/8)
      p4 = perc SplashCymbal (1/4)
      bass = (bd :+: bd :+: snr :+: bd :+:
              bd :+: bd :+: snr :+: bd :+:
              snr :+: bd :+: snr :+: bd :+:
              bd :+: bd :+: snr :+: bd
             )
      snare = qnr :+: sn :+: enr
      ride = times 5 p2 :+: snr
      splash = p4
  in chord $ map forever [bass,snare,ride,splash]

-- Driving bass rhythm
beat :: Music Pitch
beat =
  let r = replicate
      rhythm = (r 4 sn) ++ 
               (r 2 ((en/3))) ++
               (r 1 ((en/3) * 2)) ++
               (r 2 (en/3)) ++
               (r 2 sn) ++
               (r 4 tn) ++
               (r 5 (en/5)) ++
               (r 2 sn) 
  in line $ map (perc LowWoodBlock) rhythm
  


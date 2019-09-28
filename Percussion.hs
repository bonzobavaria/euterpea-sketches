module Percussion (
  groove
) where

import Euterpea

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

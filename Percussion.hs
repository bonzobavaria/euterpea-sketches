module Percussion (
  groove
) where

import Euterpea

-- Rock beat
groove :: Music Pitch
groove =
  let bd = perc BassDrum1 (1/16) 
      p2 = perc RideCymbal1 (3/16) 
      sn = perc ElectricSnare (1/8)
      p4 = perc SplashCymbal (1/4)
  in (forever
     $ (bd :+: bd :+: snr :+: bd :+:
        bd :+: bd :+: snr :+: bd :+:
        snr :+: bd :+: snr :+: bd :+:
        bd :+: bd :+: snr :+: bd
       ))
     :=:
     (forever
     $ (times 5 p2 :+: snr))
     :=:
     (forever
     $ (qnr :+: sn :+: enr))
     :=:
     (forever $ p4)

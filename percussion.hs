import Euterpea

-- Rock beat
groove :: Music Pitch
groove =
  let p1 = perc BassDrum1 (1/16) 
      p2 = perc RideCymbal1 (3/16) 
      p3 = perc ElectricSnare (1/8)
      p4 = perc SplashCymbal (1/4)
  in (forever
     $ (p1 :+: p1 :+: snr :+: p1 :+:
        p1 :+: p1 :+: snr :+: p1 :+:
        snr :+: p1 :+: snr :+: p1 :+:
        p1 :+: p1 :+: snr :+: p1
       ))
     :=:
     (forever
     $ (times 5 p2 :+: snr))
     :=:
     (forever
     $ (qnr :+: p3 :+: enr))
     :=:
     (forever $ p4)
  

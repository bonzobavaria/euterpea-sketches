-- Don't forget to start qjackctl and connect midi through to fluidsynth!
import Euterpea

t251 :: Music Pitch
t251 = let dMinor = d 4 qn :=: f 4 qn :=: a 4 qn
           gMinor = g 4 qn :=: bf 4 qn :=: d 4 qn
       in dMinor :+: gMinor

wts :: Pitch -> [Music Pitch]
wts p = let f ap = note qn (pitch (absPitch p + ap))
        in map f [0, 2, 4, 6, 8]

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

-- Ch. 3 exersize
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = let as = map absPitch [p1, p2]
                  range = [(minimum as) .. (maximum as)]
                  order = if p1 < p2 then id else reverse
                  process = order . map (note qn) . map pitch
              in line $ process range


mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p ints = let f acc p [] = acc
                     f acc p (x:xs) = f (p : acc) (trans x p) xs 
                     process = reverse . map (note qn)
                 in line $ process $ f [] p ints




main = play $ forever composition
--main = play $ forever t251

module Nuage where

import Euterpea

import qualified Utils.Utils as Utils
import qualified Utils.Scales as Scales

-- Length of 10 bars matches length of waves part
strings =
    let
        inst = StringEnsemble1
        mkNote = map $ note (wn * 2)
        harmony = line $ map chord $ map mkNote
            [ [(C, 2), (G, 2), (E, 3)]
            , [(A, 2), (E, 3), (F, 3)]
            , [(B, 2), (C, 3), (A, 3)]
            , [(G, 1), (F, 2), (C, 3)]
            , [(B, 2), (C, 3), (A, 3)]
            ] :: Music Pitch
    in instrument inst harmony

tinks =
    let 
        rhythm = concat
            [ replicate 3 qn
            , replicate 3 (qn / 3)
            , replicate 2 qn
            ]
    in line $ (map (perc MuteTriangle) rhythm) ++ [rest (6 / 2)]

bassDrum = perc AcousticBassDrum (2 / 1)

flutePart =
    let
        inst = Flute
        notes =
                rest qn
            :+: note dhn (F, 5) 
            :+: note hn  (E, 5) 
            :+: note 2   (G, 5) 
            :+: rest hn
            :+: note hn  (B, 5) 
            :+: note dhn (C, 5) 
            :+: note wn  (D, 5) 
            :+: note wn  (G, 5) 
    in instrument inst notes

-- 10 Bars / 4 repetions
waves =
    let
        inst = Viola
        notes = Utils.mkScale' C Scales.major :: Utils.NoteSet
        motif = Utils.unravel [[0..4], [3,2..0]] :: Utils.Motif
        rhythm = repeat en
        renderedMotif = Utils.render motif notes (C, 4)
        mel = Utils.melody rhythm renderedMotif
    in Utils.clip inst mel

currents =
    let
        inst = OrchestralHarp
        notes = Utils.mkScale' C Scales.major
        -- 32 notes, 40 qn duration = 10 bars / repitition
        motif = Utils.unravel [[0, 2, 3, -1], [0, 3, 2, -1], [4, 0]]
        rhythm = cycle [qn, qn, qn, hn]
        renderedMotif = Utils.render motif notes (B, 4) :: [AbsPitch]
        mel = Utils.melody rhythm renderedMotif :: [Music Pitch]
    in  instrument inst $ line mel

composition = 
    -- TODO: Create a bpm function to use with tempo
    tempo (4 / 5) $ 
            (times 4 strings)
        :=: (offset 10 $ times 8 waves) 
        :=: (offset 30 $ times 2 currents)
        :=: (offset 30 $ times 2 tinks)
        :=: (offset 30 $ times 5 bassDrum)
        :=: (offset 17 $ flutePart :+: rest 4 :+: flutePart)

main = do
    writeMidi "Nuages.mid" composition 

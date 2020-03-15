module Utils.Chords where

-- These chords are meaningful in the context of a 12-tone scale, while patterns like tertiary, quintal, and quartal are adaptive to the scale they're applied to.
-- Triads
maj =      [0,4,7]
min =      [0,3,7]
dim =      [0,3,6]
-- Extended harmony
maj7 =     [0,4,7,11]
min7 =     [0,3,7,10]
dim7 =     [0,3,6,9]
halfDim7 = [0,3,6,10]
dom7 =     [0,4,7,10]
-- Ninth chords are distinguished from added-ninth chords by the presence or
-- absence of the seventh.
ninth =    [0,4,7,10,14]
maj69 =    [0,4,7,9,14]
add9 =     [0,4,7,14]

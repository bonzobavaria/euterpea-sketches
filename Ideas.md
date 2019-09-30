## Motivation

Euterpea doesn't include all the features I want to work with notes in the way 
I want. So I'm building a utility library for it, which is separate from making 
compositions. My challenge will be to keep my compositions working correctly 
with my library!

## Infinite Note Sets

I want to be able to arpeggiate mulitple octaves of a chord, or any pattern 
that fits within a single octave. So I should be able to `take 8 maj7` to get 
two octaves of a major 7 chord, which is what I've already done for scales. So 
every chord and scale should be considered to spread across the entire range of 
MIDI notes for every user interaction.

These two functions for making scales need to be combined into one, and should 
be perfectly useful for getting a **note set** across the range of MIDI notes 
for any scale or chord. I should be able to call `nSet D hirajoshi` or `nSet Fs 
maj7`.

Maybe the most Euterpean way to get these scales is to convert my relational 
scale into a `[PitchClass]`, then get a `[(PitchClass,Octave)]` that spans 10 
octaves. But then I'd end up converting those back to `absPitch`es to use them.  
And also the root wouldn't be the head of my list.

``` haskell
-- Ex: scale D major = map (+2) major
scale :: PitchClass -> Scale -> Scale
scale key sc = map (+(absPitch(key,-1))) sc

-- get a list of every possible AbsPitch for a scale
mkScale :: Scale -> [AbsPitch]
mkScale scale = 
  let everyNote = concat $ take 11 $ iterate (map (+12)) scale
  in takeWhile (<= 127) everyNote
```

Now I'm thinking that a `NoteSet` should be totally independent of 
transposition to a key.

``` haskell
extend :: [Int] -> NoteSet
extend = (takeWhile (<= 127)) . concat . (iterate (map (+12)))
```

Then a melodic sequence can be **rendered** into a list of type `[Pitch]` by
a function that is applied to the melodic sequence and a starting pitch. That
would be after the function that zips `Motifs` into `NoteSets`. So I guess a
`Sequence` is a `Motif` plus a `NoteSet`. I can probably define a **semigroup**
to make that workflow easier.

One **huge** problem is that this workflow doesn't seem to allow negative 
motivic values, which it totally should, so I need to think more carefully 
about how `Motif`s and `NoteSets` get combined.

If swapping notesets is very easy, then the need for chords would be hugely 
reduced, or chords could be seen as ordinary notesets. 

So the way to combine different notesets and stuff is to get a **sequence** 
(`[Pitch]`) out of of a single noteset, then create new **motifs**, and target 
the desired areas of the extant sequence to **map them onto**. So a motif could 
render a dorian sequence, then I could create another motif and choose a scale 
for it. By mapping over the first sequence I can provide notes in that sequence 
as render targets.

So how can I insert lists into other lists?

## Consistent Octave

The `Octave` type should be used consistently between my utils and Euterpea. In 
Euterpea MIDI note 0 is `(C,-1)`.

## Iterating through note sets

I want to be able to **strum** though a **note set**, so that for a list of 
`[0,4,7,11,12,16]`, I need a function to create 
`[0,4,7,4,7,11,7,11,12,11,12,16]`. I would be great to be able to specify both 
**step** and **jump** values, similar to the `disjunction` function in 
`Patterns.hs`, and specifying an index on every iteration. Is this what 
**lenses** are for?

``` haskell
-- Get a 2-octave maj7 chord
l = take 8 $ concat $ iterate (map (+12)) [0,4,7,11]
-- Get a list of iterations
p = map (\x -> take 3 $ dropWhile (< x) l) l
concat $ takeWhile (\x -> length x >= 3) p

```

## Conflicting methods

Right now I'm working on mapping motifs onto scales and chords. I need to 
evaluate how fluidly those scales and chords can change, and if fluidity in 
that matter is what I want. How easily could I create a passage consisting of 

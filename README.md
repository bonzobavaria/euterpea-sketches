# Euterpea

## How to start

Start fluidsynth with your `startfluidsynth` script.
Start `qjackctl`, and connect midi through to fluidsynth.
Test fluidsynth with `noteon 1 60 120`.
Now open `ghci` and `import Euterpea`.
Try `play $ note qn (pitch 64)`.

## File Heirarchy

**Chords** and **scales** are just descriptions of numerical patterns and
shouldn't need to import other files or Euterpea. Utils can make use of that
info and also import Euterpea. 

## Composition Types

Patterns are organized in layers to make it simple to move from primitive
numerical patters to complete compositions as follows:

+ **Patterns**: Simple numerical patterns describing numerical relationships in
  a pre-musical form
+ **Sequences**: Patterns applied to a scale, but without rhythm
+ **Melodies**: Sequences with rhythm
+ **Clips**: Melodies that specify an instrument
+ **Scenes**: Arrangements of clips
+ **Compositions**: Arrangements of scenes

This workflow resembles working with something like Ableton Live, but has a
shortcoming in that it encouraged block and loop-based composition and
discourages melodic independence and unclearly separated musical sections. 

## Types in Euterpea

## Pitch vs absPitch

An AbsPitch is a midi note, like `64`. 

`type AbsPitch = Int`

A Pitch is the form you will usually consume, and has the type:

`type Pitch = (PitchClass, Octave)`

The `pitch`, and `absPitch` functions convert midi note (Int) pitches to Pitch
and vice versa.

## Types

A composition that you can play using the `play` function is of the type `Music a`, which is a recursive data type defined as:

``` Haskell
data Music a
  = Prim (Primitive a)
  | (Music a) :+: (Music a)
  | (Music a) :=: (Music a)
  | Modify Control (Music a)
```

So to get a `Music a`, you just need to make a `Primitive a`:

`data Primitive a = Note Dur a | Rest Dur`

and wrap it with `Prim`.

Examples: 

`Prim (Note (1/32) (Gs,4))`

`Prim (note qn (C,4))`

These examples are both of type `Music Pitch`

You can concatenate `Music a` values with `:+:` or layer them with `:=:` and
still have a `Music a`, since `Music a` is a recursive data type.

Often you'll end up with something of type `[Music Pitch]`, which is just a
bunch of notes that you need to concatenate into a `Music Pitch`. This is what
the `line` function is good for.

`line :: [Music a] -> Music a`

It's equivalent to `foldr (:+:) (rest 0)`.

Note that `Music a` is an instance of `ToMusic1 a`, which is required for it to
be played as MIDI or written to a MIDI file.

# Euterpea

## How to start

Start fluidsynth with your `startfluidsynth` script.
Start `qjackctl`, and connect midi through to fluidsynth.
Test fluidsynth with `noteon 1 60 120`.
Now open `ghci` and `import Euterpea`.
Try `play $ note qn (pitch 64)`.

## Pitch vs absPitch

An AbsPitch is a midi note, like `64`. 

`type AbsPitch = Int`

A Pitch is the form you will usually consume, and has the type:

`type Pitch = (PitchClass, Octave)`

The `pitch`, and `absPitch` functions convert midi note (Int) pitches to Pitch
and vice versa.

## Types

A composition that you can play using the `play` function is of the type `Music
a`, which is a recusive data type defined as:

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

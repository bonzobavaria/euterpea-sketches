# Euterpea

## How to start

Start fluidsynth with your `startfluidsynth` script.
Start `qjackctl`, and connect midi through to fluidsynth.
Test fluidsynth with `noteon 1 60 120`.
Now open `ghci` and `import Euterpea`.
Try `play $ note qn (pitch 64)`.

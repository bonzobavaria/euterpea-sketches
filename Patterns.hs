module Patterns where

-- These patterns express disjunct melodies across an
-- established scale, unlike scales, which are independent of
-- a scale, or meaningful in the context of the 12-tone
-- chromatic scale.

disjunction :: Int -> Int -> [Int]
disjunction step len = take len $ iterate (+ step) 0

-- These names look a little confusing, but that's just
-- because we're using zero-indexing, so a fourth is 3, fifth
-- is 4 etc.
tertian = disjunction 2
quintal = disjunction 3
quartal = disjunction 4

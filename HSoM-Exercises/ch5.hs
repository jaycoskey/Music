-- Haskell School of Music, Chapter 5

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 5.1 =====
-- Ex: Defined a function twice, for which (twice (+1)) 2 => 4

-- Code for this exercise:
twice :: (a -> a) -> a -> a
twice f = foldr (.) id (replicate 2 f)
addTwo = twice (+1)

-- so (twice twice) applies twice two times to its argument
addFour      = (twice twice)         (+1)
addSixteen_a = (twice twice twice)   (+1)
addSixteen_b = (twice (twice twice)) (+1)

test_5_1_a = (addTwo 2)       == 4
test_5_1_b = (addFour 2)      == 6
test_5_1_c = (addSixteen_a 2) == 18
test_5_1_d = (addSixteen_b 2) == 18

test_5_1   = and [test_5_1_a, test_5_1_b, test_5_1_c, test_5_1_d]

-- ===== Exercise 5.2 =====

-- Part 1: Defined a function power, for which  power (+2) 5 1 => 11
power :: (a -> a) -> Int -> a -> a
power f n = foldr (.) id (replicate n f)

-- Part 2: Use power in a musical context to define something useful.
squish :: Music a -> Music a
squish mus = Modify (Tempo 2) mus :+: Modify (Tempo 2) mus

closeEncounters = line $ [g 3 qn, a 3 qn, f 3 qn, f 2 qn, c 3 qn]

music_5_2_a = closeEncounters
music_5_2_b = squish closeEncounters
music_5_2_c = power squish 3 closeEncounters

test_5_2_a = play music_5_2_a  -- Test: Close Encounters
test_5_2_b = play music_5_2_b  -- Test: Close Encounters twice
test_5_2_c = play music_5_2_c  -- Test: Close Encounters many times.

-- ===== Exercise 5.3 =====
-- Ex: Part 1: What is the principal type of fix?
--     Part 2: Rewrite it so it's not recursive.
--     Part 3: Do you think this process can be applied to any recursive function?

-- Note: See the post "You could have re-invented fix too!",
--     at http://www.vex.net/~trebla/haskell/fix.xhtml

-- Code from this exercise
fix f = f (fix f)

-- Part 1:
-- fix f = f x 
--   where x = fix f
-- So the type of fix f is the type of f x
-- The output type of f matches its input type, so f :: a -> a, and
-- fix :: (a -> a) -> a

-- Code from HSoM, Chapter 5:
remainder :: Integer -> Integer -> Integer
remainder n d = if n < d then n
                else remainder (n- d) d

-- Part 2: Code for this exercise:
rem_5_3 = fix rem
  where rem remF n d = if n < d then n
                       else remF (n - d) d

test_5_3_a = rem_5_3 20 10 == 0
test_5_3_b = rem_5_3 21 10 == 1
test_5_3_c = rem_5_3 29 10 == 9

test_5_3   = and [test_5_3_a, test_5_3_b, test_5_3_c]

-- Part 3: Can all recursive functions be rewritten using fix?
-- I believe the answer is yes, with the possible exception of polymorphic recursion.

-- ===== Exercise 5.4 =====

-- Part 1
apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs ap1s ap2s = [(ap1, ap2) | ap1 <- ap1s, ap2 <- ap2s, ap1 - ap2 > 2, ap1 - ap2 < 8]

-- Part 2
notePairMusic :: [AbsPitch] -> [AbsPitch] -> Music Pitch
notePairMusic ap1s ap2s = foldr1 (:+:) $ map apPair2Chord pairs
  where pairs = apPairs ap1s ap2s
        mkEn = \ap -> Prim $ Note en $ pitch ap
        apPair2Chord :: (AbsPitch, AbsPitch) -> Music Pitch
        apPair2Chord (ap1, ap2) = chord $ map mkEn [ap1, ap2]

-- Test music
aps_5_4_a = [30, 32 .. 60]
aps_5_4_b = [15, 18 .. 61]
apPairs_5_4 = apPairs       aps_5_4_a aps_5_4_b
music_5_4   = notePairMusic aps_5_4_a aps_5_4_b

-- Test code
test_5_4 = play music_5_4

-- ===== Exercise 5.5 =====
-- Ex: Use currying to eliminate the duration argument in the definition of hList.
-- TODO: Factor out duration argument from hList_d.

-- Code from HSoM, Chapter 5:
hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p : ps) = hNote d p :+: hList d ps

-- Part 2: Code for this exercise:
-- Duration argument shifted to final position.
hList_b :: [Pitch] -> Dur -> Music Pitch
hList_b []       d = rest 0
hList_b (p : ps) d = (:+:) (hNoteFlip p d) (hList_b ps d)
  where hNoteFlip = flip hNote

-- Duration argument appears only once on right-hand side.
hList_c :: [Pitch] -> Dur -> Music Pitch
hList_c []       = const $ rest 0
hList_c (p : ps) = \d -> foldr1 (:+:) $ (flip map) [hNoteFlip p, hList_c ps] ($ d)
  where hNoteFlip = flip hNote

-- A non-recursive version (though line is implemented as a recursive function)
hList_d :: [Pitch] -> Dur -> Music Pitch
hList_d [] = \_ -> rest 0
hList_d ps = \d -> line $ (flip map) (map hNoteFlip  ps) ($ d) 
  where hNoteFlip :: Pitch -> Dur -> Music Pitch
        hNoteFlip = flip hNote

-- Test code
-- Test: The following four phrases should sound exactly the same.
test_5_5_a = play $ hList   qn [(C,4), (E,4), (G,4)]
test_5_5_b = play $ hList_b    [(C,4), (E,4), (G,4)] qn
test_5_5_c = play $ hList_c    [(C,4), (E,4), (G,4)] qn
test_5_5_d = play $ hList_d    [(C,4), (E,4), (G,4)] qn

-- ===== Exercise 5.6 =====
-- Ex: Use line, map, and ($) to give a concise definition of addDur.

-- Code for this exercise:
-- Compare with the function repd in the answer to Exercise 4.1.
addDur d = \ns -> line $ map (\x -> x d) ns

-- Test music
music_5_6 = addDur qn [c 4, e 4, g 4]

-- Test code
test_5_6 = play music_5_6  -- Plays C Major chord notes

-- ===== Exercise 5.7 =====
-- Ex: Rewrite this example
--         map (\x -> (x + 1) / 2) xs
--     using a composition of sections.

-- Code from this exercise:
f_5_7_a  = \x -> (x + 1) / 2
-- Code for this exercise:
f_5_7_b = (/2) . (+1)

-- Test code
test_5_7_a = map f_5_7_a [9, 19, 29] == [5.0, 10.0, 15.0]
test_5_7_b = map f_5_7_b [9, 19, 29] == [5.0, 10.0, 15.0]

-- Should yield True, even though it's dangerous to compare Floats.
test_5_7   = and [test_5_7_a, test_5_7_b]

-- ===== Exercise 5.8 =====
-- Part 1: Rewrite the expression
--             map f (map g xs)
--        using function composition and a single call to map.
-- Answer: f_5_8_a = map (f . g) xs

-- Part 2: Then rewrite the expression
--             map (\x -> (x + 1) / 2) xs
--        as a map of maps.
f_5_8_b = (map (/2)) . (map (+1))

-- Test code
test_5_8_b = f_5_8_b [9, 19, 29] == [5.0, 10.0, 15.0]

-- ===== Exercise 5.9 =====
-- Ex: Go back to any exercises prior to this chapter, and simplify them using ideas learned here.

-- TODO: Don't see any opportunities there, but perhaps f1_5_10, below, could be simpler.

-- ===== Exercise 5.10 =====
-- Ex: Using higher-order functions introduced in this chapter,
--     fill in the two missing functions, f1 and f2, in the evaluation below:
--         f1 (f2 (*) [1,2,3,4]) 5 => [5, 10, 15, 20]

-- Code for this exercise:
f1_5_10 = \fs x -> map ($ x) fs
f2_5_10 = map
eval_5_10 f1 f2 = f1 (f2 (*) [1,2,3,4]) 5
result_5_10     = eval_5_10 f1_5_10 f2_5_10

test_5_10 = result_5_10 == [5, 10, 15, 20]

-- Haskell School of Music, Chapter 5

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 5.1 =====
-- (twice (+1)) 2 => 4
twice :: (a -> a) -> a -> a
twice f = foldr (.) id (replicate 2 f)
addTwo = twice (+1)
-- addTwo 2 => 4

-- so (twice twice) applies twice two times to its argument
addFour     = (twice twice)         (+1)
addSixteen  = (twice twice twice)   (+1)
addSixteen' = (twice (twice twice)) (+1)

-- ===== Exercise 5.2 =====
-- power (+2) 5 1 => 11
power :: (a -> a) -> Int -> a -> a
power f n = foldr (.) id (replicate n f)

-- Use power in a musical context to define something useful.
squish :: Music a -> Music a
squish mus = Modify (Tempo 2) mus :+: Modify (Tempo 2) mus

closeEncounters = line $ [g 3 qn, a 3 qn, f 3 qn, f 2 qn, c 3 qn]
-- play $ closeEncounters
-- play $ squish closeEncounters
-- play $ power squish 3 closeEncounters

-- ===== Exercise 5.3 =====
-- Note: See the post "You could have re-invented fix too!",
--     at http://www.vex.net/~trebla/haskell/fix.xhtml

fix f = f (fix f)
-- fix f = f x 
--   where x = fix f
-- So the type of fix f is the type of f x
-- The output type of f matches its input type, so f :: a -> a, and
-- fix :: (a -> a) -> a

remainder :: Integer -> Integer -> Integer
remainder n d = if n < d then n
                else remainder (n- d) d

remainder' = fix rem
  where rem remF n d = if n < d then n
                       else remF (n - d) d

test_5_3_a = remainder  23 10
test_5_3_b = remainder' 23 10

-- ===== Exercise 5.4 =====
apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs ap1s ap2s = [(ap1, ap2) | ap1 <- ap1s, ap2 <- ap2s, ap1 - ap2 > 2, ap1 - ap2 < 8]

notePairMusic :: [AbsPitch] -> [AbsPitch] -> Music Pitch
notePairMusic ap1s ap2s = foldr1 (:+:) $ map apPair2Chord pairs
  where pairs = apPairs ap1s ap2s
        mkEn = \ap -> Prim $ Note en $ pitch ap
        apPair2Chord :: (AbsPitch, AbsPitch) -> Music Pitch
        apPair2Chord (ap1, ap2) = chord $ map mkEn [ap1, ap2]

music_5_4 = notePairMusic [30, 32 .. 60] [15, 18 .. 61]
-- play music_5_4

-- ===== Exercise 5.5 =====
-- Is there a way to use currying to eliminate the duration argument in the definition of hList?
hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p : ps) = hNote d p :+: hList d ps

-- Duration argument shifted to final position.
hList_b :: [Pitch] -> Dur -> Music Pitch
hList_b []       d = rest 0
hList_b (p : ps) d = (:+:) (hNoteFlip p d) (hList_b ps d)
  where hNoteFlip = flip hNote

-- Duration argument appears only once on right-hand side.
-- TODO: Factor out duration argument from hList_c.
hList_c :: [Pitch] -> Dur -> Music Pitch
hList_c []       _ = rest 0
hList_c (p : ps) d = foldr1 (:+:) $ (flip map) [hNoteFlip p, hList_c ps] ($ d)
  where hNoteFlip = flip hNote

-- hList_d

test_5_5_a = hList   qn [(C,4), (E,4), (G,4)]
test_5_5_b = hList_b    [(C,4), (E,4), (G,4)] qn
test_5_5_c = hList_c    [(C,4), (E,4), (G,4)] qn
-- test_5_5_d = hList_d    [(C,4), (E,4), (G,4)] qn

-- ===== Exercise 5.6 =====
-- Use line, map, and ($) to give a concise definition of addDur.
addDur d = \ns -> line $ map (\x -> x d) ns
-- See the function repd in the answer to Exercise 4.1.

-- ===== Exercise 5.7 =====
-- Rewrite this example
--     map (\x -> (x + 1) / 2) xs
-- using a composition of sections.
f_5_7_a  = \x -> (x + 1) / 20
f_5_7_b = (/2) . (+1)
-- map f_5_7_a [9, 19, 29] => [5.0, 10.0, 15.0]
-- map f_5_7_b [9, 19, 29] => [5.0, 10.0, 15.0]

-- ===== Exercise 5.8 =====
-- Rewrite the expression
--     map f (map g xs)
-- using function composition and a single call to map.
-- Then rewrite the expression
--     map (\x -> (x + 1) / 2) xs
-- as a map of maps.
f_5_7_c = (map (/2)) . (map (+1))

-- ===== Exercise 5.9 =====
-- Go back to any exercises prior to this chapter, and simplify them using ideas learned here.
-- Don't see any opportunities there, but perhaps f1_5_10, below, could be simpler.

-- ===== Exercise 5.10 =====
-- Using higher-order functions introduced in this chapter,
-- fill in the two missing functions, f1 and f2, in the evaluation below:
--     f1 (f2 (*) [1,2,3,4]) 5 => [5, 10, 15, 20]
testfunc_5_10 f1 f2 = f1 (f2 (*) [1,2,3,4]) 5
f1_5_10 = \fs x -> map ($ x) fs
f2_5_10 = map
testfunc_5_10_result = testfunc_5_10 f1_5_10 f2_5_10
-- testfunc_5_10_result => [5, 10, 15, 20]

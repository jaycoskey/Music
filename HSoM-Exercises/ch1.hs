-- Haskell School of Music, Chapter 1

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 1.1 =====
-- simple (simple 2 3 4) 5 6
--     = simple (2 * (3 + 4)) 5 6  -- unfold inner function definition
--     = simple (2 * 7) 5 6
--     = simple 14 5 6
--     = 14 * (5 + 6)              -- unfold other function definition
--     = 14 * 11
--     = 154

-- ===== Exercise 1.2 =====
-- simple (a - b) a b
--     = (a - b) * (a + b)
--     = a^2 + a * b - a * b - b^2
--     = a^2 - b^2

-- ===== Exercise 1.3 =====
-- [A, B, C] :: [PitchClass]
-- [D, 42]   -- Not well-typed: PitchClass and numeric literal have incompatible types.
-- (-42, Ef) -- Not well-typed, for previous reason.
-- [('a', 3), [('b', 5)] :: [(Char, Int)]
--    Actually, more generally, it has type Num a => [(Char, a)]
-- simple 'a' 'b' 'c'  -- Not well-typed
-- (simple 1 2 3, simple) :: Num a => (a, a -> a -> a -> a)
-- ["I", "love", "Euterpea"] :: [String] or [[Char]]

-- ===== Exercise 1.4 =====
-- Ex: Modify the defs of hNote and hList so they each take an interval argument

p1 = (C, 4)
p2 = (E, 4)
p3 = (G, 4)

-- Code from HSoM, Chapter 1:
hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p : ps) = hNote d p :+: hList d ps

mel :: Music Pitch
mel = hNote qn p1 :+: hNote qn p2 :+: hNote qn p3

-- Code for this exercise
hNote_trans :: Dur -> Pitch -> Int -> Music Pitch
hNote_trans d p n = note d p :=: note d (trans n p)

hList_trans :: Dur -> [Pitch] -> Int -> Music Pitch
hList_trans d [] n = rest 0
hList_trans d (p : ps) n = hNote_trans d p n :+: hList_trans d ps n

-- Test music
mel_trans :: Music Pitch
mel_trans = hNote_trans qn p1 (-3) :+: hNote_trans qn p2 (-3) :+: hNote_trans qn p3 (-3)

mel_list :: Music Pitch
mel_list = hList qn [p1, p2, p3]

mel_list_trans :: Music Pitch
mel_list_trans = hList_trans qn [p1, p2, p3] (-3)

music_1_4 = mel :+: hnr :+: mel_trans :+: hnr :+: mel_list :+: hnr :+: mel_list_trans

-- Test code
test_1_4 = play music_1_4  -- Test: All four phrases should sound identical.

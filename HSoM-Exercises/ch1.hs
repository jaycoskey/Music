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
hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hNoteTrans :: Dur -> Pitch -> Int -> Music Pitch
hNoteTrans d p n = note d p :=: note d (trans n p)

p1 = (C, 4)
p2 = (E, 4)
p3 = (G, 4)

mel :: Music Pitch
mel = hNote qn p1 :+: hNote qn p2 :+: hNote qn p3

melTrans :: Music Pitch
melTrans = hNoteTrans qn p1 (-3) :+: hNoteTrans qn p2 (-3) :+: hNoteTrans qn p3 (-3)

testTrans = mel :+: wnr :+: melTrans
-- play testTrans

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p : ps) = hNote d p :+: hList d ps

hListTrans :: Dur -> [Pitch] -> Int -> Music Pitch
hListTrans d [] n = rest 0
hListTrans d (p : ps) n = hNoteTrans d p n :+: hListTrans d ps n

melList :: Music Pitch
melList = hList qn [p1, p2, p3]

melListTrans :: Music Pitch
melListTrans = hListTrans qn [p1, p2, p3] (-3)

testListTrans = mel :+: wnr :+: melTrans :+: wnr :+: melList :+: wnr :+: melListTrans
-- play testListTrans

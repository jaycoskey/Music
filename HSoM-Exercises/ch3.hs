-- Haskell School of Music, Chapter 3

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 3.1 =====
-- Code for this exercise:

-- Part 1: Transpose a series of pitches.
f1 :: Int -> [Pitch] -> [Pitch]
f1 n ps = map (trans n) ps

-- Part 2: Map durations to rests.
f2 :: [Dur] -> [Music a]
f2 ds = map rest ds

-- Part 3: Provide a staccato interpretation of the notes.
f3 :: [Music Pitch] -> [Music Pitch]
f3 [] = []
f3 (m : ms) = f3_impl(m) : (f3 ms)
  where
    f3_impl(m') = case m' of
        Prim (Note d p) -> Prim(Note (d / 2) p) :+: Prim(Rest (d / 2))
        Prim (Rest d)   -> Prim(Rest d)
        m1 :+: m2       -> f3_impl(m1) :+: f3_impl(m2)
        m1 :=: m2       -> f3_impl(m1) :=: f3_impl(m2)
        Modify mod m1   -> Modify mod (f3_impl m1)

-- Test music:
music_3_1_a = [c 4 qn , d 4 en, e 4 hn]
music_3_1_b = f3 music_3_1_a

-- Test code:
-- Test: The second phrase should sound like a staccato version of the first.
test_3_1 = play $ (line music_3_1_a) :+: hnr :+: (line music_3_1_b)

-- ===== Exercise 3.2 =====
-- Let flipf = flip f.  Then
-- flip (flip f) x y = flip flipf x y = flipf y x = flip f y x = f x y
-- So flip (flip f) = f

-- ===== Exercise 3.3 =====
-- xs = [1, 2, 3] :: [Integer]
-- ys = map (+) xs :: [Integer -> Integer]

-- ===== Exercise 3.4 =====
-- Ex. Define applyEach that applies a list of functions to a value,
--     resulting in a list.
--   For example, applyEach [simple 2 2, (+3)] 5 => [14, 8]
--                where simple x y z = x * (y + z)

-- Code from HSoM, Chapter 3:
simple x y z = x * (y + z)

-- Code for this exercise:
applyEach_a :: [a -> b] -> a -> [b]
applyEach_a fs x = map ($ x) fs

applyEach_b :: [a -> a] -> a -> [a]
applyEach_b [] x = []
applyEach_b (f : fs) x = (f x) : (applyEach_b fs x)

-- Test: Each of the following should yield True.
test_3_4_a = (applyEach_a [simple 2 2, (+3)] 5) == [14, 8]
test_3_4_b = (applyEach_a [simple 2 2, (+3)] 5) == [14, 8]

-- ===== Exercise 3.5 =====
-- Ex. Define applyEach that applies a list of functions to a value in turn,
--     resulting in a single value.
-- applyAll [simple 2 2, (+3)] 5 => 20
applyAll_a :: [a -> a] -> a -> a
applyAll_a fs = foldr (.) id fs

applyAll_b :: [a -> a] -> a -> a
applyAll_b [] x = x
applyAll_b (f : fs) x = f (applyAll_b fs x)

-- Test: Each of the following should yield True.
test_3_5_a = (applyAll_a [simple 2 2, (+3)] 5) == 20
test_3_5_b = (applyAll_a [simple 2 2, (+3)] 5) == 20

-- ===== Exercise 3.6 =====
-- Ex: Which of the following functions is more efficient, and why?
--       appendr, appendl :: [[a]] -> [a]
--       appendr = foldr (flip (++)) []
--       appendl = foldl (flip (++)) []
-- foldl uses constant space, while foldr takes space linear in the length of its list argument.

-- ===== Exercise 3.7 =====
-- Ex. Rewrite the definition of length non-recursively.

-- Code from HSoM, Chapter 3 (and, in this case, from the Prelude):
-- length [] = 0
-- length (x : xs) = 1 + length xs

-- Code for this exercise:
length_3_7 :: [a] -> Int
length_3_7 = foldr (\_ n -> 1 + n) 0

-- Test code
test_3_7 = (length_3_7 [1,2,3] == 3)  -- Test: Should yield True.

-- ===== Exercise 3.8 =====

-- Code for this exercise:
doubleEach           = map (* 2)
pairAndOne           = map (\x -> (x, x+1))
addEachPair          = map (\(x, y) -> (x + y))
addPairsPointwise xs = (sum $ map fst xs, sum $ map snd xs)

test_doubleEach        = (doubleEach [1,2,3]) == [2,4,6]
test_pairAndOne        = (pairAndOne [1,2,3]) == [(1,2), (2,3), (3,4)]
test_addEachPair       = (addEachPair       [(1,2), (2,3), (3,4)]) == [3,5,7]
test_addPairsPointwise = (addPairsPointwise [(1,2), (2,3), (3,4)]) == (6,9)

-- Test: Should yield True.
test_3_8 = and [test_doubleEach, test_pairAndOne, test_addEachPair, test_addPairsPointwise]

-- ===== Exercise 3.9 =====

-- Code for this exercise:
fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse durs ns = zipWith ($) ns durs

-- Test music
music_3_9_fuse   = line $ fuse [qn, hn, sn] [c 4, d 4, e 4]
music_3_9_nofuse = line $ [c 4 qn, d 4 hn, e 4 sn]

-- Test code
-- Test: Both phrases should sound identical.
-- Note: This works because Eq a => Music a
test_3_9_a = music_3_9_fuse == music_3_9_nofuse

-- Test: Both phrases should sound identical.
test_3_9_b = play $ music_3_9_fuse :+: hnr :+: music_3_9_nofuse

-- ===== Exercise 3.10 =====
-- TODO: Just noticed that HSoM asks for both recursive and non-recursive solutions.

-- Code for this exercise:
-- maxAbsPitch  = maximum
maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch = foldr1 max

-- minAbsPitch = minimum
minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch = foldr1 min

-- ===== Exercise 3.11 =====
-- TODO: Just noticed that HSoM asks for both recursive and non-recursive solutions.

-- Code for this exercise:
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = foldr1 (:+:) musics
  where
    ap1 = absPitch p1
    ap2 = absPitch p2
    aps = [ap1 .. ap2]
          -- if (ap1 > ap2)
          --   then takeWhile (>= ap2) $ iterate (\x -> x-1) ap1
          --   else takeWhile (<= ap2) $ iterate (\x -> x+1) ap1
    gamut = map pitch aps
    mkQn = \x -> Prim $ Note qn x
    musics = map mkQn gamut

-- Test code
test_3_11 = (chrom (C, 4) (C,4)) == Prim (Note qn (C,4))  -- Test: Should yield True.

-- ===== Exercise 3.12 =====

-- Code for this exercise:
majorScale = [2, 2, 1, 2, 2, 2, 1] :: [Int]
chromaticScale = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] :: [Int]
mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p ints = foldr1 (:+:) $ reverse musics
  where
    ap1 = absPitch p
    mkMusics ap []       acc = ap : acc
    mkMusics ap (i : is) acc = mkMusics (ap + i) is (ap : acc)
    aps = mkMusics ap1 ints []
    gamut = map pitch aps
    mkQn = \x -> Prim $ Note qn x
    musics = map mkQn gamut

-- Test music
music_3_12 = mkScale (C,4) majorScale

-- Test code
test_3_12 = play music_3_12  -- Test: Should play a major scale.

-- ===== Exercise 3.13 =====

-- Code for this exercise:
data MusicMode = Ionian
               | Dorian
               | Phrygian
               | Lydian
               | Mixolydian
               | Aeolian
               | Locrian

genScale :: MusicMode ->  Pitch -> Music Pitch
genScale mode p = mkScale p (scale mode)
  where
    ionianScale = [2, 2, 1, 2, 2, 2, 1]
    genMode n = take 12 $ drop n $ cycle ionianScale
    scale Ionian     = genMode 0 -- I
    scale Dorian     = genMode 1 -- II
    scale Phrygian   = genMode 2 -- III
    scale Lydian     = genMode 3 -- IV
    scale Mixolydian = genMode 4 -- V
    scale Aeolian    = genMode 5 -- VI
    scale Locrian    = genMode 6 -- VII

-- Test music
music_3_13 = genScale Ionian (C,4)

-- Test code
test_3_13 = play music_3_13  -- Test: Should play an Ionian (i.e., major) scale.

-- ===== Exercise 3.14 =====
-- Here's a relatively concise score of Frère Jacques.
twice m = m :+: m
fj1, fj2, fj3, fj4, fj :: Music Pitch
fj1 = c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn
fj2 = e 4 qn :+: f 4 qn :+: g 4 hn
fj3 = g 4 en :+: a 4 en :+: g 4 en :+: f 4 en :+: e 4 qn :+: c 4 qn
fj4 = c 4 qn :+: g 3 qn :+: c 4 hn
fj = twice fj1 :+: twice fj2 :+: twice fj3 :+: twice fj4

-- Here's one that uses slightly fewer characters, with perhaps some sacrifice of clarity. 
x2 m = m :+: m
repd d = \ns -> line $ map (\x -> x d) ns
qns = repd qn
ens = repd en
fj1' = x2 $ qns [c 4, d 4, e 4, c 4]
fj2' = x2 $ qns [e 4, f 4] :+: g 4 hn
fj3' = x2 $ ens [g 4, a 4, g 4, f 4] :+: qns [e 4, c 4]
fj4' = x2 $ qns [c 4, g 3] :+: c 4 hn
fj' = fj1' :+: fj2' :+: fj3' :+: fj4'

-- Test music/code
test_3_14_a = play fj
test_3_14_b = play fj'

-- ===== Exercise 3.15 ===== 
encrypt :: String -> String
encrypt str = map (\c -> toEnum ((fromEnum c) + 1)) str

decrypt :: String -> String
decrypt str = map (\c -> toEnum ((fromEnum c) - 1)) str

-- Test code
plaintext  = "Hello"
ciphertext = encrypt plaintext
recovered  = decrypt ciphertext

test_3_15 = recovered == plaintext  -- Test: Should yield True

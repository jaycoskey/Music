-- Haskell School of Music, Chapter 2

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 3.1 =====
-- Transpose a series of pitches.
f1 :: Int -> [Pitch] -> [Pitch]
f1 n ps = map (trans n) ps

-- Map durations to rests.
f2 :: [Dur] -> [Music a]
f2 ds = map rest ds

-- Provide a staccato interpretation of the notes.
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

test_3_1_3 = [c 4 qn , d 4 en, e 4 hn]
test_3_1_3' = f3 test_3_1_3
-- play test_3_1_3
-- play test_3_1_3'

-- ===== Exercise 3.2 =====
-- Let flipf = flip f.  Then
-- flip (flip f) x y = flip flipf x y = flipf y x = flip f y x = f x y
-- So flip (flip f) = f

-- ===== Exercise 3.3 =====
-- xs = [1, 2, 3] :: [Integer]
-- ys = map (+) xs :: [Integer -> Integer]

-- ===== Exercise 3.4 =====
-- applyEach [simple 2 2, (+3)] 5 => [14, 8]
-- where simple x y z = x * (y + z)
applyEach :: [a -> b] -> a -> [b]
applyEach fs x = map ($ x) fs

applyEach' :: [a -> a] -> a -> [a]
applyEach' [] x = []
applyEach' (f : fs) x = (f x) : (applyEach' fs x)

-- ===== Exercise 3.5 =====
-- applyAll [simple 2 2, (+3)] 5 => 20
applyAll :: [a -> a] -> a -> a
applyAll fs = foldr (.) id fs

applyAll' :: [a -> a] -> a -> a
applyAll' [] x = x
applyAll' (f : fs) x = f (applyAll' fs x)

-- ===== Exercise 3.6 =====
-- Which of the following functions is more efficient, and why?
appendr, appendl :: [[a]] -> [a]
appendr = foldr (flip (++)) []
appendl = foldl (flip (++)) []
-- foldl uses constant space, while foldr takes space linear in the length of its list argument.

-- ===== Exercise 3.7 =====
-- length [] = 0
-- length (x : xs) = 1 + length xs

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

-- ===== Exercise 3.8 =====
doubleEach           = map (* 2)
pairAndOne           = map (\x -> (x, x+1))
addEachPair          = map (\(x, y) -> (x + y))
addPairsPointwise xs = (sum $ map fst xs, sum $ map snd xs)

-- ===== Exercise 3.9 =====
fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse durs ns = zipWith ($) ns durs

fuse_musics_test = fuse [qn, hn, sn] [c 4, d 4, e 4] -- => [c 4 qn, d 4 hn, e 4 sn]
-- play $ line $ fuse_musics_test

-- ===== Exercise 3.10 =====
-- maxAbsPitch  = maximum
maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch = foldr1 max

-- minAbsPitch = minimum
minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch = foldr1 min

-- ===== Exercise 3.11 =====
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

-- ===== Exercise 3.12 =====
majorScale = [2, 2, 1, 2, 2, 2] :: [Int]
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

-- ===== Exercise 3.13 =====
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

-- ===== Exercise 3.15 ===== 
encrypt :: String -> String
encrypt str = map (\c -> toEnum ((fromEnum c) + 1)) str

decrypt :: String -> String
decrypt str = map (\c -> toEnum ((fromEnum c) - 1)) str

plaintext  = "Hello"
ciphertext = encrypt plaintext
recovered  = decrypt ciphertext

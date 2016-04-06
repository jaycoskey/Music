-- Haskell School of Music, Chapter 9

import Euterpea
import Euterpea.Music

import HSoM

-- Inserting here code from this chapter.
data Cluster  = Cluster SNote [Cluster]
type SNote    = (Dur,AbsPitch)

selfSim :: [SNote] -> Cluster
selfSim pat = Cluster (0,0) (map mkCluster pat)
    where mkCluster note =
              Cluster note (map (mkCluster . addMult note) pat)

addMult :: SNote -> SNote -> SNote
addMult (d0,p0) (d1,p1) = (d0*d1, p0+p1)

fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note cls) = [note]
fringe n (Cluster note cls) = concatMap (fringe (n-1)) cls

simToMusic :: [SNote] -> Music Pitch
simToMusic = line . map mkNote

mkNote :: (Dur,AbsPitch) -> Music Pitch
mkNote (d, ap) = note d (pitch ap)

-- JMC: Adding signature
ss :: [SNote] -> Int -> AbsPitch -> Dur -> Music Pitch 
ss pat n tr te = 
   transpose tr $ tempo te $ simToMusic $ fringe n $ selfSim pat

m0 :: [SNote]
m0 = [(1,2),(1,0),(1,5),(1,7)]
tm0  = instrument Vibraphone (ss m0 4 50 20)

-- JMC: Adding revM, from Chapter 6
revM :: Music a -> Music a
revM n@(Prim _) = n
revM (Modify c m) = Modify c (revM m)
revM (m1 :+: m2) = revM m2 :+: revM m1
revM (m1 :=: m2) =
    let d1 = dur m1
        d2 = dur m2
    in if d1 > d2 then revM m1 :=: (rest (d1 - d2) :+: revM m2)
                  else (rest (d2 - d1) :+: revM m1) :=: revM m2

ttm0 = tm0 :=: transpose (12) (revM tm0)

m1 :: [SNote]
m1  = [(1,0), (0.5,0), (0.5,0)]
tm1 = instrument Percussion (ss m1 4 43 2)

m2 :: [SNote]
m2 = [(dqn,0), (qn,4)]
tm2 = ss m2 6 50 (1/50)

m3 :: [SNote]
m3 = [(hn,3), (qn,4), (qn,0), (hn,6)]
tm3 = ss m3 4 50 (1/4)

ttm3 = let line1 = instrument Flute tm3
           line2 = instrument AcousticBass
                  $ transpose (-9) (revM tm3)
       in line1 :=: line2

m4 :: [SNote]
m4 = [ (hn,3), (hn,8), (hn,22), (qn,4), (qn,7), (qn,21)
     , (qn,0), (qn,5), (qn,15), (wn,6), (wn,9), (wn,19)
     ]

tm4 = ss m4 3 50 8

fringe' :: Int -> Cluster -> [[SNote]]
fringe' 0  (Cluster note cls)  = [[note]]
fringe' n  (Cluster note cls)  = map (fringe (n-1)) cls

simToMusic'  :: [[SNote]] -> Music Pitch
simToMusic'  = chord . map (line . map mkNote)

ss' pat n tr te = 
   transpose tr $ tempo te $ simToMusic' $ fringe' n $ selfSim pat

ss1  = ss' m2 4 50 (1/8)
ss2  = ss' m3 4 50 (1/2)
ss3  = ss' m4 3 50 2

m5   = [(en,4), (sn,7), (en,0)]
ss5  = ss  m5 4 45 (1/500)
ss6  = ss' m5 4 45 (1/1000)

-- ===== Exercise 9.1 =====
scratchDog  = (transpose 30 tm1) :=: (wnr :+: transpose 36 tm1)
scratchDog2 = (transpose 30 tm1) :=: (hnr :+: transpose 33 tm1) :=: (wnr :+: transpose 36 tm1)
scratchDog3 = (transpose 30 tm1)
              :=: (qnr  :+: transpose 31 tm1)
              :=: (hnr  :+: transpose 33 tm1)
              :=: (dhnr :+: transpose 34 tm1)
              :=: (wnr  :+: transpose 36 tm1)

closeEncountersSeed = zip [qn, qn, hn, en, qn] (map absPitch [(G,3), (A,3), (F,3), (F,2), (C,3)])
twice m = m :+: m
addFlip m = m :+: (revM m)

test_9_1_a = addVolume 127 $ instrument Trumpet $ addFlip  $ ss closeEncountersSeed 1 0     1
test_9_1_b = addVolume 96  $                                 ss closeEncountersSeed 2 (-48) (11/16)
test_9_1_c = addVolume 96  $                                 ss closeEncountersSeed 3 (-96) (1331/1408)

test_9_1 = chord [test_9_1_a, test_9_1_b, test_9_1_c]

-- ===== Exercise 9.2 =====
-- Define a version of fringe that is linear in the total length of the final list.

-- fringe :: Int -> Cluster -> [SNote]
-- fringe 0 (Cluster note cls) = [note]
-- fringe n (Cluster note cls) = concatMap (fringe (n-1)) cls

-- ===== Exercise 9.3 =====
d1 = [hn,   qn,  hn,  qn,  hn]
d2 = [dqn,  hn,  qn,  hn, dqn]
d3 = [dqn, dqn,  hn, dqn, dqn]

rotate n xs = take (length xs) $ drop n $ cycle xs
p1 = map absPitch [(C,3), (D,3), (E,3), (G,3), (A,3)]
p2 = rotate 1 p1
p3 = rotate 2 p1
p4 = rotate 3 p1
p5 = rotate 4 p1

dp d p = ss (zip d p) 2 (-48) 1
m11=dp d1 p1; m12=dp d1 p2; m13=dp d1 p3; m14=dp d1 p4; m15=dp d1 p5
m21=dp d2 p1; m22=dp d2 p2; m23=dp d2 p3; m24=dp d2 p4; m25=dp d2 p5
m31=dp d3 p1; m32=dp d1 p2; m33=dp d3 p3; m34=dp d3 p4; m35=dp d3 p5

tumbling_9_3_a = (transpose (12) m14) :=: (transpose (-12) $ revM m31)
tumbling_9_3_b = (transpose (12) m22) :=: (transpose (-12) $ revM m14)
tumbling_9_3_c = (transpose (12) m35) :=: (transpose (-12) $ revM m23)
-- Meh.

-- ===== Exercise 9.4 =====
-- Play the levels of a Cluster in parallel.
-- They can be glued together using chord instead of line, as with fringe'/simToMusic'.
tm2_9_4_a = ss' m2 6 50 (1/50)
-- But that doesn't play them all with the same duration.

simToMusic_9_4 :: [[SNote]] -> Music Pitch
simToMusic_9_4 snotes = chord procrusteanLines
    where lines = map line $ (map . map) mkNote snotes
          maxDur = maximum $ map dur lines
          procrusteanLines = map (\mus -> tempo ((dur mus) / maxDur) mus) lines

ss_9_4 :: [SNote] -> Int -> AbsPitch -> Dur -> Music Pitch 
ss_9_4 pat n tr te =
   transpose tr $ tempo te $ simToMusic_9_4 $ fringe' n $ selfSim pat

tm2_9_4_b = ss_9_4 m2 6 50 (1/50)

-- ===== Exercise 9.5 =====
-- Use scales, and swap pitch & duration.
-- TODO

-- ===== Exercise 9.6 =====
-- Add volume to SNote
-- TODO

-- ===== Exercise 9.7 =====
-- Devise some other variant of self-similar music,
-- using structures different from those in selfSim.
-- TODO

-- ===== Exercise 9.8 =====
-- Define a function that gives the same result as ss,
-- but without using a data type such as Cluster.
-- TODO

-- ===== Exercise 9.9 =====
-- Entire melody level-to-level transformation
-- Possible ideas:
--   * Permute pitches, while preserving durations.
--   * Permute durations, while preserving pitches.
--   * duplicate melody, while increasing tempo to preserve duration.
--   * Rotate PitchClasses, while preserving Octaves.
-- TODO
-- Haskell School of Music, Chapter 9

import Euterpea
import Euterpea.Music

import HSoM

-- Inserting here code from this chapter.
data Cluster = Cluster SNote [Cluster]
type SNote   = (Dur,AbsPitch)

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

procrusteanize :: [Music a] -> [Music a]
procrusteanize ms = map adjDur ms
    where maxDur = maximum $ map dur ms
          adjDur mus = tempo ((dur mus) / maxDur) mus

simToMusic_9_4 :: [[SNote]] -> Music Pitch
simToMusic_9_4 snotes = chord procrusteanLines
    where lines = map line $ (map . map) mkNote snotes
          procrusteanLines = procrusteanize lines

ss_9_4 :: [SNote] -> Int -> AbsPitch -> Dur -> Music Pitch 
ss_9_4 pat n tr te =
   transpose tr $ tempo te $ simToMusic_9_4 $ fringe' n $ selfSim pat

tm2_9_4_b = ss_9_4 m2 6 50 (1/50)

-- ===== Exercise 9.5 =====
-- Use scales, and swap pitch & duration.

-- Instead of using
--   * type AbsPitch = Int as a count of notes on the chromatic scale based at C, -1) (**)
--   * type Pitch = (PitchClass, Octave)
-- we'll use
--   * type Key = (PitchClass, Mode) (suggested by Context's definition of cKey)
--     Note that KeySig is already taken, as a constructor of Control.
--   * type AbsKeyPitch = Int
--   * type KeyPitch = (Key, AbsKeyPitch)
-- where (k@(pc, mode), kap) :: KeyPitch corresponds to the (zero-based) kap-th note in the Key k,
-- based at the note (pc, 0) :: Pitch.

-- (**) Per Secion 2.4 of HSoM, pitch 0 = (C,0), but in the current version of Euterpea, pitch 0 = (C,-1).

type Key         = (PitchClass, Mode)
type AbsKeyPitch = Int
type KeyPitch    = (Key, AbsKeyPitch)
type KNote       = (KeyPitch, Dur)
data KCluster    = KCluster KNote [KCluster]

type ScaleIntervals = [Int]
mkScaleIntervals :: Mode -> ScaleIntervals
mkScaleIntervals Minor = [2, 2, 1, 2, 2, 2]
mkScaleIntervals Major = [2, 2, 1, 2, 2, 2]

toPitch :: KeyPitch -> Pitch
toPitch ((pc, mode), kp) = pitch ap
    where scaleIntervals = mkScaleIntervals mode
          scaleLength    = length scaleIntervals
          scaleSpan      = sum scaleIntervals
          scaleDiv       = kp `div` scaleLength
          scaleMod       = kp `mod` scaleLength
          scaleModSpan   = foldr (+) 0 (take scaleMod scaleIntervals)
          ap = absPitch (pc, 0) + scaleDiv * scaleSpan + scaleModSpan

-- Note: Preserve the key of the first argument.
kAddMult :: KNote -> KNote -> KNote
kAddMult ((k0,akp0),d0) ((k1,akp1),d1) = ((k0, akp0+akp1), d0*d1)

kSelfSim :: [KNote] -> KCluster
kSelfSim pat = KCluster (((C,Major),0), 0) (map mkKCluster pat)
    where mkKCluster kNote =
              KCluster kNote (map (mkKCluster . kAddMult kNote) pat)

kFringe :: Int -> KCluster -> [KNote]
kFringe 0 (KCluster kNote kCls) = [kNote]
kFringe n (KCluster kNote kCls) = concatMap (kFringe (n-1)) kCls

mkKNote :: (KeyPitch, Dur) -> Music Pitch
mkKNote (kp, d) = note d (toPitch kp)

kSimToMusic :: [KNote] -> Music Pitch
kSimToMusic = line . map mkKNote

kSs :: [KNote] -> Int -> AbsKeyPitch -> Dur -> Music Pitch 
kSs pat n tr te = 
   transpose tr $ tempo te $ kSimToMusic $ kFringe n $ kSelfSim pat

km2 :: [KNote]
km2 = [(((C,Major),0),dqn), (((C,Major),4), qn)]
tkm2_9_5 = kSs km2 6 50 (1/50)

-- ===== Exercise 9.6 =====
-- Add volume to SNote
type VNote     = (Dur,AbsPitch,Volume)
data VCluster  = VCluster VNote [VCluster]

vSelfSim :: [VNote] -> VCluster
vSelfSim pat = VCluster (0,0,0) (map mkVCluster pat)
    where mkVCluster vNote =
              VCluster vNote (map (mkVCluster . vNoteMix vNote) pat)

vNoteMix :: VNote -> VNote -> VNote
vNoteMix (d0,ap0,v0) (d1,ap1,v1) = ( d0 * d1
                                   , ap0 + ap1
                                   , 32 + ((v0 + v1) `mod` 64)  -- Non troppo piano, non troppo forte
                                   )

vFringe :: Int -> VCluster -> [VNote]
vFringe 0 (VCluster vNote vCls) = [vNote]
vFringe n (VCluster vNote vCls) = concatMap (vFringe (n-1)) vCls

mkVNote :: (Dur, AbsPitch, Volume) -> Music (Pitch, Volume)
mkVNote (d, ap, v) = addVolume v $ note d (pitch ap)

vSimToMusic :: [VNote] -> Music (Pitch, Volume)
vSimToMusic = line . map mkVNote

vss :: [VNote] -> Int -> AbsPitch -> Dur -> Music (Pitch, Volume)
vss pat n tr te = 
   transpose tr $ tempo te $ vSimToMusic $ vFringe n $ vSelfSim pat

vm2 :: [VNote]
vm2 = [(dqn,0, 90), (qn,4, 100)]
tvm2 = vss vm2 6 50 (1/50)

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

-- Use the "truncating parallel composition" operator, (/=:).
swapHalves :: Rational -> Music a -> Music a
swapHalves rat mus = half2 :+: half1
    where dur1 = rat * (dur mus)
          half1 = takeM dur1 mus
          half2 = dropM dur1 mus

-- Functions defined in Chapter 6 of HSoM
-- BEGIN
dropM :: Dur -> Music a -> Music a
dropM d m | d <= 0           = m
dropM d (Prim (Note oldD p)) = note (max (oldD - d) 0) p
dropM d (Prim (Rest oldD))   = rest (max (oldD - d) 0)
dropM d (m1 :=: m2)          = dropM d m1 :=: dropM d m2
dropM d (m1 :+: m2)          = let m'1 = dropM d m1
                                   m'2 = dropM (d - dur m1) m2
                               in  m'1 :+: m'2
dropM d (Modify (Tempo r) m) = tempo r (dropM (d * r) m)
dropM d (Modify c m)         = Modify c (dropM d m)

takeM :: Dur -> Music a -> Music a
takeM d m | d <= 0    = rest 0
takeM d (Prim (Note oldD p)) = note (min oldD d) p
takeM d (Prim (Rest oldD))   = rest (min oldD d)
takeM d (m1 :=: m2)          = takeM d m1 :=: takeM d m2
takeM d (m1 :+: m2)          = let m'1 = takeM d m1
                                   m'2 = takeM (d - dur m'1) m2
                               in  m'1 :+: m'2
takeM d (Modify (Tempo r) m) = tempo r (takeM (d * r) m)
takeM d (Modify c m)         = Modify c (takeM d m)
-- END

-- Slice up a piece of music before re-joining it, like shuffling a deck of cards.
revMSlices :: [Rational] -> Music a -> [Music a]
revMSlices durs mus = reverse slices
    where beginEnds =  zip (0.0 : durs) (durs ++ [1.0])
          slice :: (Rational, Rational) -> Music a -> Music a
          slice (begin, end) mus = dropM begin $ takeM end mus
          slices = map (\be -> slice be mus) beginEnds

closeEncounters :: Music Pitch
closeEncounters = line $ [g 3 qn, a 3 qn, f 3 qn, f 2 qn, c 3 hn]

slicePoints :: [Rational]
slicePoints = [0.25, 1/3.1415, 0.66]

test_9_9_a :: Music Pitch
test_9_9_a = line $ revMSlices slicePoints closeEncounters
-- play test_9_9_a

nthIteration :: Int -> (a -> a) -> a -> a
nthIteration n f xs = (iterate f xs) !! n

test_9_9_b :: Music Pitch
test_9_9_b = nthIteration 3 (line . revMSlices slicePoints) closeEncounters
-- play test_9_9_b

-- Apply a function to each slice.
revModMSlices :: [Rational] -> (Music a -> Music a) -> Music a -> [Music a]
revModMSlices durs modF mus = reverse $ map modF slices
    where beginEnds =  zip (0.0 : durs) (durs ++ [1.0])
          slice :: (Rational, Rational) -> Music a -> Music a
          slice (begin, end) mus = dropM begin $ takeM end mus
          slices = map (\be -> slice be mus) beginEnds

test_9_9_b' :: Music Pitch
test_9_9_b' = nthIteration 3 (line . revModMSlices slicePoints revM) closeEncounters
-- play test_9_9_b'

test_9_9_b'' = test_9_9_b :=: test_9_9_b'
-- play test_9_9_b''

-- Also apply a function to the collection of slices.
type MSliceMod a = Music a -> Music a
type MSliceCollectionMod a = [Music a] -> [Music a]

revModMSlices' :: [Rational] -> MSliceMod a -> MSliceCollectionMod a -> Music a -> [Music a]
revModMSlices' durs sliceF sliceCollectionF mus = reverse $ sliceCollectionF $ map sliceF slices
    where beginEnds =  zip (0.0 : durs) (durs ++ [1.0])
          slice :: (Rational, Rational) -> Music a -> Music a
          slice (begin, end) mus = dropM begin $ takeM end mus
          slices = map (\be -> slice be mus) beginEnds

test_9_9_c = tempo (1/6) $ chord $ blender $ nthIteration 3 (line . blender) closeEncounters
             where blender :: Music a -> [Music a]
                   blender = revModMSlices' slicePoints revM procrusteanize

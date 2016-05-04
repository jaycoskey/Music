-- Amen Break, a drum solo from the song Amen Brother, by The Winstons.
-- Originally played by Gregory Cylvester "G. C." Coleman.
-- Transcription into Euterpea by Jay Coskey
-- For a readable score, see https://en.wikipedia.org/wiki/Amen_break#Drumming_tabs_and_notation
-- For accents, see https://breakbeatdrumming.wordpress.com/2009/01/31/the-amen-break/

module AmenBreak where

import Data.Ratio
import Euterpea
-- import Euterpea.Music
-- import HSoM

-- ========================================
-- revM, from Chapter 6 of HSoM
-- ========================================
revM :: Music a -> Music a
revM n@(Prim _) = n
revM (Modify c m) = Modify c (revM m)
revM (m1 :+: m2) = revM m2 :+: revM m1
revM (m1 :=: m2) =
    let d1 = dur m1
        d2 = dur m2
    in if d1 > d2 then revM m1 :=: (rest (d1 - d2) :+: revM m2)
                  else (rest (d2 - d1) :+: revM m1) :=: revM m2

-- timesM, from HSoM
timesM :: Int -> Music a -> Music a
timesM 0 m = rest 0
timesM n m = m :+: timesM (n - 1) m

-- ========================================
-- The Amen Break
-- ========================================
data Beat = Beat { bDur   :: Dur
                 , bHeard :: Int
                 , bAcc   :: Int
                 }
--            deriving Show

instance Show Beat where
    show (b@Beat {bDur=d, bHeard=h, bAcc=a})
        = "Beat {"
            ++ "bDur=" ++ (show d)
            ++ ", bHeard=" ++ (show h)
            ++ ", bAcc=" ++ (show a)
            ++ "}"

type Beats = [Beat]
infixr 6 .+.
infixr 7 .*.
(.*.) n   bs  = concat $ replicate n bs
(.+.) bs1 bs2 = bs1 ++ bs2

beats2m :: PercussionSound -> Beats -> Music Pitch
beats2m percInstr [] = rest 0
beats2m percInstr (Beat { bDur=d, bHeard=h, bAcc=a } : beats)
    = beat1 :+: (beats2m percInstr beats)
        where b0 = Modify (Instrument Percussion) $ perc percInstr d
              beat1 = case (h,a) of
                        (0, _) -> rest d
                        (1, 0) -> b0
                        (1, 1) -> Modify (Phrase [Dyn (Accent 1.5)]) b0

-- Beat and silence definitions (analog of notes & rests, for percussion)
hns   = [Beat { bDur = hn,  bHeard = 0, bAcc = 0 }]

qns   = [Beat { bDur = qn,  bHeard = 0, bAcc = 0 }]
-- qnb   = [Beat { bDur = qn,  bHeard = 1, bAcc = 0 }]
-- qnb'  = [Beat { bDur = qn,  bHeard = 1, bAcc = 1 }]

--- dens  = [Beat { bDur = den, bHeard = 0, bAcc = 0 }]
-- denb  = [Beat { bDur = den, bHeard = 1, bAcc = 0 }]
denb' = [Beat { bDur = den, bHeard = 1, bAcc = 1 }]

ens   = [Beat { bDur = en,  bHeard = 0, bAcc = 0 }]
enb   = [Beat { bDur = en,  bHeard = 1, bAcc = 0 }]
enb'  = [Beat { bDur = en,  bHeard = 1, bAcc = 1 }]

sns   = [Beat { bDur = sn,  bHeard = 0, bAcc = 0 }]
snb   = [Beat { bDur = sn,  bHeard = 1, bAcc = 0 }]
-- snb'  = [Beat { bDur = sn,  bHeard = 1, bAcc = 1 }]

-- Amen Break
instrC = CrashCymbal1
instrR = RideCymbal1
instrS = AcousticSnare
instrB = AcousticBassDrum

b31      = denb' .+. snb             -- Recurs 6 times.  Dur = qn
enbs4_b3 = 2.*.enb .+. enb' .+. enb  -- Recurs 3x.       Dur = hn
enbs4_b4 = 3.*.enb .+. enb'
sn_sb2   = 2.*.(sns .+. snb)         -- Recurs twice.    Dur = qn

amen1r = beats2m instrR $ 2.*.enbs4_b3
amen1s = beats2m instrS $ qns .+. b31 .+. sns .+. snb .+. ens .+. b31
amen1b = beats2m instrB $ 2.*.enb .+. qns .+. ens .+. 2.*.snb .+. qns

amen2r = amen1r
amen2s = amen1s
amen2b = amen1b

amen3r = beats2m instrR $ enbs4_b3 .+. enbs4_b4
amen3s = beats2m instrS $ 2.*.ens .+. enb .+. sn_sb2 .+.  2.*.ens .+. enb
amen3b = beats2m instrB $ 2.*.enb .+. 3.*.ens .+. enb .+. qns

amen4c = beats2m instrC $ hns .+. ens .+. enb .+. qns
amen4r = beats2m instrR $ enbs4_b3 .+. enb .+. ens .+. enb .+. enb'
amen4s = beats2m instrS $ sns .+. snb .+. ens .+. b31 .+. sn_sb2 .+. ens .+. enb
amen4b = beats2m instrB $ ens .+. 2.*.snb .+. qns .+. ens .+. enb .+. qns

amen1 = chord [amen1r, amen1s, amen1b]
amen2 = chord [amen2r, amen2s, amen2b]
amen3 = chord [amen3r, amen3s, amen3b]
amen4 = chord [amen4r, amen4s, amen4b, amen4c]

amenBreak = line [amen1, amen2, amen3, amen4]

-- ========================================
-- Test code
-- ========================================
playBeats :: Beats -> IO ()
playBeats bs = play $ beats2m AcousticBassDrum bs

test1  = and $ map (\x -> dur x == 1%1) [amen1r, amen1s, amen1b]
test2  = and $ map (\x -> dur x == 1%1) [amen2r, amen2s, amen2b]
test3  = and $ map (\x -> dur x == 1%1) [amen3r, amen3s, amen3b]
test4  = and $ map (\x -> dur x == 1%1) [amen4r, amen4s, amen4b, amen4c]

test   = and [test1, test2, test3, test4]

-- ========================================
-- Amen Break variations
-- ========================================
rest_1_7_8 = dwnr :+: dqnr
rest_2_1_8 = wnr :+: wnr :+: enr
_amenBreak4_1_7_8        = rest_1_7_8 :+: amenBreak4
_amenBreak4_2_1_8        = rest_2_1_8 :+: amenBreak4


amenBreak4              = timesM 4 amenBreak
amenBreak4_Rev          = revM amenBreak4
amenBreak4_FwdRev       = amenBreak4 :=: revM amenBreak4
amenBreak4_ens          = amenBreak4 :=: _amenBreak4_1_7_8 :=: _amenBreak4_2_1_8 
amenBreak4_qn_hn        = amenBreak4 :=: (qnr :+: amenBreak4) :=: (hnr :+: amenBreak4)
amenBreak_4_2_1         = (timesM 4 amenBreak)
                            :=: (tempo (2/4) $ timesM 2 amenBreak)
                            :=: (tempo (1/4) $ timesM 1 amenBreak)

-- ========================================
-- Failures (those that don't sound as interesting)
-- ========================================
{-
amenBreak4_qn_hn_FwdRev = amenBreak4_qn_hn :=: revM amenBreak4_qn_hn
amenBreak_3_2 = (timesM 3 amenBreak) :=: (tempo (2/3) $ timesM 2 amenBreak)
amenBreak_4_3 = (timesM 4 amenBreak) :=: (tempo (3/4) $ timesM 3 amenBreak)
amenBreak_5_3 = (timesM 5 amenBreak) :=: (tempo (3/5) $ timesM 3 amenBreak)
amenBreak_7_3 = (timesM 7 amenBreak) :=: (tempo (3/7) $ timesM 3 amenBreak)
amenBreak_7_5 = (timesM 7 amenBreak) :=: (tempo (5/7) $ timesM 5 amenBreak)
amenBreak_9_8 = (timesM 9 amenBreak) :=: (tempo (8/9) $ timesM 8 amenBreak)
-}

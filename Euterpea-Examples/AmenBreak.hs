-- Amen Break.  Transcription into Euterpea by Jay Coskey
-- See https://breakbeatdrumming.wordpress.com/2009/01/31/the-amen-break/

module AmenBreak where

import Data.Ratio

import Euterpea
import Euterpea.Music
import HSoM

data Beat = Beat { bDur :: Dur
                 , bVol :: Int
                 , bAcc :: Int
                 }
--            deriving Show

instance Show Beat where
    show (b@Beat {bDur=d, bVol=v, bAcc=a})
        = "Beat {"
            ++ "bDur=" ++ (show d)
            ++ ", bVol=" ++ (show v)
            ++ ", bAcc=" ++ (show a)
            ++ "}"

type Beats = [Beat]
infixr 6 .+.
infixr 7 .*.
(.*.) n   bs  = concat $ replicate n bs
(.+.) bs1 bs2 = bs1 ++ bs2

bs2m :: PercussionSound -> Beats -> Music Pitch
-- bs2m :: PercussionSound -> Beats -> Music (Pitch, Volume)
bs2m percInstr [] = rest 0
bs2m percInstr (Beat { bDur=d, bVol=v, bAcc=a } : beats)
    = beat1 :+: (bs2m percInstr beats)
        where b0 = Modify (Instrument Percussion) $ perc percInstr d
              beat1 = case (v,a) of
                        (0, _) -> rest d
                        (1, 0) -> b0
                        (1, 1) -> Modify (Phrase [Dyn (Accent 1.5)]) b0

-- Beat and silence definitions (analog of notes & rests, for percussion)
hns   = [Beat { bDur = hn, bVol = 0, bAcc = 0 }]

qns   = [Beat { bDur = qn,  bVol = 0, bAcc = 0 }]
qnb   = [Beat { bDur = qn,  bVol = 1, bAcc = 0 }]
qnb'  = [Beat { bDur = qn,  bVol = 1, bAcc = 1 }]

dens  = [Beat { bDur = den, bVol = 0, bAcc = 0 }]
denb  = [Beat { bDur = den, bVol = 1, bAcc = 0 }]
denb' = [Beat { bDur = den, bVol = 1, bAcc = 1 }]

ens   = [Beat { bDur = en,  bVol = 0, bAcc = 0 }]
enb   = [Beat { bDur = en,  bVol = 1, bAcc = 0 }]
enb'  = [Beat { bDur = en,  bVol = 1, bAcc = 1 }]

sns   = [Beat { bDur = sn,  bVol = 0, bAcc = 0 }]
snb   = [Beat { bDur = sn,  bVol = 1, bAcc = 0 }]
snb'  = [Beat { bDur = sn,  bVol = 1, bAcc = 1 }]

-- Amen Break
instrC = CrashCymbal1
instrR = RideCymbal1
instrS = AcousticSnare
instrB = AcousticBassDrum

b31      = denb' .+. snb             -- Recurs 6 times.  Dur = qn
enbs4_b3 = 2.*.enb .+. enb' .+. enb  -- Recurs 3x.       Dur = hn
enbs4_b4 = 3.*.enb .+. enb'
sn_sb2   = 2.*.(sns .+. snb)         -- Recurs twice.    Dur = qn

amen1r = bs2m instrR $ 2.*.enbs4_b3
amen1s = bs2m instrS $ qns .+. b31 .+. sns .+. snb .+. ens .+. b31
amen1b = bs2m instrB $ 2.*.enb .+. qns .+. ens .+. 2.*.snb .+. qns

amen2r = amen1r
amen2s = amen1s
amen2b = amen1b

amen3r = bs2m instrR $ enbs4_b3 .+. enbs4_b4
amen3s = bs2m instrS $ 2.*.ens .+. enb .+. sn_sb2 .+.  2.*.ens .+. enb
amen3b = bs2m instrB $ 2.*.enb .+. 3.*.ens .+. enb .+. qns

amen4c = bs2m instrC $ hns .+. ens .+. enb .+. qns
amen4r = bs2m instrR $ enbs4_b3 .+. enb .+. ens .+. enb .+. enb'
amen4s = bs2m instrS $ sns .+. snb .+. ens .+. b31 .+. sn_sb2 .+. ens .+. enb
amen4b = bs2m instrB $ ens .+. 2.*.snb .+. qns .+. ens .+. enb .+. qns

amen1 = chord [amen1r, amen1s, amen1b]
amen2 = chord [amen2r, amen2s, amen2b]
amen3 = chord [amen3r, amen3s, amen3b]
amen4 = chord [amen4r, amen4s, amen4b, amen4c]

amenBreak = line [amen1, amen2, amen3, amen4]

-- Test code
playBeats :: Beats -> IO ()
playBeats bs = play $ bs2m AcousticBassDrum bs

test1  = and $ map (\x -> dur x == 1%1) [amen1r, amen1s, amen1b]
test2  = and $ map (\x -> dur x == 1%1) [amen2r, amen2s, amen2b]
test3  = and $ map (\x -> dur x == 1%1) [amen3r, amen3s, amen3b]
test4  = and $ map (\x -> dur x == 1%1) [amen4r, amen4s, amen4b, amen4c]

test   = and [test1, test2, test3, test4]
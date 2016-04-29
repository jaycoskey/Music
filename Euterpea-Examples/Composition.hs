-- module Music.Experimental.Composition where
module Composition where

import Data.List
import Euterpea

type Durs    = [Dur]
type Pitches = [Pitch]
-- type NoteAttribute  -- Volume = Int  -- 0 to 127

-- PhraseAttribute -- (Dyn Dynamic, Tmp Tempo, Art Articulation, Orn Ornament)

-- End result is a list of [N]otes
mzipN :: Durs -> Pitches -> [Music Pitch]
mzipN durs pitches = zipWith note durs pitches

-- End result is a [M]usic value.
mzipM :: Durs -> Pitches -> Music Pitch
mzipM durs pitches = line $ mzipN durs pitches

-- Test music
durs1      = [en,   en,   den,  sn,   den,  sn,   en,   en,   qn   ]
pitches1   = [(C,4),(E,4),(G,4),(B,4),(E,4),(G,3),(G,4),(G,4),(C,4)]
durs1r     = reverse durs1
pitches1r  = reverse pitches1
instr1     = Clarinet
instrName1 = "Clarinet"

durs2      = [en,   sn,   sn,   den,  sn,   den,  sn,   qn,   qn   ]
pitches2   = [(F,3),(A,3),(A,3),(C,4),(E,3),(G,3),(E,3),(C,3),(F,3)]
durs2r     = reverse durs2
pitches2r  = reverse pitches2
instr2     = Viola
instrName2 = "Viola"

durs3      = [en,   en,   den,  sn,   den,  sn,   den,  sn,   sn,   sn,   en   ]
pitches3   = [(A,3),(C,3),(C,3),(E,3),(C,4),(G,3),(E,3),(G,2),(G,3),(C,4),(F,3)]
durs3r     = reverse durs3
pitches3r  = reverse pitches3
instr3     = AcousticGrandPiano
instrName3 = "Piano"

mzip1  = mzipM durs1 pitches1
mzip2  = mzipM durs2 pitches2
mzip3  = mzipM durs3 pitches3

m1     = addVolume 50  $ instrument instr1 mzip1 
m2     = addVolume 110 $ instrument instr2 mzip2
m3     = addVolume 127 $ instrument instr3 mzip3

mzip1r = mzipM durs1r pitches1r
mzip2r = mzipM durs2r pitches2r
mzip3r = mzipM durs3r pitches3r

m1r    = addVolume 50  $ instrument instr1 mzip1 
m2r    = addVolume 110 $ instrument instr2 mzip2
m3r    = addVolume 127 $ instrument instr3 mzip3

absPitch2LpNote :: Octave -> AbsPitch -> String
absPitch2LpNote baseOct ap = lpNote ++ lpOct
    where ptch = pitch ap
          pc   = fst ptch
          oct  = snd ptch
          lpNote = case pc of
              C -> "c"; Cs -> "cis"; D -> "d"; Ds -> "dis"
              E -> "e";              F -> "f"; Fs -> "fis"
              G -> "g"; Gs -> "gis"; A -> "a"; As -> "ais"
              B -> "b"
          lpOct = replicate (oct - baseOct) '\''  -- Assumes oct >= baseOct

durNotation :: Dur -> String
durNotation dur
    | dur == wn    = "1"
    |   dur == dwn = "1."
    | dur == hn    = "2"
    |   dur == dhn = "2."
    | dur == qn    = "4"
    |   dur == dqn = "4."
    | dur == en    = "8"
    |   dur == den = "8."
    | dur == sn    = "16"
    |   dur == dsn = "16."
    | otherwise    = error "unsupported duration: " ++ (show dur)

-- These yield the correct frequencies,
--     but use only sharps---never flats.
absPitches2LpChord :: Octave -> Dur -> [AbsPitch] -> String
absPitches2LpChord baseOct d aps = "<" ++ notes ++ ">" ++ durStr
    where notes  = intercalate " " $ map (absPitch2LpNote baseOct) aps
          durStr = durNotation d

durAbsPitch2LpNote :: Octave -> Dur -> AbsPitch -> String
durAbsPitch2LpNote baseOct d ap = noteName ++ durStr
    where noteName = absPitch2LpNote baseOct ap
          durStr   = durNotation d

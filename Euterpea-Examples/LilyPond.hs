-- module Music.Experiment.LilyPond where
module LilyPond where

import Data.List
import Data.List.Split

import Euterpea

import Chord

-- ========== ========== ========== ==========
-- LILYPOND FILE COMPONENTS
-- ========== ========== ========== ==========
lpHeader                 = unlines
    [ "\\version \"2.18.2\""
    , "\\layout { indent = 0.0 }"
    , "\\score {"
    , "  <<"
    ]
lpAbsoluteBegin          = unlines
    [ "    \\absolute {"
    ]
lpKeyTempo               = unlines
    [ "      \\key c \\major"
    , "      \\time 4/4"
    ]
lpChordmodeBegin         = unlines
    [ "      \\chordmode {"
    ]
lpChordmodeEnd           = unlines
    [ "      }"
    ]
lpAbsoluteEnd            = unlines
    [ "    }"
    ]
lpAddLyricsBegin         = unlines
    [ "  \\addlyrics {"
    ]
lpAddLyricsEnd           = unlines
    [ "  }"
    ]
lpOutputFooter           = unlines
    [ "  >>"
    , "  \\layout { }"
    , "  \\midi { \\tempo 4 = 100 }"
    , "}"
    ]

-- ========== ========== ========== ==========
-- LILYPOND OUTPUT CODE
-- ========== ========== ========== ==========
chordCode2Input :: ChordCode -> String
chordCode2Input "pow1.5"   = "\\powerChords_c1:1.5"
chordCode2Input "pow1.5.8" = "\\powerChords_c1:1.5.8"
chordCode2Input code       = "c1:" ++ code

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

durAbsPitch2LpNote :: Octave -> Dur -> AbsPitch -> String
durAbsPitch2LpNote baseOct d ap = lpNote ++ lpDur
    where lpNote = absPitch2LpNote baseOct ap
          lpDur  = durNotation d

-- These yield the correct frequencies, but display all non-scale notes as sharps.
absPitches2LpChord :: Octave -> [AbsPitch] -> String
absPitches2LpChord baseOct aps = "<" ++ notes ++ ">1"
    where notes = intercalate " " $ map (absPitch2LpNote baseOct) aps

-- Single instrument, single note melody
lpFileFromMelody :: String -> String -> [(Dur, Pitch)] -> String
lpFileFromMelody instrStr tempoStr durPitches
    = lpHeader
      ++ lpAbsoluteBegin
      ++ lpKeyTempo
      ++ "      " ++ lpMelody
      -- ++ lpChordmodeBegin
      -- ++ lpChordListReference
      -- ++ lpChordmodeEnd
      ++ lpAbsoluteEnd
      -- ++ lpAddLyrics
      -- ++ lpChordCodeListReference
      -- ++ lpAddLyricsEnd
      ++ lpOutputFooter
    where 
        baseOct  = 3
        toLpNotes (d,p) = durAbsPitch2LpNote baseOct d (absPitch p)
        lpNotes  = map toLpNotes durPitches
        lpMelody = intercalate " " lpNotes ++ "\n"

-- ========== ========== ========== ==========
-- LILYPOND TEST #0 [Reference]
-- ========== ========== ========== ==========
lpChordListReference     = unlines
    [ "c1:5         c1:m5        c1:aug"
    , "c1:dim       c1:7         c1:maj7      c1:m7"
    , "c1:dim7      c1:aug7      c1:m7.5-     c1:7+"
    , "c1:6         c1:m6        c1:9         c1:maj9"
    , "c1:m9        c1:11        c1:maj11     c1:m11"
    , "c1:13        c1:13.11     c1:maj13.11  c1:m13.11"
    , "c1:sus2      c1:sus4"
    , "\\powerChords c1:1.5"
    , "\\powerChords c1:1.5.8"
    ]

lpChordCodeListReference = unlines
    [ "  \"5\"     \"m5\"     \"m\"         \"aug\""
    , "  \"dim\"   \"7\"      \"maj7\"      \"m7\""
    , "  \"dim7\"  \"aug7\"   \"m7.5-\"     \"7+\""
    , "  \"6\"     \"m6\"     \"9\"         \"maj9\""
    , "  \"m9\"    \"11\"     \"maj11\"     \"m11\""
    , "  \"13\"    \"13.11\"  \"maj13.11\"  \"m13.11\""
    , "  \"sus2\"  \"sus4\"   \"pow1.5\"    \"pow1.5.8\""
    ]

lpOutputReference
    = lpHeader
      ++ lpAbsoluteBegin
      ++ lpKeyTempo
      ++ lpChordmodeBegin
      ++ lpChordListReference
      ++ lpChordmodeEnd
      ++ lpAbsoluteEnd
      ++ lpAddLyricsBegin
      ++ lpChordCodeListReference
      ++ lpAddLyricsEnd
      ++ lpOutputFooter

-- ========== ========== ========== ==========
-- LILYPOND TEST #1
-- ========== ========== ========== ==========
prefixWithNote s = "c1:" ++ s ++ " "
indent3        s = "      " ++ s
lpChordListTest1
    = map (\c -> if c == '_' then ' '; else c) $ unlines groupsOf4
        where rootedChordCodes = map chordCode2Input commonChordCodes
              -- TODO?: Replace unwords with something with event spacing.
              groupsOf4 = map (indent3 . unwords) $ chunksOf 4 rootedChordCodes

quotify        s = "\"" ++ s ++ "\" "
indent2        s = "    " ++ s
lpChordCodeListTest1
    = unlines groupsOf4
        where chordCodes = map quotify commonChordCodes
              groupsOf4  = map (indent2 . concat) $ chunksOf 4 chordCodes

lpOutputTest1
    = lpHeader
      ++ lpAbsoluteBegin
      ++ lpKeyTempo
      ++ lpChordmodeBegin
      ++ lpChordListTest1
      ++ lpChordmodeEnd
      ++ lpAbsoluteEnd
      ++ lpAddLyricsBegin
      ++ lpChordCodeListTest1
      ++ lpAddLyricsEnd
      ++ lpOutputFooter

-- ========== ========== ========== ==========
-- LILYPOND TEST #2
-- ========== ========== ========== ==========
mkAbsPitchesC_C4 = mkAbsPitchesC (C, 4)

lpChordListTest2 = unlines chords2
    where chords  = map (indent3 . (absPitches2LpChord 3) . mkAbsPitchesC_C4) commonChordCodes
          chords2 = map (intercalate "") $ chunksOf 2 chords

lpChordCodeListTest2
    = lpChordCodeListTest1

lpOutputTest2
    = lpHeader
      ++ lpAbsoluteBegin
      ++ lpKeyTempo
      -- ++ lpChordmodeBegin
      ++ lpChordListTest2
      -- ++ lpChordmodeEnd
      ++ lpAbsoluteEnd
      ++ lpAddLyricsBegin
      ++ lpChordCodeListTest2
      ++ lpAddLyricsEnd
      ++ lpOutputFooter

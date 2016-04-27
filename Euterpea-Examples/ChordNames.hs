-- module Music.Chord.Names where

import Data.List
import Data.List.Split

import Euterpea

type ScaleIntervals = [Int]
type MInterval = Int

-- TODO: Refactor mival into concepts
mival :: String -> MInterval
mival "u"      = 0
mival "m2"     = 1
mival "M2"     = 2
mival "m3"     = 3
mival "M3"     = 4
mival "p4"     = 5
mival "d5"     = 6
mival "a4"     = 6   -- synonym
mival "p5"     = 7
mival "a5"     = 8
mival "m6"     = 8   -- synonym
mival "M6"     = 9
mival "d7"     = 9   -- synonym
mival "m7"     = 10
mival "M7"     = 11
mival "o"      = 12
mival "m9"     = 13
mival "M9"     = 14
mival "a9"     = 15
mival "M10"    = 16
mival "p11"    = 17
mival "a11"    = 18
mival "p12"    = 19
mival "m13"    = 20
mival "M13"    = 21
mival "a13"    = 22
mival "m14"    = 22  -- synonym
mival "M14"    = 23
mival "p15"    = 24

-- Note: MRelChord does not include an interval representing the base note.
type MRelChord     = [MInterval]
type MAbsChord     = (Pitch, MRelChord)
type MIntervalName = String
type ChordCode     = String

-- E.g., Major chord (w/o base pitch) = mivals [ival "M3", ival "p5"]
mivals :: [MIntervalName] -> MRelChord
mivals ivalNames = map mival ivalNames

-- E.g., mrchord    "maj7" = mrchord   ival "5" "M7"
mrchordIval :: ChordCode -> MIntervalName -> MRelChord
mrchordIval chrdCode mivalName = (mrchord chrdCode) ++ [mival mivalName]

-- The chord "codes" below are taken from Lp docs (Appendix A.2)
--   with the power chord codes being modified slightly.
-- See http://lilypond.org/doc/v2.19/Documentation/notation/common-chord-modifiers
--   * Errata on that web page:
--       - The Lp code for the 2-note power chord should read \powerChords c1:1.5
--       - The Lp code for the 3-note power chord should read \powerChords c1:1.5.8
-- TODO: Refactor mrchord and mrchordName into concepts
mrchord :: ChordCode -> MRelChord
mrchord chordCode = case chordCode of
    "5"        -> mivals ["M3", "p5"]
    ""         -> mrchord ""    -- synonym
    "m5"       -> mivals ["m3", "p5"]
    "m"        -> mrchord "m5"  -- synonym
    "aug"      -> mivals ["M3", "a5"]
    "dim"      -> mivals ["m3", "d5"]
    "7"        -> mrchordIval "5"     "m7"
    "maj7"     -> mrchordIval "5"     "M7"
    "m7"       -> mrchordIval "m"     "m7"
    "dim7"     -> mrchordIval "dim"   "d7"
    "aug7"     -> mrchordIval "aug"   "m7"
    "m7.5-"    -> mrchordIval "dim"   "m7"
    "7+"       -> mrchordIval "m"     "M7"
    "6"        -> mrchordIval "5"     "M6"
    "m6"       -> mrchordIval "m5"    "M6"
    "9"        -> mrchordIval "7"     "M9"
    "maj9"     -> mrchordIval "maj7"  "M9"
    "m9"       -> mrchordIval "m7"    "M9"
    "11"       -> mrchordIval "9"     "p11"
    "maj11"    -> mrchordIval "maj9"  "p11"
    "m11"      -> mrchordIval "m9"    "p11"
    "13"       -> mrchordIval "9"     "M13"
    "13.11"    -> mrchordIval "11"    "M13"
    "maj13.11" -> mrchordIval "maj11" "M13"
    "m13.11"   -> mrchordIval "m11"   "M13"
    "sus2"     -> mivals ["M2", "p5"]
    "sus4"     -> mivals ["p4", "p5"]
    "pow1.5"   -> mivals ["p5"]
    "pow1.5.8" -> mivals ["p5", "o"]

mrchordName name = case name of
    "5"        -> "Major"
    ""         -> mrchordName "5"   -- synonym
    "m5"       -> "Minor"
    "m"        -> mrchordName "m5"  -- synonym
    "aug"      -> "Augmented"
    "dim"      -> "Diminished"
    "7"        -> "Dominant seventh"
    "maj7"     -> "Major seventh"
    "m7"       -> "Minot seventh"
    "dim7"     -> "Diminished seventh"
    "aug7"     -> "Augmented seventh"
    "m7.5-"    -> "Half-diminished seventh"
    "7+"       -> "Minor-major seventh"
    "6"        -> "Major sixth"
    "m6"       -> "Minor sixth"
    "9"        -> "Dominant ninth"
    "maj9"     -> "Major ninth"
    "m9"       -> "Minor ninth"
    "11"       -> "Dominant eleventh"
    "maj11"    -> "Major eleventh"
    "m11"      -> "Minor eleventh"
    "13"       -> "Dominant thirteenth"
    "13.11"    -> "Dominant thirteenth ???"
    "maj13.11" -> "Major thirteenth"
    "m13.11"   -> "Minor thirteenth"
    "sus2"     -> "Suspended second"
    "sus4"     -> "Suspended fourth"
    "pow1.5"   -> "Power chord (two-voiced)"
    "pow1.5.8" -> "Power chord (three-voiced)"

mkChordAbsPitches :: Pitch -> ChordCode -> [AbsPitch]
mkChordAbsPitches p code = aps
    where ap0 = absPitch p
          aps = ap0 : (map (ap0 +) (mrchord code))

-- mkChord: Make a chord, using a ChordCode
mkChord :: Dur -> Pitch -> ChordCode -> Music Pitch
mkChord d p code = chord notes
    where aps = mkChordAbsPitches p code
          toNote ap = note d (pitch ap)
          notes = map toNote aps

-- type ChordRoman = String
-- mkChordRoman: Make a chord, using a Roman numeral
-- mkChordRoman :: Dur -> MScale -> ChordRoman -> Music

-- ========== ========== ========== ==========
-- TEST CODE
-- ========== ========== ========== ==========
commonChordCodes
    = [ "5", "m5", "aug", "dim"
      , "7", "maj7", "m7", "dim7", "aug7", "m7.5-", "7+"
      , "6", "m6"
      , "9", "maj9", "m9"
      , "11", "maj11", "m11"
      , "13", "13.11", "maj13.11", "m13.11"
      , "sus2", "sus4"
      , "pow1.5", "pow1.5.8"
      ]

mkChord_en_C4 = mkChord en (C,4)
commonChords_en_C4 = line $ map mkChord_en_C4 commonChordCodes

pbChord (pc, oct, cc) = mkChord qn (pc,oct) cc
-- pachelbel = I-V-vi  -  iii-IV-I  -  IV-V-I
pachelbel = line $ map pbChord [ (C,4,"5"),  (G,4,"5"), (A,4,"m5")
                               , (E,4,"m5"), (F,4,"5"), (C,4,"5")
                               , (F,4,"5"),  (G,4,"5"), (C,4,"5")
                               ]

-- ========== ========== ========== ==========
-- LILYPOND SETUP
-- ========== ========== ========== ==========
lpHeader           = unlines
    [ "\\version \"2.18.2\""
    , "\\layout { indent = 0.0 }"
    , "<<"
    ]
lpAbsoluteBegin    = unlines
    [ "  \\absolute {"
    ]
lpKeyTempo         = unlines
    [ "    \\key c \\major"
    , "    \\time 4/4"
    ]
lpChordmodeBegin   = unlines
    [ "    \\chordmode {\n"
    ]
lpChordList        = unlines
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
lpChordmodeEnd     = "    }\n"
lpAbsoluteEnd      = unlines
    [ "  }"
    ]
lpAddLyrics        = unlines
    [ "  \\addlyrics {"
    ]
lpChordCodeList = unlines
    [ "  \"5\"     \"m5\"     \"m\"         \"aug\""
    , "  \"dim\"   \"7\"      \"maj7\"      \"m7\""
    , "  \"dim7\"  \"aug7\"   \"m7.5-\"     \"7+\""
    , "  \"6\"     \"m6\"     \"9\"         \"maj9\""
    , "  \"m9\"    \"11\"     \"maj11\"     \"m11\""
    , "  \"13\"    \"13.11\"  \"maj13.11\"  \"m13.11\""
    , "  \"sus2\"  \"sus4\"   \"pow1.5\"    \"pow1.5.8\""
    ]
lpOutputFooter     = unlines
    [ "  }"
    , ">>"
    , "\\layout { }"
    ]

lpOutputReference
    = lpHeader
      ++ lpAbsoluteBegin
      ++ lpKeyTempo
      ++ lpChordmodeBegin
      ++ lpChordList
      ++ lpChordmodeEnd
      ++ lpAbsoluteEnd
      ++ lpAddLyrics
      ++ lpChordCodeList
      ++ lpOutputFooter

-- ========== ========== ========== ==========
-- LILYPOND TEST #1
-- ========== ========== ========== ==========
chordCode2Input :: ChordCode -> String
chordCode2Input "pow1.5"   = "\\powerChords_c1:1.5"
chordCode2Input "pow1.5.8" = "\\powerChords_c1:1.5.8"
chordCode2Input code       = "c1:" ++ code

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
      ++ lpAddLyrics
      ++ lpChordCodeListTest1
      ++ lpOutputFooter

-- ========== ========== ========== ==========
-- LILYPOND TEST #2
-- ========== ========== ========== ==========
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

-- These yield the correct frequencies, but display all non-scale notes as sharps.
absPitches2LpChord :: Octave -> [AbsPitch] -> String
absPitches2LpChord baseOct aps = "<" ++ notes ++ ">1"
    where notes = intercalate " " $ map (absPitch2LpNote baseOct) aps

mkChordAbsPitches_C4 = mkChordAbsPitches (C, 4)

lpChordListTest2 = unlines chords2
    where chords  = map (indent3 . (absPitches2LpChord 3) . mkChordAbsPitches_C4) commonChordCodes
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
      ++ lpAddLyrics
      ++ lpChordCodeListTest2
      ++ lpOutputFooter

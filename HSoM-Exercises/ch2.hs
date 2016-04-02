-- Haskell School of Music, Chapter 2

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 2.1 =====
t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

-- twoFiveOne Construct a ii-V-I chord progression
--     in the key whose major scale begins on the pitch p.
-- Major scale: whole, whole, half, whole, whole, whole, half
--   * chord_ii is one whole step up from p, which is 2 semitones.
--   * chord_V is 3 whole steps + one half-step up from p, or 7 semitones.
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = chord_ii :+: chord_V :+: chord_I
  where
    chord_ii, chord_V, chord_I :: Music Pitch
    chord_ii = minorChord d     (trans 2 p)
    chord_V  = majorChord d     (trans 7 p)
    chord_I  = majorChord (2*d) p
    minorChord, majorChord :: Dur -> Pitch -> Music Pitch
    minorChord d pBase = (Prim $ Note d pBase)
                         :=: (Prim $ Note d ((trans 3) pBase))
                         :=: (Prim $ Note d ((trans 7) pBase))
    majorChord d pBase = (Prim $ Note d pBase)
                         :=: (Prim $ Note d ((trans 4) pBase))
                         :=: (Prim $ Note d ((trans 7) pBase))

-- play t251
t251' = twoFiveOne (C, 4) wn
-- Test: twoFiveOne (C, 4) wn = t251

-- ===== Exercise 2.2 =====
--
-- Part 1
data BluesPitchClass = Ro -- Root
                     | MT -- Minor Third
                     | Fo -- Fourth
                     | Fi -- Fifth
                     | MS -- Minor Seventh

-- Part 2
type BluesPitch = (BluesPitchClass, Octave)

-- Part 3
ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro oct d = note d (Ro, oct)
mt oct d = note d (MT, oct)
fo oct d = note d (Fo, oct)
fi oct d = note d (Fi, oct)
ms oct d = note d (MS, oct)

-- Part 4.  Convert to Music Pitch
fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (Ro, oct))) = Prim (Note d (C, oct))
fromBlues (Prim (Note d (MT, oct))) = Prim (Note d (D, oct))
fromBlues (Prim (Note d (Fo, oct))) = Prim (Note d (E, oct))
fromBlues (Prim (Note d (Fi, oct))) = Prim (Note d (G, oct))
fromBlues (Prim (Note d (MS, oct))) = Prim (Note d (A, oct))
fromBlues (Prim (Rest d  ))         = Prim (Rest d)
fromBlues (m1 :+: m2)               = fromBlues m1 :+: fromBlues m2
fromBlues (m1 :=: m2)               = fromBlues m1 :=: fromBlues m2
fromBlues (Modify mod m)            = Modify mod $ fromBlues m

-- Part 5
bluesMelody_part1, bluesMelody_part2 :: Music BluesPitch
bluesMelody_part1 = ro 4 qn :+: fo 4 qn :+: mt 4 en :+: ms 4 en :+: fo 3 en :+: fi 3 en
bluesMelody_part2 = ro 3 qn :+: fi 4 hn                         :+: ro 3 qn

bluesMelody :: Music Pitch
bluesMelody       = fromBlues $ bluesMelody_part1 :=: bluesMelody_part2
-- play bluesMelody

-- ===== Exercise 2.3 =====

-- ===== Exercise 2.4 =====

-- ===== Exercise 2.5 =====

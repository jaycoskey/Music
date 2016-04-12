-- Haskell School of Music, Chapter 2

import Euterpea
import Euterpea.Music
import HSoM

-- ========== Exercise 2.1 ==========
-- Ex: Define a function twoFiveOne that constructs a ii-V-I chord progression
--     in a major scale starting at a given pitch, p, with durations d, d, and 2*d.

-- Code from HSoM, Chapter 2:
t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

-- Code for this exercise:
-- Notes to self:
--   * Major scale: whole, whole, half, whole, whole, whole, half
--   * chord_ii is based one whole step up from p, which is 2 semitones.
--   * chord_V is based 3 whole steps + one half-step up from p, or 7 semitones.
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

music_2_1 = twoFiveOne (C, 4) wn
test_2_1 = play $ t251 :+: wnr :+: music_2_1  -- Test: Both phrases should sound identical.

-- ===== Exercise 2.2 =====
-- Code for this exercise:
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

-- Part 5: Test music
bluesMelody_part1, bluesMelody_part2 :: Music BluesPitch
bluesMelody_part1 = ro 4 qn :+: fo 4 qn :+: mt 4 en :+: ms 4 en :+: fo 3 en :+: fi 3 en
bluesMelody_part2 = ro 3 qn :+: fi 4 hn                         :+: ro 3 qn

bluesMelody :: Music Pitch
bluesMelody       = fromBlues $ bluesMelody_part1 :=: bluesMelody_part2

-- Test code
test_2_2 = play bluesMelody

-- ===== Exercise 2.3 =====
-- scale = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
-- notesPerScale = length scale
-- abspitch (pitch ap) = abspitch $ (scale !! (ap `mod` notesPerScale), ap `div` 12)
--     = 12 * (ap `div` 12) + pcToInt (ap `mod` notesPerScale)
--     = ap

-- pitch (abspitch (pc, oct))
--     = pitch(12 * oct + pcToInt pc)
--     = (scale !! (pcToInt pc), ap `div` 12)
--     = (pc, (12 * oct) `div` 12)
--     = (pc, oct)

-- ===== Exercise 2.4 =====
-- trans i (trans j p)
--     = trans i (pitch (absPitch p + j))
--     = trans i (scale !! ((absPitch (p + j) `mod` notesPerScale)), (absPitch (p + j) `div` 12))
--     = ...
-- Some arithmetic is omitted, but the motivation is that trans is a linear translation,
-- so the effect of composed operations is additive.

-- ===== Exercise 2.5 =====
-- Ex. Write a function that takes a Transpose "modification" and actually transforms the music
--     by applying the change in pitch to the actual notes.
-- Code for this exercise:
transM :: AbsPitch -> Music Pitch -> Music Pitch
transM t (Prim (Note d p))          = Prim (Note d (pitch ((absPitch p) + t)))
transM t (Prim (Rest d))            = Prim (Rest d)
transM t (m1 :+: m2)                = (transM t m1) :+: (transM t m2)
transM t (m1 :=: m2)                = (transM t m1) :=: (transM t m2)
transM t (Modify (Transpose t') m') = transM (t + t') m'
transM t (Modify mod m')            = Modify mod (transM t m')

applyTranspose :: Music Pitch -> Music Pitch
applyTranspose mus = transM (0::AbsPitch) mus

-- Test music
music_2_5_a = Modify (Transpose 12) $ line [g 2 qn, e 3 qn, c 3 qn]
music_2_5_b = applyTranspose music_2_5_a

-- Test code
test_2_5 = play $ music_2_5_a :+: hnr :+: music_2_5_b  -- Test: Both phrases should sound identical. 

-- Haskell School of Music, Chapter 6

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 6.1 =====

-- ===== Exercise 6.2 =====

-- ===== Exercise 6.3 =====

-- ===== Exercise 6.4 =====

-- ===== Exercise 6.5 =====

-- ===== Exercise 6.6 =====

-- ===== Exercise 6.7 =====

-- ===== Exercise 6.8 =====

-- ===== Exercise 6.9 =====

-- ===== Exercise 6.10 =====

-- ===== Exercise 6.11 =====

-- ===== Exercise 6.12 =====

-- ===== Exercise 6.13 =====

-- ===== Exercise 6.14 =====
-- Exercise 6.14 Do something wild and crazy with Euterpea.
-- A few ideas, saved here for later.
--
-- (1) Create a composition that combines melodic (i.e., note-based) music
--     with soundscapes (i.e., full-spectrum music created with signals),
--     and glides between the two extremes.
--
-- (2) Create a validation framework that allows one to create validators
--     for pieces.  Some sample validators:
--      (a) Validate the ranges of notes by instrument.  For example, check
--          that the notes for a (modern) piano range from (A, 0) to (C, 8).
--      (b) Validate the number of notes by instrument.  E.g., check that
--          a violin is playing at most two notes at any given moment.
--          Implementation notes:
--            * filter by instrument
--            * Compute concurrency profile, [(Int, Dur)]
--                - Notes and Rests have concurrency 1 and 0, respectively. 
--                - (:+:) => Concatenation
--                - (:=:) => Merge profiles inductively, clipping durations
--                           as needed.
--      (c) Validate the placement of notes.  E.g., check that the notes
--          for a piano at any moment fall into two groups that can likely be
--          played with human hands.  A composition might meet simple selected
--          criteria (e.g., two 1-octave spans consisting of at most 5 notes
--          each) and still be impossible to play, but this would serve as a
--          valid check.
--
-- (3) Implement a distance function (viz. Damerau-Levenstein) between
--     pieces of music.
--
-- (4) Automatically identify repeated subsets of a composition, usually
--     done with a suffix tree.  See the wiki page "Longest repeated
--     substring problem".
--
-- (5) Modify Euterpea to support ties.
--     == The problem. ==
--     Euterpea does not output sheet music (a.k.a., "engravings"), though
--     Isaac Reilly has created a system called Heqet that converts 
--     Euterpea compositions to Lilypond format.  Lilypond can then be run
--     to create an engraving.  But Euterpea does not (currently) support
--     ties.  Heqet could add ties, say converting a dotted half note into
--     a tie between a half note and a quarter note.  But that would not
--     necessarily reflect the intent of the composer who wrote the piece
--     in Euterpea.
-- 
--     == The solution == 
--     [Involves a minor modification to Euterpea]
--
--     Euterpea has an enum called PhraseAttribute:
--         data PhraseAttribute = Dyn Dynamic
--                              | Tmp Tempo
--                              | Art Articulation
--                              | Orn Ornament
--             deriving (Show, Eq, Ord)
--
--     This enum could be extended with the addition of a Tie, which
--     would join notes in the "phrase" being modified.  The default
--     player, defPlayer, could join together consecutive notes within
--     the same phrase.  An engraving system (such as Heqet + Lilypond)
--     could preserve the distinct notes, and add a tie annotation to the
--     engraving.
--
-- (6) Modify Euterpea to support barred notes (e.g., eighths, sixteenths).
--     This could be done with a phrase annotation, like ties in (5).
--
-- (7) [Craziest idea] Named notes and regions.
--
--     I originally thought of this in the context of implementing ties.
--     One (overengineered) way to implement ties would be to name notes
--     by using the type-parameterization of "Music a" to have named
--     music values, with type
--         Music (b, String)
--     In the case of named notes, it would look like this:
--         note12d, note13a :: Music Pitch
--         note12d      = Prim $ Note qn (C, 4)
--         note13a      = Prim $ Note en (C, 4)
--         mus1_named   = Modify (MName "note12d") note12d
--         mus2_named   = Modify (MName "note13a") note13a
--         mus_1_2      = line [mus1_named, mus2_named]
--         mus_1_2_tied = Modify (Tie "m12d" "m13a") mus
--     where Tie could be a constructor for a new enum called MLink.
--
--     In order for a Player to process this correctly, you'd need
--     information to be preserved along the recursive handling of
--     Music values.  This can be done by adding fields to the Context
--     record, such as cMNames and cMLinks.  This seems like an overkill
--     for an implementation of ties, but there are other ideas that
--     would benefit from named music values.  For example, an analysis
--     program that identifies duplicated or near-duplicated regions
--     could name those regions and make those named identifiers available
--     for other use later.  It seems there would be some application,
--     even if it wouldn't be a terribly compelling one.


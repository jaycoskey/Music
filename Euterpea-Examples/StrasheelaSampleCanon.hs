-- Sample canon found by a Strasheela program.  Transcription into Euterpea by Jay Coskey
-- From the paper
--   Strasheela: Design and Usage of a Music Composition Environment Based on the Oz Programming Model
-- by Torsten Anders, Christina Anagnostopoulou, and Michael Alcorn
-- http://cmr.soc.plymouth.ac.uk/tanders/publications/TorstenAnders-MOZ2004.pdf

module StrasheelaSampleCanon where

import Euterpea
import Euterpea.Music

common = line $  [g 4 hn, e 4 hn]
              ++ [d 4 hn, e 4 qn, g 4 qn]
              ++ [a 4 hn, g 4 hn]
              ++ [e 4 qn, g 4 en, e 4 en, f 4 en, d 4 en, b 3 qn]
              ++ [d 4 hn, c 4 hn]

treb = wnr :+: common
bass = (transpose (-7) common) :+: (line $ [b 3 hn, c 4 hn])
canon = treb :=: bass
-- Haskell School of Music, Chapter 4

import Euterpea
import Euterpea.Music
import HSoM

-- ===== Exercise 4.1 =====
-- Notes repeated in series
repd d = \ns -> line $ map (\x -> x d) ns
wns = repd wn
hns = repd hn
qns = repd qn
ens = repd en
bqns = qns -- Barred quarter-notes
bens = ens -- Barred eighth-notes

-- Notes repeated in parallel (intervals & chords)
chordd d = \ns -> chord $ map (\x -> x d) ns
chns = chordd hn
cqns = chordd qn
cens = chordd en

-- Other forms of repetition
twice m = m :+: m
repeatWithEnd2 common end1 end2 = common :+: end1 :+: common :+: end2

-- DONE: Add key signature.  One sharp => G major (E minor)
-- TODO: Add additional chords from score.
-- TODO: Add effect of accidental:natural to F quarter notes in measures #8 and #31.

-- TITLE: In The Mood.  Words and Music by Joe Garland.  Arranged by Phil Guberman
itm_treb_1_4 = bens [g 3, b 3, d 4, g 4] :+: g 4 en :+: g 4 dqn
               :+: bens [a 3, c 4, e 4, e 4] :+: e 4 en :+: e 4 dqn
               :+: bens [g 3, b 3, d 4, g 4] :+: g 4 qn :+: (g 4 den :+: f 4 sn)
               :+: d 4 qn :+: qnr :+: hnr
itm_bass_1_4 = wns [g 2, a 2, b 2]
               :+: (d 3 den :+: d 3 sn) :+: qns [c 3, b 2, a 2]

whosTheLivin = bens [b 3, d 4, g 4, b 3] :+: bens [d 4, g 4, b 3, d 4]
               :+: bens [g 4, b 3, d 4, g 4] :+: g 4 qn :+: qnr

itm_treb_5_8 = twice whosTheLivin
itm_bass_5_8 = hns [g 2, b 2]
               :+: d 3 hn :+: qns [e 3, d 3]
               :+: hns [g 2, b 2]
               :+: d 3 hn :+: g 3 qn :+: {-NATURAL-} f 3 qn

itm_treb_9_12 = bens [c 4, e 4, g 4, c 4] :+: bens [e 4, g 4, c 4, e 4]
                :+: bens [g 4, c 4, e 4, g 4] :+: g 4 qn :+: qnr
                :+: whosTheLivin
itm_bass_9_12 = c 3 hn :+: e 3 hn
                :+: c 3 hn :+: c 3 qn :+: e 3 qn
                :+: g 3 hn :+: e 3 hn
                :+: d 3 hn :+: b 2 qn :+: g 2 qn

itm_treb_13_15 = bens [d 4, f 4, a 4, d 4] :+: bens [f 4, a 4, d 4, f 4]
                 :+: bens [a 4, d 4, f 4, a 4] :+: a 4 en :+: g 4 qn :+: g 4 en
                 :+: twice (bens [g 4, g 4, g 4, g 4])
itm_bass_13_15 = hns [d 3, c 3] :+: hns [b 2, a 2] :+: g 2 hn :+: c 3 qn :+: e 3 qn

itm_treb_16a = twice (g 4 en :+: g 4 qn) :+: qnr
itm_bass_16a = ef 3 hn :+: g 3 qn :+: d 3 qn

itm_treb_16b = itm_treb_16a
itm_bass_16b = itm_bass_16a

-- ========== ========== ========== ==========
-- SECOND HALF
-- ========== ========== ========== ==========
inTheMood_treb = g 4 qn :+: (e 4 en :+: g 4 (en + qn)) :+: qnr
thatsWhat_treb = denr :+: c 4 sn :+: (e 4 den :+: g 4 sn) :+: b 4 en :+: b 4 dqn
itm_treb_17_20 = twice $ inTheMood_treb :+: thatsWhat_treb
inTheMood_bass = b 3 qn :+: b 3 qn :+: bf 3 qn :+: b 3 qn
thatsWhat_bass = qns [a 3, a 3, d 3, d 3]
itm_bass_17_20 = inTheMood_bass
                 :+: thatsWhat_bass
                 :+: inTheMood_bass
                 :+: qns [a 3, g 3, d 3, d 3]

d5enx4 = bens [d 5, d 5, d 5, d 5]
myHeart_treb   = denr :+: c 4 sn :+: (e 4 den :+: g 4 sn) :+: b 4 en :+: d 4 qn :+: b 4 en
itm_treb_21_24 = cqns [d 4, g 4] :+: (e 4 en :+: g 4 (en + qn)) :+: qnr
                 :+: myHeart_treb
                 :+: (d5enx4 :=: f 4 hn) :+: (d5enx4 :=: af 4 hn)
                 :+: d 5 qn :+: (bf 4 en :+: a 4 (en + qn)) :+: qnr
itm_bass_21_24 = inTheMood_bass
                 :+: thatsWhat_bass
                 :+: chns [d 3, c 4] :+: chns [d 3, b 3]
                 :+: cqns [d 3, c 4] :+: cqns [ef 3, df 4] :+: cqns [d 3, c 4] :+: f 3 qn

itm_treb_25_28 = inTheMood_treb
                 :+: denr :+: c 4 sn :+: bens [e 4, g 4] :+: b 4 en :+: b 4 dqn
                 :+: inTheMood_treb
                 :+: thatsWhat_treb
itm_bass_25_28 = inTheMood_bass :+: thatsWhat_bass :+: inTheMood_bass
                 :+: qns [a 3, a 3, d 3, f 3]

itm_treb_29_31 = inTheMood_treb
                 :+: myHeart_treb
                 :+: (d5enx4 :=: f 4 hn) :+: (d5enx4 :=: {-NATURAL-} f 4 hn)
itm_bass_29_31 = inTheMood_bass
                 :+: qns [a 3, a 3, d 3, f 3]
                 :+: chns [d 3, c 4] :+: chns [d 3, b 3]

itm_treb_32a = d 5 qn :+: bf 4 qn :+: a 4 (qn + qn)
itm_bass_32a = cqns [d 3, c 4] :+: cqns [ef 3, df 4]
                 :+: cqns [g 3, c 4] :+: f 3 qn

itm_treb_32b = d 5 qn :+: (bf 4 en :+: a 4 (en + en) :+: g 4 en) :+: qnr
itm_bass_32b = cqns [d 3, c 4] :+: (cens [ef 3, df 4] :+: cqns [d 3, c 4] :+: cens [g 3, b 3]) :+: qnr

instr_treb = instrument AcousticGrandPiano  -- Trumpet
instr_bass = instrument AcousticGrandPiano  -- Trombone

zipStaves :: [Music a] -> [Music a] -> Music a
zipStaves trebs basses =
    line $ zipWith (:=:)
                   (map instr_treb trebs)
                   (map instr_bass basses)

itm_1_4 = zipStaves [itm_treb_1_4] [itm_bass_1_4]
itm_5_15 = zipStaves
               [itm_treb_5_8, itm_treb_9_12, itm_treb_13_15]
               [itm_bass_5_8, itm_bass_9_12, itm_bass_13_15]

itm_16a   = zipStaves [itm_treb_16a] [itm_bass_16a]
itm_16b   = zipStaves [itm_treb_16b] [itm_bass_16b]

itm_17_31 = zipStaves
                [itm_treb_17_20, itm_treb_21_24, itm_treb_25_28, itm_treb_29_31]
                [itm_bass_17_20, itm_bass_21_24, itm_bass_25_28, itm_bass_29_31]

itm_32a   = zipStaves [itm_treb_32a] [itm_bass_32a]
itm_32b   = zipStaves [itm_treb_32b] [itm_bass_32b]

itm = Modify (Tempo (90/60)) $ Modify (KeySig G Major)
        $ itm_1_4
          :+: repeatWithEnd2 itm_5_15  itm_16a itm_16b
          :+: repeatWithEnd2 itm_17_31 itm_32a itm_32b

itm_short = Modify (Tempo (180/120)) $ Modify (KeySig G Major)
        $ itm_1_4 :+: itm_5_15 :+: itm_16b :+: itm_17_31 :+: itm_32b

-- ===== Exercise 4.2 =====
prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x : xs) = let f pf = x : pf
                    in [x] : map f (prefixes xs)

-- prefixes "Hello" => ["H", "He", "Hel", "Hell", "Hello"]

prefix :: [Music a] -> Music a
prefix mels = m :+: transpose 5 m :+: m
  where m1 = line (concat (prefixes mels))
        m2 = transpose 12 (line (concat (prefixes (reverse mels))))
        m  = instrument Flute m1 :=: instrument VoiceOohs m2

mel1 = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]
closeEncounters  = [g 3 qn, a 3 qn, f 3 qn, f 2 qn, c 3 qn]
closeEncounters' = map (transpose 12) closeEncounters

-- play $ line mel1
-- play $ line mel2
-- play $ line closeEncounters

-- play $ prefix mel1
-- play $ prefix mel2
-- play $ prefix closeEncounters

-- ===== Exercise 4.3 =====
f_m1_HSoM, f_m2_HSoM :: [Music a] -> Music a
f_m1_HSoM mels = line (concat (prefixes mels))
f_m2_HSoM mels = transpose 12 (line (concat (prefixes (reverse mels))))
type MusicAgg a = [Music a] -> Music a

prefix_poly
    ::  MusicAgg a -> MusicAgg a -> Rational -> InstrumentName -> InstrumentName
    -> [Music a] -> Music a
prefix_poly f_m1 f_m2 pDelay instr1 instr2  mels = m :+: transpose 5 m :+: m
  where m1 = line (concat (prefixes mels))
        m2 = transpose 12 (line (concat (prefixes (reverse mels))))
        m  = instrument instr1 m1
                 :=: ((Prim $ Rest (pDelay * (dur m1))) :+: instrument instr2 m2)

-- Part 1
-- In the above function, prefix_poly, both instruments are parameterized.

-- Part 2
-- In the function prefix_poly:
--   When pDelay = 0, m1 and m2 are added in parallel, as before.
--   When pDelay = 1, m1 and m2 are added in series.
--   For values of pDelay inbetween 0 and 1, m1 and m2 are in parallel, but shifted.

-- Part 3
-- In the function prefix_poly, a delay of pDelay * (dur m1) is inserted in the defn of m.

f_m1_test, f_m2_test :: [Music a] -> Music a
f_m1_test mels = line $ concat $ take 5 $ drop 3 $ cycle $ prefixes mels
f_m2_test mels = transpose 12 $ line $ concat $ take 5 $ drop 3 $ cycle $ prefixes $ reverse mels
prefix_test = prefix_poly f_m1_test f_m2_test (1/3) Bagpipe Trumpet closeEncounters

-- play prefix_test -- Note: Sounds hideous, doesn't it?

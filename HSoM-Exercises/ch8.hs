-- Haskell School of Music, Chapter 8

import System.IO.Unsafe
import Debug.Trace

import Euterpea
import Euterpea.Music

import HSoM

-- ===== Exercise 8.1 =====
-- Note to self: playA defPMap defCon <music_a>

-- From Euterpea-Music-Note-Performance.lhs:
-- defPasHandler (Dyn (Accent x))    = 
--     map (\e -> e {eVol = round (x * fromIntegral (eVol e))})

myPasHandler :: PhraseAttribute -> Performance -> Performance
-- See Figure 8.6
myPasHandler (Dyn (Crescendo x)) perf = boostVolsBy x
    where t0 = eTime (head perf)
          perfDur :: Dur
          perfDur =  sum $ map eDur perf
          propTime :: PTime -> Rational
          propTime t = (t - t0) / perfDur
          propVolDelta :: PTime -> Rational
          propVolDelta t = x * (propTime t)
          inRange mn mx x = min mx $ max mn x
          boostMEventVol (e@MEvent {eTime = t, eVol = v})
              = e { eVol = inRange 127 127  -- TODO: Revert to 0 127 after bug fixed
                                   (trace ( -- TODO: No output to the console window
                                            "Vol="
                                            ++ show (round((1 + (propVolDelta t)) * (fromIntegral v)))
                                            ++ "\n"
                                          )
                                          (round((1 + (propVolDelta t)) * (fromIntegral v)))
                                    )
                  }
          boostVolsBy :: Rational -> [MEvent]
          boostVolsBy x = map boostMEventVol perf
myPasHandler pa                    pf = defPasHandler pa pf

myPlayer = MkPlayer { pName        = "CustCresc"
                    , playNote     = defPlayNote defNasHandler
                    , interpPhrase = defInterpPhrase defPasHandler
                    --, notatePlayer = ()
                    }
myPMap :: PlayerName ->  Player Note1
myPMap "CustCresc" = myPlayer
myPMap p           = defPMap p

custCresc :: Music a -> Music a
custCresc mus = Modify (Phrase [Dyn (Crescendo (5/4))]) mus
aps           = [30, 70]
gamut_dur     = wn
-- aps        = [30 .. 70]
-- gamut_du   = sfn
gamut         = addVolume 127 $ line $ map ap2Note aps
                  where ap2Note = \ap -> Prim $ Note gamut_dur $ pitch ap
gamut_cresc   = custCresc gamut
test_8_1      = gamut :+: gamut_cresc

-- playA myPMap defCon test_8_1

-- TODO: Fix a bug so that the following expression plays two sequences (gamut & gamut_cresc),
--       with the second being louder than the first.
--       Currently, all notes of gamut are audible,
--       but only some the notes of gamut_cresc are audible,
--       with the exact # being audible depending on the Crescendo value.
--       The Crescendo value of 10,000 seems to have a special significance.

-- ===== Exercise 8.2 =====
-- Choose some of the other phrase attributes and provide interpretations for them.
-- Note: You might need to access the cKey field.
-- TODO: Ritardando

-- ===== Exercise 8.3 =====
-- Define a play myPlayer that appropriately handles the Pedal articulation
-- and both the ArpeggioUp and ArpeggioDown ornamentation.
-- You should define myPlayer as a derivative of defPlayer or newPlayer.

-- ===== Exercise 8.4 =====
-- Define a player jazzPlayer that plays a melody using a jazz "swing" feel.
--   * en :+: en => tempo (3/2) $ qn :+: en
-- Note: Instead of being implemented as a PhraseFun, this can be implemented as a NoteFun
--     that uses the cTime and cDur in the context to distinguish downbeat from upbeat.

-- ===== Exercise 8.5 =====
-- Implement the ornamentation DiatonicTrans, which is a "diatonic transposition" within
-- a particular key.  The argument to DiatonicTrans is an integer representing the number of
-- scale degrees to do the transposition.
--   diatonicTrans (C, Major) (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: f 4 en  :+: g 4 en
--   diatonicTrans (G, Major) (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: fs 4 en :+: g 4 en
-- Hint: You will need to use cKey to access the key from the context .


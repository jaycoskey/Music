-- Haskell School of Music, Chapter 8

import Euterpea
import Euterpea.Music

import HSoM
-- ===== Exercise 8.1 =====
-- Note to self: playA defPMap defCon <music_a>

-- From Euterpea-Music-Note-Performance.lhs:
-- defPasHandler (Dyn (Accent x))    = 
--     map (\e -> e {eVol = round (x * fromIntegral (eVol e))})

myPasHandler :: PhraseAttribute -> Performance -> Performance
myPasHandler (Dyn (Crescendo x)) pf = map boostVolume pf
    where boostVolume = \e -> e { eVol = eVol e {- round((1+x) * fromIntegral(eVol e)) -} }
myPasHandler pa                  pf = defPasHandler pa pf

myPlayer = MkPlayer { pName        = "CustCresc"
                    , playNote     = defPlayNote defNasHandler
                    , interpPhrase = defInterpPhrase defPasHandler
                    --, notatePlayer = ()
                    }
myPMap :: PlayerName ->  Player Note1
myPMap "CustCresc" = myPlayer
myPMap p           = defPMap p

custCresc :: Music a -> Music a
custCresc mus = Modify (Phrase [Dyn (Crescendo (1/1))]) mus
gamut = line $ map (\ap -> Prim $ Note en $ pitch ap) [30, 35 .. 70]
gamut_cresc = custCresc gamut
test_8_1  = gamut :+: gamut_cresc

-- TODO: Fix a bug so that the following expression plays two sequences,
--       with the second being louder than the first.
--       Currently, only the first note of gamut_cresc is audible,
--       even when the volume level (eVol) remains unchanged and the Crescendo value is 1/1.
-- playA myPMap defCon test_8_1

-- ===== Exercise 8.2 =====
-- Choose some of the other phrase attributes and provide interpretations for them.
-- Note: You might need to access the cKey field.

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


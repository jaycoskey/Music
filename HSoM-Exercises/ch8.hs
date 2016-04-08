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
myPasHandler (Dyn (Crescendo x)) performance = boostVolsBy x
    where t0 = eTime (head performance)
          perfDur :: Dur
          perfDur =  sum $ map eDur performance
          propTime :: PTime -> Rational
          propTime t = (t - t0) / perfDur
          propVolDelta :: PTime -> Rational
          propVolDelta t = x * (propTime t)
          -- inRange mn mx x = min mx $ max mn x
          newVol t v = round((1 + (propVolDelta t)) * (fromIntegral v))
          boostMEventVol (e@MEvent {eTime = t, eVol = v})
              = e { eVol = trace ("Vol=" ++ show (newVol t v))
                                 (newVol t v)
                  }
          boostVolsBy :: Rational -> [MEvent]
          boostVolsBy x = map boostMEventVol performance
myPasHandler pa                    pf = defPasHandler pa pf

type PasHandler = PhraseAttribute -> Performance -> Performance
-- type PhraseFun = (PMap a -> Context a -> [PhraseAttribute])
-- perf :: PMap a -> Context a -> Music a -> (Performance, DurT)
myInterpPhrase :: (PhraseAttribute -> Performance -> Performance) -- PasHandler
                  -> (PMap a -> Context a -> [PhraseAttribute] -> Music a -> (Performance, DurT))
myInterpPhrase pasHandler pm context pas m = (foldr pasHandler pf pas, durPerf)
    where (pf, durPerf) = perf pm context m

myPlayer = MkPlayer { pName        = "CustCresc"
                    , playNote     = defPlayNote    defNasHandler
                    , interpPhrase = myInterpPhrase myPasHandler
                    --, notatePlayer = ()
                    }
myPMap :: PlayerName -> Player Note1
myPMap "CustCresc" = myPlayer
myPMap p           = error "Use CustCresc player for testing"

myCon = defCon { cPlayer = myPlayer }

-- custCresc :: Music a -> Music a
custCresc mus = Modify (Phrase [Dyn (Crescendo (3/1))]) mus
-- aps           = [20, 80]
-- gamut_dur     = wn
aps           = replicate 32 60
gamut_dur     = sn
gamut         = line $ map ap2Note aps
                  where ap2Note = \ap -> Prim $ Note gamut_dur $ pitch ap
gamut_soft    = addVolume 31 $ gamut
gamut_cresc   = custCresc gamut_soft
test_8_1      = gamut_soft :+: gamut_cresc

-- Test: play               test_8_1
-- Test: playA myPMap myCon test_8_1

-- ===== Exercise 8.2 =====
-- Choose some of the other phrase attributes and provide interpretations for them.
-- Note: You might need to access the cKey field.

-- From Figure 8.4:
-- data PhraseAttribute = Dyn Dynamic
--                      | Tmp Tempo
--                      | Art Articulation
--                      | Orn Ornament

-- TODO: ===== Tempo -> Ritardando =====
pasHandler_slow :: PhraseAttribute -> Performance -> Performance
-- See Figure 8.6
pasHandler_slow (Tmp (Ritardando x)) performance = changeTempoBy x
    where t0 :: PTime
          t0 = eTime (head performance)
          -----
          perfDur  :: Dur
          perfDur    =  sum $ map eDur performance
          -----
          propTime,newTime :: PTime -> Rational
          propTime t = (t - t0) / perfDur
          -- inRange mn mx x = min mx $ max mn x
          newTime  t = x * (propTime t) * t
          -----
          changeMEventTempo :: MEvent -> MEvent
          changeMEventTempo (e@MEvent {eTime = t})
              = e { eTime = trace ("Vol=" ++ show (newTime t))
                                (newTime t)
                  }
          -----
          changeTempoBy :: Rational -> [MEvent]
          changeTempoBy x = map changeMEventTempo performance
pasHandler_slow pa                   performance = defPasHandler pa performance

-- Defined in Ex 8.1: type PasHandler = PhraseAttribute -> Performance -> Performance
-- type PhraseFun = (PMap a -> Context a -> [PhraseAttribute])
-- perf :: PMap a -> Context a -> Music a -> (Performance, DurT)
interpPhrase_slow :: (PhraseAttribute -> Performance -> Performance) -- PasHandler
                       -> (PMap a -> Context a -> [PhraseAttribute] -> Music a -> (Performance, DurT))
interpPhrase_slow pasHandler pm context pas m = (foldr pasHandler pf pas, durPerf)
    where (pf, durPerf) = perf pm context m

player_slow = MkPlayer { pName        = "CustSlow"
                       , playNote     = defPlayNote       defNasHandler
                       , interpPhrase = interpPhrase_slow pasHandler_slow
                       --, notatePlayer = ()
                       }
pMap_slow :: PlayerName -> Player Note1
pMap_slow "CustSlow" = myPlayer
pMap_slow p          = error "Use CustSlow player for testing"

con_slow = defCon { cPlayer = player_slow }

-- custCresc :: Music a -> Music a
custSlow mus = Modify (Phrase [Tmp (Ritardando (5/1))]) mus

-- Defined in Ex 8.1: aps       = replicate 32 60
-- Defined in Ex 8.1: gamut_dur = sn
-- Defined in Ex 8.1: gamut     = addVolume 31 $ line $ map ap2Note aps

gamut_loud = gamut
gamut_slow = custSlow gamut_loud
test_8_2 = gamut_loud :+: gamut_slow

-- TODO: ===== Articulation -> Pizzicato =====


-- ===== Exercise 8.3 =====
-- Define a play myPlayer that appropriately handles the Pedal articulation
-- and both the ArpeggioUp and ArpeggioDown ornamentation.
-- You should define myPlayer as a derivative of defPlayer or newPlayer.

-- TODO: Articulation -> Pedal
-- TODO: Ornament -> ArpeggioDown, ArpeggioUp

-- ===== Exercise 8.4 =====
-- Define a player jazzPlayer that plays a melody using a jazz "swing" feel.
--   * en :+: en => tempo (3/2) $ qn :+: en
-- Note: Instead of being implemented as a PhraseFun, this can be implemented as a NoteFun
--     that uses the cTime and cDur in the context to distinguish downbeat from upbeat.

-- type NoteFun a            = Context a            -> Dur -> a            -> Performance
-- .... NoteFun (Pitch, [a]) = Context (Pitch, [a]) -> Dur -> (Pitch, [a]) -> Performance
-- defPlayNote   :: (Context  (Pitch, [a]) -> a -> MEvent -> MEvent) -> NoteFun (Pitch, [a])
-- defNasHandler ::  Context a -> NoteAttribute -> MEvent -> MEvent (?)
{-
jazzPlayNote :: (Context  (Pitch, [a]) -> a -> MEvent -> MEvent) -> NoteFun (Pitch, [a])
jazzPlayNote
    nasHandler
    c@(Context cTime cPlayer cInst cDur cPch cVol cKey)
    d
    (p, nas) = [ foldr (nasHandler c) initEv nas ]
               where initEv = MEvent { eTime   = cTime
                                     , eInst   = cInst
                                     , eDur    = d * cDur
                                     , eVol    = cVol
                                     , ePitch  = absPitch p + cPch
                                     , eParams = []
                                     }

jazzPlayer :: Player (Pitch, [NoteAttribute])
jazzPlayer = MkPlayer { pName        = "JazzPlayer"
                      , playNote     = jazzPlayNote    defNasHandler
                      , interpPhrase = defInterpPhrase defPasHandler
                      --, notatePlayer = ()
                      }
jazzCon = defCon { cPlayer = jazzPlayer }

aps2ens :: [AbsPitch] -> Music Pitch
aps2ens aps = line $ map mkEn aps
    where mkEn = \ap -> Prim $ Note en $ pitch ap
mus_8_4 = aps2ens [40 .. 55]
-}
-- play mus_8_4
-- playA pMap_8_4 jazzCon mus_8_4

-- ===== Exercise 8.5 =====
-- Implement the ornamentation DiatonicTrans, which is a "diatonic transposition" within
-- a particular key.  The argument to DiatonicTrans is an integer representing the number of
-- scale degrees to do the transposition.
--   diatonicTrans (C, Major) (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: f 4 en  :+: g 4 en
--   diatonicTrans (G, Major) (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: fs 4 en :+: g 4 en
-- Hint: You will need to use cKey to access the key from the context.

-- TODO
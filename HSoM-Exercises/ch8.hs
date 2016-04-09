-- Haskell School of Music, Chapter 8

-- import System.IO.Unsafe
import Data.List
import Data.Ratio

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

aps           = replicate 32 60
notes_dur     = sn
notes         = line $ map ap2Note aps
                  where ap2Note = \ap -> Prim $ Note notes_dur $ pitch ap
notes_soft    = addVolume 31 $ notes
notes_cresc   = custCresc notes_soft
test_8_1      = notes_soft :+: notes_cresc

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

-- ===== Tempo -> Ritardando =====
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

-- Defined in Ex 8.1: notes
notes_loud = notes
notes_slow = custSlow notes_loud
test_8_2_a = notes_loud :+: notes_slow

-- ===== Articulation -> Pizzicato =====
-- TODO: Why is the first note in test_8_2_b2 silent when played Pizzicato?
-- TODO: Why is the second note in the result of shortenNote silent,
--         even without setting eVol = 0?

pasHandler_pizz :: PhraseAttribute -> Performance -> Performance
-- See Figure 8.6
pasHandler_pizz (Art Pizzicato) performance = newPerformance
    where -- Pizzicato changes an instrument's harmonics, but we ignore that here,
          --   and only model the change in note duration.
          -- Pizzicato changes notes in the phrase uniformly, so
          --   there's no need to track time elapsed within the phrase.
          shortenNote :: MEvent -> [MEvent]
          shortenNote e@MEvent{eDur = d, eVol = v} =
              [ e { eDur = trace ( "note dur=" ++ (show shortDur)
                                   ++ "; rest dur=" ++ (show restDur)
                                 )
                                 shortDur
                  , eVol = v
                  }
              , e { eDur = restDur }
              ]
              where shortDur = min sn d
                    restDur  = d - shortDur

          newPerformance = concat $ map shortenNote performance
pasHandler_pizz pa                  pf = defPasHandler pa pf

-- Defined in Ex 8.1: type PasHandler = PhraseAttribute -> Performance -> Performance
-- type PhraseFun = (PMap a -> Context a -> [PhraseAttribute])
-- perf :: PMap a -> Context a -> Music a -> (Performance, DurT)
interpPhrase_pizz :: (PhraseAttribute -> Performance -> Performance) -- PasHandler
                  -> (PMap a -> Context a -> [PhraseAttribute] -> Music a -> (Performance, DurT))
interpPhrase_pizz pasHandler pm context pas m = (foldr pasHandler pf pas, durPerf)
    where (pf, durPerf) = perf pm context m

player_pizz = MkPlayer { pName        = "CustPizz"
                       , playNote     = defPlayNote       defNasHandler
                       , interpPhrase = interpPhrase_pizz pasHandler_pizz
                       --, notatePlayer = ()
                       }
pMap_pizz :: PlayerName -> Player Note1
pMap_pizz "CustPizz" = player_pizz
pMap_pizz p          = error "Use CustPizz player for testing"

con_pizz = defCon { cPlayer = player_pizz }

-- custPizz :: Music a -> Music a
custPizz mus = Modify (Phrase [Art Pizzicato]) mus

test_8_2_b1 = instrument Violin $ line [c 4 hn, e 4 hn, g 4 hn, b 4 hn]
test_8_2_b2 = custPizz test_8_2_b1

-- Test: play                     test_8_2_b1
-- Test: playA pMap_pizz con_pizz test_8_2_b2

-- ===== Exercise 8.3 =====
-- Define a play myPlayer that appropriately handles the Pedal articulation
-- and both the ArpeggioUp and ArpeggioDown ornamentation.
-- You should define myPlayer as a derivative of defPlayer or newPlayer.

applyArpeggio argOrderF performance = newPerformance
    where sortByPitch (x@MEvent {ePitch = xp}) (y@MEvent {ePitch = yp}) = (argOrderF compare) xp yp
          sortedByPitch = sortBy sortByPitch performance
          groupLen      = toRational $ length performance
          minDur        = minimum $ map (\e -> eDur e) performance
          maxLagDur     = min wn (minDur / 2)  -- Larger lag => easier to hear while testing
          stepLagDur    = maxLagDur / (groupLen - 1)
          lagDurs       = map (* stepLagDur) [0 .. (groupLen - 1)]
          applyLag lag e@MEvent{eDur = d, eTime = t} =
              e { eTime = t + lag
                , eDur = trace ("Applying lag of " ++ (show lag) ++ " at time " ++ (show t))
                               d - lag
                }
          newPerformance = zipWith applyLag lagDurs sortedByPitch

applyArpeggioDown performance = applyArpeggio flip performance
applyArpeggioUp   performance = applyArpeggio  id  performance
compareETime (x@MEvent {eTime = xt}) (y@MEvent {eTime = yt}) = xt == yt

pasHandler_pedal :: PhraseAttribute -> Performance -> Performance
-- See Figure 8.6
-- Note: The Pedal articulation applies to all notes in the phrase,
--       so there is no need to inspect the time of each note.
-- TODO: Assuming for now that all events are notes.  Remove this assumption
pasHandler_pedal pa performance = case pa of
    Art Pedal        -> map newMEvent performance
        where 
              newMEvent (e@MEvent {eDur = d, eVol = v})
                  = e { eDur = trace ( "Old note dur=" ++ (show d)
                                     ++ "; New note dur=" ++ (show (10*d))
                                     )
                                     (10*d)
                      }
    Orn ArpeggioDown -> concatMap newMEventGroup $ groupBy compareETime performance
        where newMEventGroup pf = if (length pf == 1) then pf
                                                      else applyArpeggioDown pf
    Orn ArpeggioUp   -> concatMap newMEventGroup $ groupBy compareETime performance
        where newMEventGroup pf = if (length pf == 1) then pf
                                                      else applyArpeggioUp   pf              
    _                -> defPasHandler pa performance

interpPhrase_pedal :: (PhraseAttribute -> Performance -> Performance) -- PasHandler
                  -> (PMap a -> Context a -> [PhraseAttribute] -> Music a -> (Performance, DurT))
interpPhrase_pedal pasHandler pm context pas m = (foldr pasHandler pf pas, durPerf)
    where (pf, durPerf) = perf pm context m          

player_pedal = MkPlayer { pName        = "CustPedal"
                        , playNote     = defPlayNote        defNasHandler
                        , interpPhrase = interpPhrase_pedal pasHandler_pedal
                        --, notatePlayer = ()
                        }
pMap_pedal :: PlayerName -> Player Note1
pMap_pedal "CustPedal" = player_pedal
pMap_pedal p           = error "Use CustPedal player for testing"

con_pedal = defCon { cPlayer = player_pedal }

custPedal   mus = Modify (Phrase [Art Pedal])        mus
custArpUp   mus = Modify (Phrase [Orn ArpeggioUp])   mus
custArpDown mus = Modify (Phrase [Orn ArpeggioDown]) mus

mkTestPiece_8_3 musicMap = (c 2 hn)
                               :+: (musicMap $ chord [c 4 hn, e 4 hn, g 4 hn, b 4 hn])
                               :+: (c 2 hn)

piece_8_3               = mkTestPiece_8_3 id
piece_8_3_arpDown       = mkTestPiece_8_3 custArpDown
piece_8_3_arpUp         = mkTestPiece_8_3 custArpUp
piece_8_3_pedal         = mkTestPiece_8_3 custPedal
piece_8_3_pedal_arpDown = mkTestPiece_8_3 (custPedal . custArpDown)
piece_8_3_pedal_arpUp   = mkTestPiece_8_3 (custPedal . custArpUp)

test_8_3                = play piece_8_3
test_8_3_arpDown        = playA pMap_pedal con_pedal piece_8_3_arpDown
test_8_3_arpUp          = playA pMap_pedal con_pedal piece_8_3_arpUp
test_8_3_pedal          = playA pMap_pedal con_pedal piece_8_3_pedal
test_8_3_pedal_arpDown  = playA pMap_pedal con_pedal piece_8_3_pedal_arpDown
test_8_3_pedal_arpUp    = playA pMap_pedal con_pedal piece_8_3_pedal_arpUp

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
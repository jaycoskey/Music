-- Haskell School of Music, Chapter 8

-- import System.IO.Unsafe
import Data.List
import Data.Ratio

import Debug.Trace

import Euterpea
import Euterpea.Music

import HSoM

-- ===== Exercise 8.1 =====
-- Ex: Complete the definition of myPasHandler from Chapter 8 of HSoM.

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

-- Test music
-- custCresc :: Music a -> Music a
custCresc mus = Modify (Phrase [Dyn (Crescendo (3/1))]) mus

aps           = replicate 32 60
notes_dur     = sn
notes         = line $ map ap2Note aps
                  where ap2Note = \ap -> Prim $ Note notes_dur $ pitch ap
notes_soft    = addVolume 31 $ notes
notes_cresc   = custCresc notes_soft
music_8_1     = notes_soft :+: notes_cresc

-- Test code
play_8_1   = playA myPMap myCon
test_8_1_a = play     music_8_1  -- Test: All soft notes
test_8_1_b = play_8_1 music_8_1  -- Test: First soft notes, then crescendo notes.

-- ===== Exercise 8.2 =====
-- Ex: Choose some of the other phrase attributes and provide interpretations for them.
-- Note: You might need to access the cKey field.

-- From Figure 8.4 of HSoM:
-- data PhraseAttribute = Dyn Dynamic
--                      | Tmp Tempo
--                      | Art Articulation
--                      | Orn Ornament

-- ----- ----- ----- ----- -----
-- Part 1: Tempo -> Ritardando
-- ----- ----- ----- ----- -----
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

-- Test music
-- Defined in Ex 8.1: notes
notes_loud = notes
notes_slow = custSlow notes_loud
test_8_2_a = notes_loud :+: notes_slow

-- ----- ----- ----- ----- -----
-- Part 2: Articulation -> Pizzicato
-- ----- ----- ----- ----- -----
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

-- Test music
music_8_2_b1 = instrument Violin $ line [c 4 hn, e 4 hn, g 4 hn, b 4 hn]
music_8_2_b2 = custPizz music_8_2_b1

-- Test code
play_8_2_b  = playA pMap_pizz con_pizz
test_8_2_b1 = play       music_8_2_b1  -- Legato notes

-- Pizzicato notes (well, really stacatto).  TODO: Why is the first note silent? 
test_8_2_b2 = play_8_2_b music_8_2_b2

-- ===== Exercise 8.3 =====
-- Ex: Define a play myPlayer that appropriately handles the Pedal articulation
--     and both the ArpeggioUp and ArpeggioDown ornamentation.
-- You should define myPlayer as a derivative of defPlayer or newPlayer.

-- Code common to both arpeggio directions.  argOrderF determines whether the arpeggio goes up or down.
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

interpPhrase_pedal
    :: (PhraseAttribute -> Performance -> Performance) -- PasHandler
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

-- Test music
mkTestPiece_8_3 musicMap = (c 2 hn)
                               :+: (musicMap $ chord [c 4 hn, e 4 hn, g 4 hn, b 4 hn])
                               :+: (c 2 hn)

piece_8_3               = mkTestPiece_8_3 id
piece_8_3_arpDown       = mkTestPiece_8_3 custArpDown
piece_8_3_arpUp         = mkTestPiece_8_3 custArpUp
piece_8_3_pedal         = mkTestPiece_8_3 custPedal
piece_8_3_pedal_arpDown = mkTestPiece_8_3 (custPedal . custArpDown)
piece_8_3_pedal_arpUp   = mkTestPiece_8_3 (custPedal . custArpUp)

-- Test code
play_8_3                = playA pMap_pedal con_pedal

test_8_3                = play piece_8_3
test_8_3_arpDown        = play_8_3 piece_8_3_arpDown
test_8_3_arpUp          = play_8_3 piece_8_3_arpUp
test_8_3_pedal          = play_8_3 piece_8_3_pedal
test_8_3_pedal_arpDown  = play_8_3 piece_8_3_pedal_arpDown
test_8_3_pedal_arpUp    = play_8_3 piece_8_3_pedal_arpUp

-- ===== Exercise 8.4 =====
-- Ex: Define a player jazzPlayer that plays a melody using a jazz "swing" feel.
--         * en :+: en => tempo (3/2) $ qn :+: en
-- Note: Instead of being implemented as a PhraseFun, this can be implemented as a NoteFun
--     that uses the cTime and cDur in the context to distinguish downbeat from upbeat.

-- type NoteFun a            = Context a            -> Dur -> a            -> Performance
-- .... NoteFun (Pitch, [a]) = Context (Pitch, [a]) -> Dur -> (Pitch, [a]) -> Performance
-- defPlayNote   :: (Context  (Pitch, [a]) -> a -> MEvent -> MEvent) -> NoteFun (Pitch, [a])
-- defNasHandler ::  Context a -> NoteAttribute -> MEvent -> MEvent (?)

-- TODO: Complete this answer.  "Swingification" not yet working.  See test code for details.

playNote_jazz
    :: (Context (Pitch, [NoteAttribute]) -> NoteAttribute -> MEvent -> MEvent)
       -> -- NoteFun (Pitch, [NoteAttribute])
          Context (Pitch, [NoteAttribute]) -> Dur -> (Pitch, [NoteAttribute]) -> Performance
playNote_jazz
    nasHandler
    c@(Context cTime cPlayer cInst cDur cPch cVol cKey)
    d
    (p, nas)
        = [foldr (nasHandler c) initEv nas]
            where
                  num       = numerator
                  den       = denominator
                  rmod1 rat = ((num rat) `mod` (den rat)) % (den rat)
                  initEv = trace (  "eTime =  " ++ (show cTime)               ++ "\n"
                              -- ++ "eInst =  " ++ (show cInst)               ++ "\n"
                                 ++ "eDur =   " ++ (show $ d * cDur)          ++ "\n"
                              -- ++ "eVol =   " ++ (show cVol)                ++ "\n"
                                 ++ "ePitch = " ++ (show $ absPitch p + cPch) -- ++ "\n"
                                 )
                                 MEvent { eTime   = cTime
                                        , eInst   = cInst
                                        -- TODO: How to pattern match on Rationals?
                                        , eDur    = if cDur == (1/8)
                                                    then if rmod1 cTime == (0/3)
                                                         then      (2/3) * 2 * d * cDur
                                                         else if rmod1 cTime == (2/3)
                                                              then (2/3)     * d * cDur
                                                              else             d * cDur
                                                    else d * cDur
                                        , eVol    = cVol
                                        , ePitch  = absPitch p + cPch
                                        , eParams = []
                                        }

pMap_jazz :: PlayerName -> Player Note1
pMap_jazz "CustJazz" = player_pedal
pMap_jazz p           = error "Use CustJazz player for testing"

player_jazz :: Player (Pitch, [NoteAttribute])
player_jazz = MkPlayer { pName       = "CustJazz"
                       , playNote     = playNote_jazz   defNasHandler
                       , interpPhrase = defInterpPhrase defPasHandler
                       --, notatePlayer = ()
                       }

con_jazz = defCon { cPlayer = player_jazz }

-- ----- ----- ----- ----- -----
-- Test music
-- ----- ----- ----- ----- -----
-- Some sample music representing Glenn Miller's "In the Mood" grabbed from ch4.hs.
repd d = \ns -> line $ map (\x -> x d) ns
hns = repd hn
qns = repd qn
ens = repd en
bens = ens -- Barred eighth-notes
twice m = m :+: m
whosTheLivin = bens [b 3, d 4, g 4, b 3] :+: bens [d 4, g 4, b 3, d 4]
               :+: bens [g 4, b 3, d 4, g 4] :+: g 4 qn :+: qnr
-- Note: The notation "_5_8" indicates measures 5 though 8.
itm_treb_5_8 = twice whosTheLivin
itm_bass_5_8 = hns [g 2, b 2]
               :+: d 3 hn :+: qns [e 3, d 3]
               :+: hns [g 2, b 2]
               :+: d 3 hn :+: g 3 qn :+: {-NATURAL-} f 3 qn
itm_5_8      = itm_treb_5_8 :=: itm_bass_5_8
itm_sample   = Modify (Tempo (90/60)) $ Modify (KeySig G Major) $ itm_5_8

-- Hand-tweaked to sound like swing.
whosTheLivin_hand = Modify (Tempo (3/2))
                      (
                        line [b 3 qn, d 4 en, g 4 qn, b 3 en]
                        :+: line [d 4 qn, g 4 en, b 3 qn, d 4 en]
                        :+: line [g 4 qn, b 3 en, d 4 qn, g 4 en]
                      )
                    :+: g 4 qn :+: qnr
itm_treb_5_8_hand = twice whosTheLivin_hand
itm_5_8_hand      = itm_treb_5_8_hand :=: itm_bass_5_8
itm_sample_hand   = Modify (Tempo (90/60)) $ Modify (KeySig G Major) $ itm_5_8_hand

-- ----- ----- ----- ----- -----
-- Test code
-- ----- ----- ----- ----- -----
play_8_4       = playA pMap_jazz con_jazz

test_8_4_std   = play     itm_sample
test_8_4_hand1 = play     itm_sample_hand

-- Note: test_8_4_hand2 is what test_8_4_jazz should sound like.
--       test_8_4_hand2 sounds different from test_8_4_hand1,
--       which shows that playNote_jazz is having an impact.
test_8_4_hand2 = play_8_4 itm_sample_hand

-- TODO: Make this sound exactly like test_8_4_hand2.
test_8_4_jazz  = play_8_4 itm_sample

-- ===== Exercise 8.5 =====
-- Ex: Implement the ornamentation DiatonicTrans, which is a "diatonic transposition" within
--     a particular key.  The argument to DiatonicTrans is an integer representing the number
--     of scale degrees to do the transposition.
--   diatonicTrans (C, Major) (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: f 4 en  :+: g 4 en
--   diatonicTrans (G, Major) (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: fs 4 en :+: g 4 en
-- Hint: You will need to use cKey to access the key from the context.

-- I believe the code above was meant to read as follows:
--   In C Major: diatonicTrans 2 (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: f 4 en  :+: g 4 en
--   In G Major: diatonicTrans 2 (c 4 en :+: d 4 en :+: e 4 en) => e 4 en :+: fs 4 en :+: g 4 en

mkScaleDiffs :: Mode -> [AbsPitch]
mkScaleDiffs mode = case mode of
    Major -> [2,2,1,2,2,2,1]
    Minor -> [2,1,2,2,1,2,2]

-- Cover two octaves to allow for transpositions up to one octave from the starting note. 
mkRefScaleNotes :: PitchClass -> Mode -> [AbsPitch]
mkRefScaleNotes pitchClass mode = reverse $ scanr (+) seed aps
    where seed = absPitch (pitchClass, 0)
          aps  = (\x -> x ++ x) (mkScaleDiffs mode)

-- See Figure 8.4
-- Note: This function assumes that either a note or its flat occurs somewhere in the scale
--       associated with Context's cKey.  This is true for Major or Minor modes, where the
--       largest gap between adjascent notes is two semitones.
--       (But this would not hold true for some scales in non-Western music.)
pasHandler_diaTrans :: Context a -> PhraseAttribute -> Performance -> Performance
pasHandler_diaTrans c@Context{cKey = (pc, mode)} (Orn (DiatonicTrans n)) pf 
  = (map diaTrans pf)
    where
      refScale        = mkRefScaleNotes pc mode
      refAbsPitch     = absPitch (pc, 0)
      diaTrans (e@MEvent {ePitch = ap}) = e {ePitch = newAbsPitch}
        where
          relOctaves      =  (ap - refAbsPitch) `div` 12
          basePitch       = ((ap - refAbsPitch) `mod` 12) + refAbsPitch
          basePitch_1     = basePitch - 1  -- The flat of the given note.
          transOctaves    = n `div` 8
          transSemitones  = n `mod` 8
          maybeIndex      = elemIndex basePitch refScale
          maybeIndex_1    = elemIndex basePitch_1 refScale
          newBasePitch    = case (maybeIndex, maybeIndex_1) of
                              (Just index, _           ) -- origRelAbsPitch is in the scale
                                -> (refScale !! (index   + transSemitones)) + 0 
                              (_,          Just index_1) -- origRelAbsPitch not in scale.  Treat it as a sharp.
                                -> (refScale !! (index_1 + transSemitones)) + 1
                              (_,          _)       -- Should not occur in Major or Minor modes.
                                -> error (  "error: Neither note #"
                                         ++ (show basePitch)
                                         ++ " nor its flat found in reference scale for "
                                         ++ (show pc) ++ " " ++ (show mode)
                                         ++ ": "
                                         ++ (show refScale)
                                         )
          newAbsPitch       = trace (
                                      "Key           = " ++ (show pc) ++ " "
                                                         ++ (show mode) ++ "\n"
                                    ++ "ap            = " ++ (show ap)             ++ "\n"
                                    ++ "n             = " ++ (show n)              ++ "\n"
                                    ++ "refAbsPitch   = " ++ (show refAbsPitch)    ++ "\n"
                                    ++ "refScale      = " ++ (show refScale)       ++ "\n"
                                    ++ "relOctaves    = " ++ (show relOctaves)     ++ "\n"
                                    ++ "basePitch     = " ++ (show basePitch)      ++ "\n"
                                    ++ "basePitch_1   = " ++ (show basePitch_1)    ++ "\n"
                                    ++ "transOctaves  = " ++ (show transOctaves)   ++ "\n"
                                    ++ "transSemitones= " ++ (show transSemitones) ++ "\n"
                                    ++ "newBasePitch  = " ++ (show newBasePitch)   ++ "\n"
                                    ++ "newAbsPitch   = " ++ (show result)         ++ "\n"
                                    )
                                    result
                                where result = newBasePitch
                                                   + (12 * relOctaves)
                                                   + (12 * transOctaves)
          newEvent          = e {ePitch = newAbsPitch}
pasHandler_diaTrans c pa pf = defPasHandler pa pf

interpPhrase_diaTrans
    :: (Context a -> PhraseAttribute -> Performance -> Performance) -- Not a PasHandler
        -> (PMap a -> Context a -> [PhraseAttribute] -> Music a -> (Performance, DurT))
interpPhrase_diaTrans pasHandler pm context pas m = case m of
    (Prim (Note d p))         -> (foldr (pasHandler context) pf pas, durPerf)
      where (pf,     durPerf)     = perf pm context m
    (Prim (Rest d))           -> (foldr (pasHandler context) pf pas, durPerf)
      where (pf,     durPerf)     = perf pm context m
    m1 :+: m2                 -> (pf1' ++ pf2', durPerf1 + durPerf2)
      where (pf1,    durPerf1)    = perf pm context m1
            (pf2,    durPerf2)    = perf pm context m2
            pf1' = foldr (pasHandler context) pf1 pas
            pf2' = foldr (pasHandler context) pf2 pas
    m1 :=: m2                 -> (merge (pf1) (pf2), max durPerf1 durPerf2)
      where (pf1,    durPerf1)    = perf pm context m1
            (pf2,    durPerf2)    = perf pm context m2
            pf1' = foldr (pasHandler context) pf1 pas
            pf2' = foldr (pasHandler context) pf2 pas
    Modify (KeySig pc mode) m -> (foldr (pasHandler context') pf_key pas, durPerf_key)
      where (pf_key, durPerf_key) = perf pm (context {cKey = (pc, mode)}) m
            context' = context {cKey = (pc, mode) }
    Modify _                m -> (foldr (pasHandler context) pf pas, durPerf)
      where (pf,     durPerf)     = perf pm context m

player_diaTrans = MkPlayer { pName        = "DiaTrans"
                           , playNote     = defPlayNote           defNasHandler
                           , interpPhrase = interpPhrase_diaTrans pasHandler_diaTrans
                           --, notatePlayer = ()
                           }
pMap_diaTrans :: PlayerName -> Player Note1
pMap_diaTrans "DiaTrans" = player_diaTrans
pMap_diaTrans p          = error "Use DiaTrans player for testing"

con_diaTrans = defCon { cPlayer = player_diaTrans }

diatonicTrans :: Int -> Music a -> Music a
diatonicTrans n mus = Modify (Phrase [Orn (DiatonicTrans n)]) mus

-- Test music
noteCount_trans        = 3
music_8_5              = line [c 4 wn, e 4 wn, g 4 wn]
music_8_5_CMajor       = Modify (KeySig C Major) music_8_5
music_8_5_CMajor_trans = diatonicTrans noteCount_trans music_8_5_CMajor
music_8_5_GMajor       = Modify (KeySig G Major) music_8_5
music_8_5_GMajor_trans = diatonicTrans noteCount_trans music_8_5_GMajor

-- Test code
play_8_5               = playA pMap_diaTrans con_diaTrans

test_8_5_plain         = play_8_5 music_8_5
test_8_5_CMajor        = play_8_5 music_8_5_CMajor
test_8_5_CMajor_trans  = play_8_5 music_8_5_CMajor_trans  -- Transposed in CMajor
test_8_5_GMajor        = play_8_5 music_8_5_GMajor
test_8_5_GMajor_trans  = play_8_5 music_8_5_GMajor_trans  -- Transposed in GMajor

test_8_5 = play_8_5 $ music_8_5_CMajor_trans :=: music_8_5_GMajor_trans -- Listen for intervals

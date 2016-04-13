-- Haskell School of Music, Chapter 9

{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Maybe (mapMaybe)

import Euterpea
import Euterpea.IO.MIDI.Play
import Euterpea.Music

import FRP.UISF                     -- SEvent defined in FRP.UISF.AuxFunctions
import FRP.UISF.Graphics               (withColor', rgbE, rectangleFilled)
-- import FRP.UISF.Widget              hiding (textField, textField', title, spacer, button)
-- import FRP.UISF.Widget.Construction (mkWidget)

import HSoM
import HSoM.Examples.MUIExamples1
import HSoM.Examples.MUIExamples2 hiding (bifurcate, echo)
import HSoM.MUI

-- For a list of UISF functions, see 
--   https://hackage.haskell.org/package/Euterpea-1.1.1/docs/src/Euterpea-IO-MUI.html

-- bifurcate function, from HSoM, Chapter 17:
bifurcate = runMUI (defaultMUIParams {uiSize=(300,500), uiTitle="Bifurcate!"}) $ proc _ -> do
    mo      <- selectOutput -< ()
    f       <- title "Frequency" $ withDisplay (hSlider (1, 10) 1) -< ()
    r       <- title "Growth rate" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
    tick    <- timer -< 1.0 / f
    rec pop <- delay 0.1 -<  maybe pop (const $ grow r pop) tick  
    _       <- title "Population" $ display -< pop
    midiOut -< (mo, fmap (const (popToNote pop)) tick)

-- echo function, from HSoM, Chapter 17:
-- Note: Running echo causes the following runtime exception in ghci (v7.10.3):
--         Exception: FRP\UISF\Widget.hs:217:34-90: Non-exaustive patterns in record update
--       TODO: Test to see if the same things happens when a MIDI device is plugged in.
echo = runMUI (defaultMUIParams {uiSize=(500,500), uiTitle="Echo"}) $ proc _ -> do
    mi <- selectInput  -< ()
    mo <- selectOutput -< ()
    m  <- midiIn       -< mi
    r  <- title "Decay rate"        $ withDisplay (hSlider (0, 0.9) 0.5) -< ()
    f  <- title "Echoing frequency" $ withDisplay (hSlider (1, 10)  10)  -< ()
    rec let m' = removeNull $ mergeS m s
        s  <- vdelay   -< (1.0 / f, fmap (mapMaybe (decay 0.1 r)) m')
    midiOut -< (mo, m')

-- ===== Exercise 17.1 =====
-- Ex: Define a MUI with a text box where the user enters a pitch (e.g., (C,4)) and a "Play" pushbutton.
--     Hint: Use reads :: Read a => String -> [(a, String)] to parse the input

-- Note: What's written here so far is exploratory.
-- TODO: Complete exercise.
-- A good resource might be: "Programming with Arrows", by John Hughes

maybeReadPitch :: String -> Maybe ((PitchClass, Octave), String)
maybeReadPitch str = do
    [(p, rest)] <- return $ reads str
    return (p, rest)

-- From http://hdiff.luite.com/cgit/Euterpea/commit?id=1.0.0
-- How to play a [MidiMessage] ?
mkNote :: Int -> ((Int,Int),(Int,Int)) -> [MidiMessage]
mkNote n ((root,int),(dur,instr)) =
   let durT = 1 / fromIntegral (2 ^ dur)
   in  if n==0 then [ANote instr root 100 durT]
               else [ANote instr (root+int) 100 durT]

-- Note: (>>>) is defined in Control.Category.
-- mkNote 0 ((3,4) (5,6)) >>> midiOut

--playChord = runMUI (defaultMUIParams {uiSize=(300,500), uiTitle="Play Chord"}) $ proc _ -> do
--    mo            <- selectOutput -< ()
--    rec chordTxt  <- textbox <<< "Enter chord here" -< "foo"
--    -- chordStrs  <- reads chordText :: [(Chord, String)]
--    maybePitchStr <- maybeReadPitch chordTxt
--    midiOut      -< (mo, fmap (const (popToNote 440.0)) tick) -- TODO: Play maybePitchStr if it's a pitch.

-- ===== Exercise 17.2 =====
-- Ex: Modify the previous answer, adding a second textbox whose note is played along with the first.
  
-- ===== Exercise 17.3 =====
-- Ex: Modify the previous answer, adding a slider to determine the rate of play.

-- ===== Exercise 17.4 =====
-- Ex: Define a MUI for a pseudo-keyboard with radio buttons for the 12 pitches.
--     Every time a new pitch is selected, that note is played.

-- ===== Exercise 17.5 =====
-- Ex: Modify the previous answer so that an integral slider determines the octave used.

-- ===== Exercise 17.6 =====
-- Ex: Implement Leon Gruenbaum's "Samchillian Tip Tip Tip Cheeepeeeee" keyboard:
--         * A 3-item radio button to choose the scale: chromatic, major, and whole-tone.
--         * Nine pushbuttons for the intervals of the selected scale: 0, and +/- {1,2,3,4}.

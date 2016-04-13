-- Haskell School of Music, Chapter 19

import Euterpea
import Euterpea.Music

import HSoM

-- ===== Exercise 19.1 =====
-- Ex: Using the function osc, create a sinusoidal wave, but using different table sizes and frequencies,
--       and see if you can hear the differences, reporting on what you hear.
--     Use outFile to write your results to a file, and be sure to use a decent set of speakers/headphones.

-- TODO

-- ===== Exercise 19.2 =====
-- Ex: The vibrato function varies a signal's frequency at a given rate and depth.
--     Define an analogous function "tremolo" that varies the volume at a given rate and depth.
--     NNNN

-- TODO

-- ===== Exercise 19.3 =====
-- Ex: Define an ADSR ("attack/decay/sustain/release") envelope generator (i.e., signal source).
--     Call this function envADSR, with type
--         type DPair = (Double, Double)  -- pair of duration and amplitude
--         envADSR :: DPair -> DPair -> DPair -> Double -> AudSF () Double
--     envADSR (attDur, attAmp) (decDur, decAmp) (relDur, relAmp) envDur = ...
--     Hint: Use Euterpea's envLineSeg function.
--     Test your result.

-- TODO

-- ===== Exercise 19.4 =====
-- Ex: Generate a signal that causes clipping, and listen to the result.
--     Then use simpleClip to "clean it up".  Can you hear the difference?
--     Write a more ambitious clipping function using a non-linear reduction in signal amplitude.

-- Code from Chapter 19 of HSoM:
simpleClip :: Clock c => SigFun c Double Double
simpleClip = arr f
  where f x = if abs x <= 1.0 then x
                              else signum x

-- Code for this exercise:

-- TODO

-- ===== Exercise 19.5 =====
-- Ex: Define two instruments, each of type Instr (AudSF () Double), each with at least two Params.
--     Define an InstrMap that uses these.
--     Use renderSF to "drive" your instruments from a Music1 value.
--     Test your result.

-- TODO

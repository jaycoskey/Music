-- Haskell School of Music, Chapter 18

-- import Euterpea
-- import Euterpea.Music

-- import HSoM

-- ===== Exercise 18.1 =====
-- Ex: Longitudinal or transverse?

-- * A vibrating violin string.                      => Transverse
-- * Stop-and-go traffic on a highway.               => Longitudinal
-- * "The wave" in a crowd at a stadium.             => Transverse
-- * "Water hammer" in the plumbing of your house.   => Longitudinal
-- * The wave caused by a stone falling into a pond. => Transverse
-- * A radio wave.                                   => AM: Transverse; FM: Longitudinal 

-- ===== Exercise 18.2 =====
-- Ex: How far does thunder travel in 5 seconds?

-- d = v * t = 343 m/s * 5 sec = 1,715 m

-- ===== Exercise 18.3 =====
-- Ex: What round-trip distance can sound travel in 2 seconds?

-- d = v * t / 2 = 343 m/s * 2 sec / 2 = 343 m

-- ===== Exercise 18.4 =====
-- Ex: What factor increase in the RMS level of a signal yields a 10 dB increase in sound level?

-- Delta_DB = 10 = 10 * log_10 (S/R)
--     => log_10 (S/R) = 1
--     => S = 10 * RMS
-- So the factor increase needed is 10.

-- ===== Exercise 18.5 =====
-- Ex: A dog's freq range = 60 Hz - 45K Hz, and bat's freq range = 2K Hz - 110K Hz.
--     What are the their dynamic ranges, and how do they compare to that of humans?

-- No formula is given for dynamic frequency range, so we'll use 10*log_10(a/b).
-- 
-- Dog: 10 * log_10(45 KHz / 60 Hz)
--          = 10 * log_10(750.0)
--          = 10 * 2.875
--          = 28.750

-- Bat: 10 * log_10(110 KHz / 2 KHz)
--          = 10 * log_10(55.0)
--          = 10 * 1.740
--          = 17.40

-- Human: 10 * log_10(20 KHz / 20 Hz)
--          = 10 * log_10(1000)
--          = 10 * 3.000
--          = 30.000

-- Humans have a slightly larger dynamic range than dogs, due to our better low range.
--   * Dogs' lower limit is 3x that of humans, while their upper limit is only 2.5x.

-- Bats have the lowest dynamic frequency range.  They can hear very high frequencies,
-- but the lowest note they can hear (2 KHz) is very high: approximately (B,6).

-- ===== Exercise 18.6 =====
-- Ex: What is the max # of audible overtones note with a fundamental freq of 100 Hz?
--     500 Hz?  1500 Hz?  5 KHz?

-- 100   Hz => (20 KHz / 100   Hz) - 1 = 199 overtones
-- 500   Hz => (20 KHz / 500   Hz) - 1 = 39  overtones
-- 1500  Hz => (20 KHz / 1500  Hz) - 1 ~ 12  overtones
-- 5    KHz => (20 KHz / 5    KHz) - 1 = 3   overtones

-- ===== Exercise 18.7 =====
-- Ex: Consider a continuous input signal whose frequency is f.
--     Find a formula for the frequency r of the reproduced signal,
--     given a sample rate s.

-- TODO

-- ===== Exercise 18.8 =====
-- Ex: How much memory is needed to record 3 min or stero sound,
--     using 16-bit samples @ a rate of 44.1 KHz?

-- Memory = 3 min * 60 sec/min * 44,100 samples/sec * 2 bytes/sample
--        = 15,876,000 bytes
--        ~ 15.8 MB

-- ===== Exercise 18.9 =====
-- Ex: For the best possible sound, how large should the table be, using fixed-waveform
--     table-lookup synthesis, to cover the audible frequency range?

-- TODO

-- ===== Exercise 18.10 =====
-- Ex: Police car speed to change a siren with frequency is "concert A" to one octave higher?
--     At that speed, what frequency would we hear after the police car passes us?

-- Note: "Concert A" = 400 Hz

-- TODO

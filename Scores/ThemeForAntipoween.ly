\version "2.18.2"
\layout { indent = 0.0 }
\header {
    title = "Theme for Antipoween"
    subtitle = "2016-04-30"
    composer = "Jay Coskey"
}
global = {
  \key c \major
  \time 4/4
}
right  = \absolute {
  \global
  c'4. g'16 g'16                  b'4. g'16 g'16
  c'4             b'4             g'8. e'16 c'4 
  c'8  g'16 e'16 g'16 e'16 c''8   g'8. a'16 f'8. e'16
  c'8  g'8        f'8. b'16       d'8. b16  g'16 c'16 e'16 b'16
  g'8  c'8        b'8. f'16       b8.  g'16 b'16 b'16 g'16 g'16
  c'16 c'16  g'8  b'8. e''16      b'4       a'16 f'16 d'16 b16
  b'8. c'16       a'8 a'8         f'4       % f'4
                                            f'8  g'16 e'16 % g'16 e'16 c''8
  b'4. g'16 g'16                  c'2
  r1
}
left = \absolute {
  \global
  <e g>4     r4 <g d'>4 r4
  <a, e>4    r4 <e b>4  r4
  <f a,>4    r4 <c e>4  r4
  <f a,>4    r4 <g b,>4 r4
  <c' e>4    r4 g4      r4
  a4         r4 e4      r4
  f4         r4 c4      r4
  g4.b16 b16 c2
  r1
}
\score {
  \new PianoStaff \with { instrumentName = "Piano" }
  <<
    \new Staff = "right" \with { midiInstrument = "acoustic grand" }
    \right
    \new Staff = "left"  \with { midiInstrument = "acoustic grand" }
    { \clef bass \left }
 % %{
    \new DrumStaff {
      \drummode {
        bd8 bd8 hh4 cymra4 cl4
        bd8 bd8 hh4 cymra4 cl4
        bd8 bd8 hh4 cymra4 cl4
        bd8 bd8 hh8 cl16 cl16
            cymra4 hh8 cl16 cl16

        hh16 hh16 hh16 r16
            cl16 cl16 cl16 r16
                         hh16 hh16 hh16 r16
                             cl16 cl16 cl16 r16
        bd8 bd8 hh8 cl16 cl16 cymra8 cymra8 cymra4
        hh8 cl16 cl16 r4 cl4  bd8 bd8
        hh8 hh8 cymra8 cl16 cl16
                         hh16 hh16 hh16 cl16
                             hh16 hh16 hh16 cymra16~cymra1
    }
  }
 % %}
  >>
  \layout { }
  \midi { \tempo 4 = 100 }
}
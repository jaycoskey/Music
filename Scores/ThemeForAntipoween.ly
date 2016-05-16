\version "2.18.2"
\layout { indent = 0.5\cm }
\header {
    title = "Theme for Antipoween"
    subtitle = "2016-04-30"
    composer = "Jay M. Coskey"
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
  g'8  c'8  f'8. f'16             b'8. c'16 b'16 b'16 g'16 g'16
  c'16 c'16  g'8  b'8. e''16      b'4       a'16 f'16 d'16 b16
  b'8. c'16       a'8 a'8         f'4       % f'4
                                            f'8  g'16 e'16 % g'16 e'16 c''8
  b'4.\turn g'16 g'16            c'2
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
  g4.b16 b16 <c e>2
  r1
}
instrumentInfoRed = 
  \lyrics {
    "Red instr. = \"long guiro\""
}
instrumentInfoBlack = 
  \lyrics {
    "Black instr. on same line = clave"
}
 
\score {
<<
  \new PianoStaff \with { instrumentName = "Piano" }
  <<
    \new Staff = "right" \with { midiInstrument = "acoustic grand" }
    \right
    \new Staff = "left"  \with { midiInstrument = "acoustic grand" }
    { \clef bass \left }
  >>
 % %{
  \new DrumStaff
  <<
    % \new Voice with { \consists "Balloon_engraver" }
    \drummode {
      \textSpannerDown
      cl8 cl8 hh8 hh16 cymra16    bd4-> r4
      cl8 cl8 hh8 hh16 cymra16    bd4-> r4
      cl8 cl8 hh8 hh16 cymra16    bd4-> r4
      cl8 cl8 hh8 hh16 cymra16    bd4-> r4
      hh16 hh16 hh16 r16
          cl16 cl16 cl16 r16
                                  hh16 hh16 hh16 r16
                                      cl16 cl16 cl16 r16
      cl8 cl8 hh8 hh16 cymra16    ~cymra2
      <<
        {
          \override NoteHead.color = #(x11-color "red") 
          \override Stem.color = #(x11-color "red")
          guil4
          \override NoteHead.color = #(x11-color "black") 
          \override Stem.color = #(x11-color "black")
          r4

          \override NoteHead.color = #(x11-color "red") 
          \override Stem.color = #(x11-color "red")
          guil4
          \override NoteHead.color = #(x11-color "black") 
          \override Stem.color = #(x11-color "black")
          r4
        }
        \new Lyrics { \instrumentInfoRed   }
        \new Lyrics { \instrumentInfoBlack }
      >>
      hh8->\ff\< hh16 r16 cymra8-> hh16 cl16
                                  cymra16-> hh16 hh16 cl16
                                      hh16 hh16 hh16 cymra16->~cymra1->\ffff\!
      \bar "|."
    }
  >>
 % %}
>>
  \layout { }
  \midi { \tempo 4 = 100 }
}
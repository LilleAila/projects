\version "2.24.4"
{
  c' e' g' e' c e g e c'' e'' g'' e'' c''' g''' e''' c'''
  \relative {
    c' d e f
    g a b c
  }
  \relative {
    d' f a g
    c b f d
  }
  \relative {
    a'1
    a2 a4 a8 a
    a16 a a a a32 a a a a64 a a a a a a a a2
  }
  \relative {
    a'4 a a4. a8
    a8. a16 a a8. a8 a4.
  }
  \relative {
    a'4 r r2
    r8 a r4 r4. r8
  }
  \relative {
    \time 3/4
    a'4 a a
    \time 6/8
    a4. a
    \time 4/4 a4 a a a
  }
  \relative {
    \time 3/4
    \tempo "Andante"
    a'4 a a
    \time 6/8
    \tempo 4. = 96
    a4. a
    \time 4/4
    \tempo "Presto" 4 = 120
    a4 a a a
  }
  \relative {
    \clef treble
    c'1
    \clef alto
    c1
    \clef tenor
    c1
    \clef bass
    c1
  }
  \relative {
    \clef bass
    \time 3/4
    \tempo "Andante" 4 = 120
    c,2 e8 c'
    g'2.
    f4 e d
    c4 c, r
  }

  \clef treble
  \time 4/4

  \relative {
    cis''4 ees fisis ases
  }

  \relative {
    \key d \major
    d'4 fis a c |
    \bar "||" \key c \minor
    c,4 ees g b |
  }

  \key c \major

  <<
    \relative {a'2 g}
    \relative {f'2 e}
  >>

  \relative {
    <<c''2 e4>>
  }
}

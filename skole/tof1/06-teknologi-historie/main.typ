#import "@preview/colorful-boxes:1.4.2": colorbox
#import "@preview/numbly:0.1.0": numbly
#import "@preview/touying:0.5.3": *
#import themes.metropolis: *
#import "@preview/callisto:0.2.5"
// #import "@preview/cetz:0.5.2"
// #import "@preview/cetz-plot:0.1.3": plot
#import "@preview/simple-plot:0.3.0": plot

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  align: horizon,
  config-info(
    title: [Historisk utvikling av en teknologisk produkt],
    subtitle: [Historien av transistorer og datamaskiner],
    author: [Olai],
    date: [2026-05-04],
    institution: [Amalie Skram VGS],
  ),
)

#set text(lang: "nb", font: "DejaVu Sans")

#set heading(numbering: numbly("{1}.", default: "1.1"))

#show raw: set text(font: "JetBrainsMono NF")
#show raw.where(block: true): x => {
  set text(size: 0.8em)
  block(
    fill: luma(245),
    stroke: 1pt + luma(200),
    inset: 10pt,
    radius: 4pt,
    width: 100%,
    x,
  )
}
#show raw.where(block: false): x => {
  h(1pt)
  box(
    fill: luma(240),
    stroke: 1pt + luma(200),
    inset: (x: 3pt),
    outset: (y: 3pt),
    radius: 2pt,
    x,
  )
  h(1pt)
}

#show link: set text(fill: blue)
#show link: underline

#let Unit(u) = [$upright(#u)$]
#let unit(u) = [$thin Unit(#u)$]

#title-slide()

= Innhold <touying:hidden>

#outline(title: none, indent: 1em, depth: 2)

= Telegraf

#slide(composer: (1fr, 1fr))[
  Funnet opp av Samuel Morse i 1837 @wiki-telegraph

  Elektriske signaler som morse

  #figure(
    image("assets/telegraph1.png", width: 100%),
    caption: [Telegrafkrets @code[s.~43]],
  )

  Består av en sender og en mottaker.

  Mottaker er en elektromagnet som lager lyd ved å flytte et lodd.
][
  #figure(
    image("assets/morse.png", height: 12cm),
    caption: [Morsekode @wiki-morse],
  )

]

#slide[
  Elektriske signaler over lange distanser. Signaldegradering er et problem.

  #table(
    columns: (1fr, 1fr),
    inset: 10pt,
    table.cell(colspan: 2, fill: rgb("#DDD"))[Avsender],
    [Spenningskilde], [$U = 100 unit(V)$],
    table.cell(colspan: 2, fill: rgb("#DDD"))[Telegrafledning],
    [Materiale], [Jern],
    [Resistivitet], [$rho = 1.0 dot 10^(-7) unit(Omega dot m)$],
    [Størrelse], [$d = 4 unit(m m) => A approx 1.25 dot 10^(-5) unit(m^2)$],
    table.cell(colspan: 2, fill: rgb("#DDD"))[Mottaker],
    [Motstand], [$R = 100 unit(Omega)$],
    [Minstestrøm], [$I = 0.25 unit(A)$],
  )
]

#slide[
  Kirchhoffs 2. lov og Ohms lov:

  $
    U_"total" & = sum U_i \
            U & = I dot R \
  $

  Dette gir:

  $
    U & = U_"kabel" + U_"mottaker" \
    U & = I dot R_"kabel" + I dot R_"mottaker" \
    U & = I (R_"kabel" + R_"mottaker") \
    I & = U / (R_"kabel" + R_"mottaker")
  $
]

#slide[
  Pouillets lov:

  $
    R = rho L/A
  $

  Denne loven definerer en leder sin totale resistans som et forhold basert på resistiviten og tverrsnittsarealet av lederen, i tillegg til lengden på lederen. Vi kan da sette opp følgende uttrykk:

  $
    R_"kabel" & = rho L/A \
    I & = U / (rho L/A + R_"mottaker") \
    I & = (100 unit(V)) / (1.0 dot 10^(-7) unit(Omega dot m) (L)/(1.25 dot 10^(-5) unit(m^2)) + 100 unit(Omega))
  $
]

#slide[
  Setter dette opp som en funksjon der $L = x$ og $x$ er lengden på kabelen i meter:

  $
    f(x) & = (100)/(10^(-7)/(1.25 dot 10^(-5)) x + 100) \
    f(x) & = (100)/(1/(125) x + 100) \
    f(x) & = (12 thin 500)/(x + 12 thin 500)
  $
]

#slide(composer: (auto, 1fr))[
  #figure(
    plot(
      width: 10,
      height: 7,
      xmin: 0,
      xmax: 60000,
      ymin: 0,
      ymax: 1,
      xlabel: $x$,
      ylabel: $y$,
      show-grid: "both",
      xtick-step: 10000,
      minor-grid-step: 2,
      ytick-step: 0.2,
      axis-x-extend: 0,
      axis-y-extend: 0,
      (
        fn: x => 12500 / (x + 12500),
        stroke: blue + 1.5pt,
        label: $f(x) = (12 thin 500) / (x + 12 thin 500)$,
        label-pos: 0.13,
        label-side: "above-right",
      ),
      (
        fn: x => 0.25,
        stroke: black + 1.5pt,
        label: $y = 0.25 unit(A)$,
        label-pos: 0.8,
        label-side: "above",
      ),
    ),
    caption: [Forholdet mellom strøm og distanse],
  )
][
  Strøm avtar med distanse.

  $
    lim_(x -> infinity) (12 thin 500) / (x + 12 thin 500) = 0
  $

  Maks distanse:

  $
                                 f(x) & > 0.25 unit(A) \
    (12 thin 500) / (x + 12 thin 500) & > 0.25 \
                          12 thin 500 & > 0.25 (x + 12 thin 500) \
                      x + 12 thin 500 & < 50 thin 000 \
                                    x & < underline(underline(37 thin 500))
  $
]

== Reléer

#slide(align: center)[
  Man trenger noe til å forsterke signalet:

  #pause

  #grid(
    columns: (1fr, 1fr),
    figure(
      image("assets/minecraft-no-repeater.png", height: 10cm),
      caption: [Krets i minecraft uten relé],
    ),

    pause,

    figure(
      image("assets/minecraft-repeater.png", height: 10cm),
      caption: [Krets i minecraft med relé],
    ),
  )
]

#slide[
  Man trenger noe som dette imellom:

  #figure(
    image("assets/relay1.png"),
    caption: [Visualisering av konseptet @code[s.~44]],
  )
]

#slide[
  Elektromagnet som lukker en krets. Merk de ulike spenningskildene:

  #grid(
    columns: (1fr, 1fr),

    figure(
      image("assets/relay2.png", height: 10cm),
      caption: [Åpen relé @code[s.~45]],
    ),

    figure(
      image("assets/relay3.png", height: 10cm),
      caption: [Lukket relé @code[s.~45]],
    ),
  )
]

#slide[
  Kretsen ser da slik ut:

  #figure(
    image("assets/telegraph2.png"),
    caption: [Telegraflinje med relé @code[s.~45]],
  )
]

= Logiske operasjoner

#slide[
  Kan brukes som en bryter styrt av elektriske signaler.

  #grid(
    columns: (1fr, 1fr, 1fr),
    figure(
      image("assets/switch1.png"),
      caption: [Åpen OG-krets],
    ),
    figure(
      image("assets/switch2.png"),
      caption: [Delvis åpen OG-krets],
    ),
    figure(
      image("assets/switch3.png"),
      caption: [Lukket OG-krets],
    ),
  )

  @code[s.~95-96]
]

#slide[
  Her er en ELLER-krets:

  #grid(
    columns: (1fr, 1fr),
    figure(
      image("assets/or1.png", height: 8cm),
      caption: [Åpen ELLER-krets],
    ),
    figure(
      image("assets/or2.png", height: 8cm),
      caption: [Lukket ELLER-krets],
    ),
  )

  @code[s.~97]
]

#slide[
  Dette definerer de logiske operasjonene vist i sannhetstabellene:

  #grid(
    columns: (1fr, 1fr, 1fr, 1fr),
    inset: 5pt,
    align: center + top,
    [
      #table(
        columns: (auto, auto, auto),
        inset: 10pt,
        [*AND*], [0], [1],
        [0], [0], [0],
        [1], [0], [1],
      )
      #figure(
        image("assets/gate-and.png"),
        caption: [AND-gate],
      )
    ],

    [
      #table(
        columns: (auto, auto, auto),
        inset: 10pt,
        [*OR*], [0], [1],
        [0], [0], [1],
        [1], [1], [1],
      )

      #figure(
        image("assets/gate-or.png"),
        caption: [OR-gate],
      )
    ],

    [

      #table(
        columns: (auto, auto, auto),
        inset: 10pt,
        [*XOR*], [0], [1],
        [0], [1], [1],
        [1], [0], [0],
      )

      #figure(
        image("assets/gate-xor.png"),
        caption: [XOR-gate],
      )
    ],

    [
      #table(
        columns: (auto, auto, auto),
        inset: 10pt,
        [*NAND*], [0], [1],
        [0], [1], [1],
        [1], [1], [0],
      )

      #figure(
        image("assets/gate-nand.png"),
        caption: [NAND-gate],
      )
    ],
  )

  Bilder: @code[s.~113-136]

  Det finnes mange flere. NAND er viktigst, som kan brukes til å lage alle.
]

== Addisjon

#slide(composer: (1fr, 1fr), align: center)[
  Viktigste operasjon i en datamaskin.

  Signalene er AV/PÅ, så man teller i binær med 0/1.

  Sannhetstabell:

  #table(
    columns: (auto, auto, auto),
    inset: 15pt,
    align: center + horizon,
    [*+*], [0], [1],
    [0], [00], [01],
    [1], [01], [10],
  )

  @code[s.~ 132]
][
  Man har altså _to_ outputs:

  #pause

  Første bit (XOR):
  #table(
    columns: (auto, auto, auto),
    inset: 10pt,
    align: center + horizon,
    [*+ sum*], [0], [1],
    [0], [0], [1],
    [1], [1], [0],
  )

  Andre bit (AND):
  #table(
    columns: (auto, auto, auto),
    inset: 10pt,
    align: center + horizon,
    [*+ carry*], [0], [1],
    [0], [0], [0],
    [1], [0], [1],
  )
]

#slide(composer: (1fr, 1fr))[
  Resultat "half adder":

  #figure(
    image("assets/half-adder.png"),
    caption: [Half adder @code[s.~137]],
  )
][
  Tall lengre enn én bit krever en "full adder":

  #pause

  #figure(
    image("assets/full-adder.png"),
    caption: [Full adder @code[s.~137]],
  )
]

#slide[
  Lange tall kan nå summeres:

  #figure(
    image("assets/full-adder-8-chain.png"),
    caption: [8-bit full adder @code[s.~140]],
  )
]

== Subtraksjon

#slide(composer: (1fr, 1fr))[
  Subtraksjon gjøres ved å invertere:

  #figure(
    image("assets/ones-complement.png"),
    caption: [One's complement / inverter @code[s.~149]],
  )
][
  Settes sammen til:

  #figure(
    image("assets/subtract.png", height: 10cm),
    caption: [Subtraction @code[s.~150]],
  )
]

== Minne

#slide(composer: (1fr, 1fr))[
  Utdata kan mates inn i kretsen igjen.

  Dette skaper minne:

  #figure(
    image("assets/r-s-flip-flop.png", height: 7cm),
    caption: [R-S flip-flop],
  )
][
  #table(
    columns: (1fr, 1fr, 1fr, 1fr),
    inset: 10pt,
    align: center,
    table.header(
      table.cell(colspan: 2, [*Inputs*]), table.cell(colspan: 2, [*Outputs*])
    ),
    [S], [R], [Q], [#overline([Q])],
    [1], [0], [1], [0],
    [0], [1], [0], [1],
    [0], [0], [Q], [#overline([Q])],
    [1], [1], table.cell(colspan: 2, [Ugyldig]),
  )

  @code[s.~162]
]

#slide[
  Dette er veldig kult, men jeg rekker ikke å gå mer i dybden :/

  #figure(
    image("assets/computer-circuit.png", height: 10cm),
    caption: [Alle komponentene kombinert: Datamaskin! @code[s.~208]],
  )
]

= Videre teknologi

== Vakuumrør

#slide(composer: (1fr, 1fr))[
  Relé bruker omtrent 10-15ms på å endre tilstand.

  Vakuumrør funnet opp i 1904 av John Ambrose Fleming, som en diode. @wiki-vacuumtube

  Disse bruker 10-100 ns, altså opptil $1 thin 000 thin 000 times$ raskere.
][
  #figure(
    image("assets/vacuumtube1.png", height: 9cm),
    caption: [Vakuumrør-triode @wiki-vacuumtube],
  )
]

#slide[
  #grid(
    columns: (1fr, 1fr),
    align: center,
    figure(
      image("assets/vacuum-diode.png", height: 8cm),
      caption: [Vakuumrørdiode @wiki-vacuumtube],
    ),

    figure(
      image("assets/vacuum-triode.png", height: 8cm),
      caption: [Vakuumrørtriode @wiki-vacuumtube],
    ),
  )

  Bilder: Svjo, CC BY-SA 3.0, #link("https://commons.wikimedia.org/w/index.php?curid=26997397")
]

== ENIAC - første datamaskin

#slide[
  Første generell bruk datamaskin, ferdig 1945.

  ENIAC: Electronic Numerical Integrator and Computer @wiki-eniac

  #grid(
    columns: (1fr, 1fr),

    figure(
      image("assets/eniac-vacuum-tubes.png", height: 7cm),
      caption: [Vakuumrør i ENIAC],
    ),

    figure(
      image("assets/eniac.png", height: 7cm),
      caption: [To deler av ENIAC],
    ),
  )

  #text(
    size: 8pt,
  )[By The original uploader was TexasDex at English Wikipedia. - Transferred from en.wikipedia to Commons by Andrei Stroe using CommonsHelper., CC BY-SA 3.0, #link("https://commons.wikimedia.org/w/index.php?curid=6480859"), #link("https://commons.wikimedia.org/w/index.php?curid=6557095")]
]

== Solid-state brytere

== Oppsummering

- 1830: Telegrafrelé
- 1906: Vakuumrør
- 1945: ENIAC, verdens første datamaskin
- 1947: Solid-state brytere

= Videre læring

#slide(composer: (1fr, 1fr), align: center + horizon)[
  Code - Charles Petzold

  #image("assets/code-cover.png", height: 8cm)

  Den viktigste kilden min, og en veldig bra bok.
][
  Nandgame: Et spill der man bygger en datamaskin fra grunnen av.

  #image("assets/nandgame-alu.png", height: 8cm)

  #link("https://nandgame.com/")
]

#slide(align: top + left)[
  #bibliography("bibliography.yaml", style: "apa")
]

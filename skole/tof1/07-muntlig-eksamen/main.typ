#import "@preview/colorful-boxes:1.4.2": colorbox
#import "@preview/numbly:0.1.0": numbly
#import "@preview/touying:0.5.3": *
#import themes.metropolis: *
#import "@preview/callisto:0.2.5"
#import "@preview/simple-plot:0.3.0": plot
#import "@preview/zap:0.5.0"
#import "@preview/cetz:0.5.2"

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  align: horizon,
  config-info(
    title: [Muntlig eksamen i ToF1],
    subtitle: [Analyse av datamateriale fra forsøk og usikkerhet],
    author: [Olai Solsvik],
    date: [2026-06-10],
    institution: [Amalie Skram VGS],
  ),
)

#set text(lang: "nb", font: "DejaVu Sans")

// #set heading(numbering: numbly("{1}.", default: "1.1"))

#show raw: set text(font: "JetBrainsMono NF")
#show raw.where(block: true): x => {
  set text(size: 0.6em)
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

#let analysis = json("data/analysis.ipynb")

#title-slide()

= Innhold <touying:hidden>

#text(size: 16pt)[
  #outline(title: none, indent: 1em, depth: 2)
]

== Introduksjon

#slide[
  === Problemstilling

  Undersøke hvordan intensiteten til et lys endres med avstand, og vurdere validiteten til to ulike sensorer for å måle dette.

  === Hypotese

  Intensiteten til lyset vil følge den invers-kvadratiske lov:

  $
    I prop 1 / (d^2)
  $
]

== Kravspesifikasjoner

#slide(composer: (1fr, 1fr))[
  Arduino:

  - Måle avstand til lyskilde
  - Måle intensitet av lyset
  - Sende data til PC over Serial

  Lyskilde:
  - Sende ut lys
  - Bevegelig i en rett linje
][
  === Arbeidstegning

  #zap.circuit({
    import zap: *
    import cetz: *

    draw.rect(
      (-1, 0),
      (9, -3),
      radius: 0.3,
      fill: blue.lighten(80%),
      stroke: blue,
    )
    draw.content((9.2, -2), [Fotoresistor], anchor: "west")

    draw.rect(
      (-1, -3.2),
      (9, -6.8),
      radius: 0.3,
      fill: red.lighten(80%),
      stroke: red,
    )
    draw.content((9.2, -5), [TEMT6000], anchor: "west")

    draw.rect(
      (-1, -7),
      (9, -10.8),
      radius: 0.3,
      fill: green.lighten(80%),
      stroke: green,
    )
    draw.content((9.2, -8.9), [Ultralyd], anchor: "west")

    node("5V", (0, 0.5), label: [#text(fill: red)[5V]])
    node("GND", (8, 0.5), label: [#text(fill: red)[GND]])

    wire((0, 0.5), (0, -8.5))
    wire((8, 0.5), (8, -8.5))

    resistor("LDR", (0, -2), (3, -2), label: "LDR", fill: blue.lighten(80%))
    resistor(
      "r1",
      (5, -2),
      (8, -2),
      label: $60 unit(k Omega)$,
      fill: blue.lighten(80%),
    )
    node("A1", (4, -1), label: [#text(fill: red)[A1]])
    wire((3, -2), (5, -2))
    wire((4, -2), (4, -1))

    node("S", (2, -4.5), label: "S")
    node("G", (4, -4.5), label: "G")
    node("V", (6, -4.5), label: (content: "V", anchor: "south"))
    node("A0", (2, -5.5), label: (
      content: [#text(fill: red)[A0]],
      anchor: "south",
    ))
    wire((2, -4.5), (2, -5.5))
    wire((4, -4.5), (4, -6))
    wire((4, -6), (8, -6))
    wire((6, -4.5), (6, -3.45))
    wire((6, -3.45), (0, -3.45))

    node("VCC", (1, -8.5), label: "VCC")
    node("GND", (7, -8.5), label: "GND")
    node("Trig", (3, -8.5), label: "Trig")
    node("Echo", (5, -8.5), label: "Echo")
    wire((1, -8.5), (0, -8.5))
    wire((7, -8.5), (8, -8.5))
    wire((3, -8.5), (3, -9.5))
    wire((5, -8.5), (5, -9.5))
    node("9", (3, -9.5), label: (
      content: [#text(fill: red)[9]],
      anchor: "south",
    ))
    node("10", (5, -9.5), label: (
      content: [#text(fill: red)[10]],
      anchor: "south",
    ))
  })
]

== Produkt

#slide[
  #grid(
    columns: 2,
    inset: 10pt,
    figure(
      image("assets/koblingsbrett.png"),
      caption: [Koblingssbrett med sensorer],
    ),
    figure(
      image("assets/lommelykt.png"),
      caption: [Lommelykt med "flagg"],
    ),
  )
]

#slide(composer: (auto, 1fr))[
  === Sensorer

  - Fotoresistor / LDR (lys)
  - TEMT6000 (lys)
  - Ultralyd / HC-SR04 (avstand)

  === Andre komponenter

  - Lommelykt
  - Papir
  - Papp
  - Arduino
][
  #table(
    columns: (1fr, 1fr, 1fr),
    inset: 10pt,
    table.header([*Sensor*], [*Type*], [*Output*]),
    [LDR], [Analog], [Logaritmisk],
    [TEMT6000], [Analog], [Lineær],
    [HC-SR04], [Digital], [Tid],
  )
]

== Metode

#slide(composer: (1fr, 1fr))[
  Plassere koblingsbrettet vertikalt, sakte bevege lyskilden bort fra sensorene i et mørkt rom.

  Data måles hvert $50 "ms"$, fra begge lyssensorene samt distanse fra ultralydsensoren.
][
  #figure(
    image("assets/metode-1.png"),
    caption: [Produkt med lyskilde plassert foran],
  )
]


#[
  #set page(columns: 2)

  == Analyse

  #slide[
    Invers-kvadratisk lov:

    $
      I & prop 1 / (d^2) \
      I & = k / (d^2)
    $

    Gjelder for lys som går fra ett punkt og ut i alle retninger.

    $
                 d_"sentrum" & = d \
                    d_"kant" & = sqrt(d^2 + R^2) \
      I_"kant" / I_"sentrum" & = (k/(sqrt(d^2 + R^2)^2))/(k/d^2) \
                             & = (d^2) /(d^2 + R^2) \
                             & = 1 / (1 + (R / d)^2)
    $

    Dette gir $(R/d)^2$ som avviket mellom den "perfekte" modellen og om kilden er en større flate.
  ]

  #slide[
    Lommelykten har omtrent diameter $D = 4 "cm"$ ($R = D/2$). Velger da startpunktet som $d = 5D$, som gir

    // $
    //   I_"kant" / I_"sentrum" & = 1 / (1 + (R/10D)^2) \
    //                          & = 1 / (1 + (R/20R)^2) \
    //                          & = 1 / (1 + (1/20)^2) \
    //                          & = 1 / 1.0025 approx 0.9975
    // $

    $
      I_"kant" / I_"sentrum" & = 1 / (1 + (R / 5D)^2) \
                             & = 1 / (1 + (R / 10R)^2) \
                             & = 1 / (1 + (1/10)^2) \
                             & = 1 / (1.01) approx 0.9901
    $

    Feilen i forhold til punktmodellen er da så liten ($<1%$) at jeg kan ignorere forskjellen. Videre legger jeg til en ekstra $5 "cm"$ for å kompensere for avstanden mellom "flagget" og lyskilden.

    Altså starter jeg målingene ved
    $
      d = 5 dot 4 + 5 = 25 "cm"
    $
  ]
]

#slide(composer: (1fr, 1fr))[
  Importerer biblioteker:

  ```py
  import pandas as pd
  from matplotlib import pyplot as plt
  import numpy as np
  from scipy.stats import linregress
  ```

  Data ser slik ut:

  ```
  TEMT6000:999,LDR:860,Distance:7
  TEMT6000:996,LDR:860,Distance:7
  TEMT6000:999,LDR:860,Distance:7
  TEMT6000:996,LDR:861,Distance:7
  TEMT6000:994,LDR:859,Distance:7
  ```

  ```py
  def read_df(path):
    df = pd.read_csv(path, header=None)
    cols = [x.split(":")[0] for x in df.iloc[0]]
    df.columns = cols
    for col in cols:
        df[col] = df[col].str.split(":").str[1].astype(int)
    return df
  ```
][

  ```py
  df = read_df("final1.csv")
  df["Distance"].plot()
  ```

  #figure(
    box(height: 14em)[
      #callisto.display("distance-raw", nb: analysis)
    ],
    caption: [Avstand over tid],
  )
]

#slide(composer: (1fr, 1fr))[
  ```py
  def clamp_diff(series, diff):
    result = series.copy()
    for i in range(1, len(result)):
        if result.iloc[i] > result.iloc[i-1] + diff:
            result.iloc[i] = result.iloc[i-1] + diff
        elif result.iloc[i] < result.iloc[i-1] - diff:
            result.iloc[i] = result.iloc[i-1] - diff
    return result
  ```

  ```py
  def read(name, diff):
    df = read_df(name)
    df["Distance"] = clamp_diff(df["Distance"], diff)
    return df

  df = read("final1.csv", 0.04)
  df["Distance"].plot()
  ```
][
  #figure(
    callisto.display("distance-clamped", nb: analysis),
    caption: [Avstand over tid (begrenset)],
  )
]

#slide[
  === Vurdering av usikkerhet

  Plassert arduino i et mørkt rom og tatt $approx 300$ målinger.

  ```py
  baseline = read_df("final-baselines.csv")
  del baseline["Distance"] # Distance is irrelevant
  ```
]

#slide[
  ```py
  ldr = baseline["LDR"]
  ldr_range = df["LDR"].max() - df["LDR"].min()
  variasjon = ldr.max() - ldr.min()
  avvik = variasjon / 2
  avvik_forhold = avvik / ldr_range * 100
  ldr_baseline = ldr.mean()
  mid = ldr.min() + variasjon / 2

  print(f"Verdi: {mid:.1f} ± {avvik:.1f}")
  print(f"Gjennomsnitt: {ldr.mean():.3f}")
  print(f"LDR = {ldr_baseline:.1f} ± {avvik:.1f}")
  print(f"Intervallstørrelse: {ldr_range}")
  print(f"Variasjonsbredde = {variasjon} ~ {avvik_forhold:.2f}% av intervall")
  print(f"Standardavvik: {ldr.std():.4f}")
  print(f"Standardfeil: {ldr.sem():.4f}")
  ```

  ```
  Verdi: 2.0 ± 2.0
  Gjennomsnitt: 0.355
  LDR = 0.4 ± 2.0
  Intervallstørrelse: 338
  Variasjonsbredde = 4 ~ 0.59% av intervall
  Standardavvik: 0.7655
  Standardfeil: 0.0427
  ```
][
  #figure(
    callisto.display("baseline-ldr", nb: analysis),
    caption: [Usikkerhet (LDR)],
  )
]

#slide[
  ```py
  temt = baseline["TEMT6000"]
  temt_range = df["TEMT6000"].max() - df["TEMT6000"].min()
  variasjon = temt.max() - temt.min()
  avvik = variasjon / 2
  avvik_forhold = avvik / temt_range * 100
  temt_baseline = temt.mean()
  mid = temt.min() + variasjon / 2

  print(f"Verdi: {mid:.1f} ± {avvik:.1f}")
  print(f"Gjennomsnitt: {temt.mean():.3f}")
  print(f"Intervallstørrelse: {temt_range}")
  print(f"Variasjonsbredde = {variasjon} ~ {avvik_forhold:.2f}% av intervall")
  print(f"Standardavvik: {temt.std():.4f}")
  print(f"Standardfeil: {temt.sem():.4f}")
  ```

  ```
  Verdi: 0.5 ± 0.5
  Gjennomsnitt: 0.003
  Intervallstørrelse: 1001
  Variasjonsbredde = 1 ~ 0.05% av intervall
  Standardavvik: 0.0558
  Standardfeil: 0.0031
  ```
][
  #figure(
    callisto.display("baseline-temt", nb: analysis),
    caption: [Usikkerhet (TEMT6000)],
  )
]

#slide[
  #figure(
    image("assets/inverse-square-law.png", height: 15em),
    caption: [
      Invers-kvadratisk lov

      #text(
        size: 8pt,
      )[By Borb, CC BY-SA 3.0, https://commons.wikimedia.org/w/index.php?curid=3816716]
    ],
  )
][
  #figure(
    callisto.display("temt-raw-scatter", nb: analysis),
    caption: [Intensitet etter avstand],
  )
]

== Analyse - TEMT6000

#slide[
  Tar gjennomsnitt for hver verdi av $d$.

  Lager en ny x-akse $x' = 1/d^2$

  Fjerner verdier før startpunktet

  Grupperer og tar gjennomsnittet for å redusere usikkerheten.

  ```py
  df2 = df.groupby("Distance").mean()
  df2["TEMT6000"] -= df2["TEMT6000"].min()
  df2["LDR"] -= df2["LDR"].min()
  df2 = df2[(df2.index >= 25)]
  df2["x2"] = 1 / (df2.index ** 2)

  df2.plot(x=r"x2", y="TEMT6000", kind="scatter")
  ```
][
  #figure(
    callisto.display("df-grouped", nb: analysis),
    caption: [TEMT6000 ($x' = 1/(d^2)$)],
  )

  Gir nå et tilnærmet lineært resultat.
]

#slide[
  Kan nå gjøre lineær regresjon:

  ```py
  a, b, r, _, _ = linregress(df2["x2"], df2["TEMT6000"])
  print(f"f(x) = ax + b ≈ {a:.3f}x - {-b:.3f}")
  print(f"R^2 = {r**2:.3f}")
  ```

  ```
  f(x) = ax + b ≈ 36599x - 0.891
  R^2 = 0.992
  ```

  $R^2$ nær $1$ indikerer at modellen stemmer bra. $R^2 > 0.99$ betyr at dataen passer svært bra. $R^2 = 0.992$ er altså nesten perfekt.
][
  #figure(
    callisto.display("temt-linear-regression", nb: analysis),
    caption: [TEMT600 med lineær regresjon],
  )
]

#slide[
  Verifiserer invers-kvadrat.

  Antar først at forholdet kan modelleres som

  $
         y & = C dot d^n \
    ln (y) & = ln(C dot d^n) \
    ln (y) & = ln(C) + ln(d^n) \
    ln (y) & = ln(C) + n ln(d)
  $

  Dette er et lineært uttrykk:

  $
    y' & = a x' + b \
    underbrace(ln(y), y') & = underbrace(n, a) underbrace(ln(d), x') + underbrace(ln(C), b)
  $

][
  Finner resultat ved regresjon:

  ```py
  log_d = np.log(df2.index.values)
  log_temt = np.log(df2["TEMT6000"].values - b)

  a_log, _ = np.polyfit(log_d, log_temt, 1)

  print(f"Exponent ≈ {a_log:.3f}")
  ```

  ```
  Exponent ≈ -1.994
  ```

  Dette sier at

  $
    I prop d^(-1.994)
  $

  , noe som er ekstremt nær det teoretiske

  $
    I prop d^(-2) equiv I prop 1/(d^2)
  $
]

== Analyse - LDR

#slide[
  Sensoren er ikke lineær.

  $
    R = k dot I^(- alpha)
  $

  #text(size: 8pt)[
    Kilde: electronics stackexchange https://electronics.stackexchange.com/questions/487182/is-there-an-equation-for-the-relationship-between-illuminance-and-the-resistance og anakara universitet https://acikders.ankara.edu.tr/pluginfile.php/98768/mod_resource/content/1/Week3.pdf
  ]


  der $k$ er en konstant og $alpha$ er sensitivitetskoeffisienten.

  $
    I prop (1/R)^(1/alpha)
  $

  En generisk LDR har $alpha approx 0.7$

  #text(
    size: 8pt,
  )[Kilde: GL55 datablad https://www.kth.se/social/files/54ef17dbf27654753f437c56/GL5537.pdf]
][
  Arduino måler spenning over den konstante motstanden på $60 unit(k Omega)$. Arduino leser spenning lineært i $[0, 1023]$

  $
    U_"Inn" & = I dot sum R \
          I & = U_"Inn" / (R_"LDR" + R_"Konstant") \
     U_"Ut" & = I dot R_"Konstant" \
     U_"Ut" & = (U_"Inn" / (R_"LDR" + R_"Konstant")) dot R_"Konstant"
  $
]

#slide(composer: (auto, 1fr))[
  $
    U_"Ut" / U_"Inn" & = "ADC"/1023 \
          1023/"ADC" & = (R_"LDR" + R_"Konstant") / R_"Konstant" \
          1023/"ADC" & = R_"LDR"/R_"Konstant" + 1 \
             R_"LDR" & = R_"Konstant" dot (1023/"ADC" - 1)
  $
][
  ```py
  resistance = 60_000 # 60 kΩ
  df["R_LDR"] = resistance * ((1023 / df["LDR"]) - 1)
  alpha = 0.7
  df["LDR_Linearized"] = 1 / (df["R_LDR"] ** (1 / alpha))
  ```

  #box(height: 10em)[
    #grid(
      columns: 2,
      inset: 10pt,
      figure(
        callisto.display("raw-ldr", nb: analysis),
        caption: [Raw ADC-verdi],
      ),
      figure(
        callisto.display("raw-ldr-r", nb: analysis),
        caption: [LDR-motstand],
      ),
    )
  ]
]

#slide[
  #grid(
    columns: 2,
    inset: 10pt,
    figure(
      callisto.display("ldr-plot-1", nb: analysis),
      caption: [LDR (raw data)],
    ),
    figure(
      callisto.display("ldr-plot-2", nb: analysis),
      caption: [LDR (linearisert)],
    ),
  )
]

#slide[
  Plotter data samt lineær regresjon.

  ```py
  a, b, r, _, _ = linregress(df2["x2"], df2["LDR_Linearized"])
  print(f"f(x) = ax + b ≈ {a:.0f}x - {-b:.3f}")
  print(f"R^2 = {r**2:.3f}")
  ```

  Resultat:

  ```
  f(x) = ax + b ≈ 0x - 0.000
  R^2 = 0.994
  ```

  $R^2 = 0.994$ er også en svært god verdi.

  Logaritmemetoden gir eksponenten $-1.994$, som er samme som for TEMT6000.
][
  #figure(
    box(height: 15em)[#callisto.display("ldr-linear-regression", nb: analysis)],
    caption: [LDR etter avstand med $x' = 1/(d^2)$, med lineær regresjon],
  )
]

== Refleksjon og feilkilder

#slide[
  Systematiske feil

  - Forskyvning av avstanden i begge ender (papp vs lys, sensorer)
  - Refleksjon og dimming i lommelykten (loven gjelder punktkilder)
  - ADC-referansespenning ($5 unit(V)$?)
  - Lys fra omgivelsene (redusert til nærmest 0)
  - Feil verdi for $alpha$

  Tilfeldige feil

  - Støy fra ultralydsensor
  - Vinkelavvik av lommelykten
]

#[
  #set page(columns: 2)

  #slide[
    === Utregning av den korrekte verdien for $alpha$

    - Måle LDR-motstand med multimeter
    - Måle lux med telefon (phyphox)

    Motstanden til LDR er gitt ved:

    $
      R & = A dot E^(-alpha)
    $

    Kan da gjøre to målinger og sette opp forholdet mellom dem, og løser likningen:

    $
          R_1/R_2 & = (A dot E_1^(-alpha))/(A dot E_2^(-alpha)) \
          R_1/R_2 & = (E_1^(-alpha))/(E_2^(-alpha)) \
          R_1/R_2 & = (E_2/E_1)^alpha \
      ln(R_1/R_2) & = ln((E_2/E_1)^alpha) \
      ln(R_1/R_2) & = alpha dot ln(E_2/E_1) \
            alpha & = (ln R_1 - ln R_2)/(ln E_2 - ln E_1)
    $
  ]
]

#slide(composer: (1fr, 1fr))[
  === Validitet av sensorer:

  #figure(
    image("assets/ldr-resistance.png", height: 11em),
    caption: [
      Semi-log plot av LDR-motstand

      #text(
        size: 8pt,
      )[Kilde: Adafruit https://learn.adafruit.com/photocells/measuring-light]
    ],
  )
][
  - Mer komplisert krets (krever ekstra motstand)
  - Mest følsom for gult og grønt lys ($approx 520-570 unit("nm")$)
  - Logaritmisk transformasjon
  - Lavere oppløsning ved sterkt lys
  - Treg respons
  - Temperaturavhengighet

  Likevel:

  - Mye billigere
  - Enklere elektronikk
  - Likere menneskelig øye
  - "Filter" mot støy pga treghet
]

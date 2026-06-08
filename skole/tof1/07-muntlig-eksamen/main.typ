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
      label: $1 unit(k Omega)$,
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
  df = read_df("dark-flashlight3-long-slow.csv")
  df["Distance"].plot()
  ```

  #figure(
    callisto.display("distance-raw", nb: analysis),
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
  df = read_df("dark-flashlight3-long-slow.csv")
  df["Distance"] = clamp_diff(df["Distance"], 1)
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

  Plassert arduino i et mørkt rom og tatt $approx 250$ målinger.

  ```py
  baseline = read_df("dark-baselines.csv")
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
  print(f"LDR = {ldr_baseline:.1f} ± {avvik:.1f}")
  print(f"Variasjonsbredde = {variasjon} ~ {avvik_forhold:.2f}% av intervall")
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
  print(f"TEMT = {temt_baseline:.1f} ± {avvik:.1f}")
  print(f"Variasjonsbredde = {variasjon} ~ {avvik_forhold:.2f}% av intervall")
  ```

  ```
  TEMT = 19.9 ± 1.0
  Variasjonsbredde = 2 ~ 0.10% av intervall
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
]

#slide[
  Tar gjennomsnitt for hver verdi av $d$.

  Lager en ny x-akse $x' = 1/d^2$

  Fjerner verdier før startpunktet

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
  f(x) = ax + b ≈ 39945x - 1.925
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
  Verifiserer at dette faktisk er invers-kvadrat:

  ```py
  log_d = np.log(df2.index.values)
  log_temt = np.log(df2["TEMT6000"].values - b)

  a_log, _ = np.polyfit(log_d, log_temt, 1)

  print(f"Exponent ≈ {slope_log:.3f}")
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

== Refleksjon og feilkilder

#slide[
  Systematiske feil

  - Bruker `int` for avstand. (Negligerbart da $d > 25$ gir feil på $± 0.5 "cm" < 2%$).

  Tilfeldige feil

  - Lys fra omgivelsene
  - Lyset treffer ikke alltid direkte i en rett linje (menneskelig feil)
]

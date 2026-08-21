#import "@preview/colorful-boxes:1.4.2": colorbox
#import "@preview/callisto:0.2.5"

#set text(
  font: "DejaVu Sans",
  size: 10pt,
  lang: "nb",
)

#set page(
  paper: "a4",
  margin: (top: 4cm, bottom: 4cm, left: 1.5cm, right: 1.5cm),
  header: {
    grid(
      columns: (1fr, auto),
      [Fysikkinnlevering: Reaksjonstid], align(right)[Olai Solsvik],
    )
    line(length: 100%)
  },
  footer: context {
    line(length: 100%)
    grid(
      columns: (1fr, auto),
      [2026-08-21], align(right)[#counter(page).display("1/1", both: true)],
    )
  },
)

#show raw: set text(font: "JetBrainsMono NF")
#show raw.where(block: true): x => block(
  fill: luma(245),
  stroke: 1pt + luma(200),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
  {
    set par(
      hanging-indent: 1.5em,
      leading: 0.6em,
    )
    x
  },
)
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

#let note(body) = block(breakable: false)[#colorbox(
  title: "Merk",
  color: "blue",
  body,
)]

#let task(body) = block(breakable: false)[#colorbox(
  title: "Oppgave",
  color: "green",
  body,
)]

#let Unit(u) = [$upright(#u)$]
#let unit(u) = [$thin Unit(#u)$]
#let implies = sym.arrow.r.double.long
#let pm = sym.plus.minus

#let nb = json("main.ipynb")

#align(center)[
  #v(1em)
  #text(size: 14pt, weight: "bold")[Fysikkinnlevering: Reaksjonstid]
  #v(1em)
  #text(size: 10pt)[Olai Solsvik]
  #v(2em)
]

Jeg gjennomførte dette forsøket sammen med Didrik.

== Hensikt

I dette forsøket skal jeg forsøke å måle reaksjonstiden min. Målet er å bruke standardmål og vurdere feilkilder.

== Metode

En person slipper linjal, den andre personen skal gripe linjalen. Formålet er å måle reaksjonstiden.

Den som griper starter med en avstand på 5cm mellom pekefinger og tommel, og plasserer midten av pekefingeren ved "0cm"-merket på linjalen. Den andre holder i toppen av linjalen og slipper på et tilfeldig tidspunkt. Linjalen slippes fra hodehøyde til mottakeren.

For hver måling holdes linjalen slik at toppen er på lik høyde som toppen av hodet til mottakeren, og linjalen slippes etter en tilfeldig tid.

== Resultater

Bruker andre bevegelseslikning

$
                v(t) & = a t \
  integral v(t) "dt" & = integral a t "dt" \
                   s & = 1/2 a t^2 + s_0
$

#note[
  Jeg innså at dette bare er første bevegelseslikning, så selv om matten er korrekt ville det vært enklere å bare bruke den direkte.
]

Ser bort fra luftmotstand og antar at linjalen er i fritt fall. Vi får da $a=g$. Løser for tid.

$
    s & = 1/2 g t^2 \
  t^2 & = (2s)/g \
    t & = sqrt((2s)/g)
$

#line(length: 100%)

Leser inn resultatene og plotter histogram:

#let data = csv("main.csv")
#let time = csv("time.csv")

#table(
  columns: 2,
  inset: 10pt,
  table.header([*Avstand*], [*Tid*]),
  ..data
    .first()
    .map(d => d + " cm")
    .zip(time.first().map(t => t + " s"))
    .flatten(),
)

```py
xs = np.loadtxt("main.csv", delimiter=",")
ts = np.sqrt(2 * xs / 100 / 9.81)
plt.hist(ts)
```

#callisto.display("hist", nb: nb)

Kan deretter finne diverse standardmål:

$
  "variasjonsbredde": & x_"max" - x_"min" \
           "varians": & sigma^2 = 1/n sum_(i=1)^n (x_i - overline(x))^2 \
                      & s^2 = 1/(n-1) sum_(i=1)^n (x_i - overline(x))^2 \
     "standardavvik": & sigma = 1/n sum_(i=1)^n (x_i - overline(x))^2 \
                      & s = 1/(n-1) sum_(i=1)^n (x_i - overline(x))^2 \
      "standardfeil": & "SE" = s/sqrt(n)
$

Da blir resultatet:

```py
print(f"Gjennomsnitt: {np.mean(ts):.3f}")
print(f"Variasjonsbredde: {np.max(ts) - np.min(ts):.3f}")
print(f"Varians (n): {np.var(ts, ddof=0):.3f}")
print(f"Varians (n-1): {np.var(ts, ddof=1):.3f}")
print(f"Standardavvik (n): {np.std(ts, ddof=0):.3f}")
print(f"Standardavvik (n-1): {np.std(ts, ddof=1):.3f}")
print(f"Standardfeil: {np.std(ts, ddof=1) / np.sqrt(len(ts)):.3f}")
```

```
Gjennomsnitt: 0.188
Variasjonsbredde: 0.077
Varians (n): 0.000
Varians (n-1): 0.000
Standardavvik (n): 0.021
Standardavvik (n-1): 0.022
Standardfeil: 0.005
```

Altså er min gjennomsnittlige reaksjonstid omtrent $0.188 unit(s) = 188 unit(m s)$ (med 3 siffers nøyaktighet).

== Refleksjon

Her er det spesielt to mulige feilkilder jeg vil fokusere på. Den første handler om tiden før slippet. Det var stor variasjon, da vi i utgangspunktet ønsket å ha helt tilfeldig tid fra start til slipp. Vi fant ut at det burde være begrenset til et intervall, da man kan fort miste fokus dersom tiden blir altfor lang.

Den andre feilkilden har med med psykologi å gjøre. Det har blitt vist tidligere at man i noen tilfeller kan gi riktig resultat ikke fordi man vet svaret, men fordi man ser på hvordan "sensoren" oppfører seg. Et av de mest kjente eksemplene på dette er hesten "Clever Hans", der den reagerte på oppførselen til eieren idet den skulle til å svare på et mattestykket, uten å vite svaret selv. Noe lignende psykologisk kan her ha fungert her som en systematisk feil der man ser på den andre sin oppførsel og dette påvirker når man prøver å ta imot linjalen.

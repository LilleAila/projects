#import "@preview/colorful-boxes:1.4.2": colorbox

#set text(
  font: "DejaVu Sans",
  size: 8pt,
  lang: "nb",
)

#set page(
  paper: "a4",
  margin: (top: 4cm, bottom: 4cm, left: 1.5cm, right: 1.5cm),
  columns: 2,
  header: {
    grid(
      columns: (1fr, auto),
      [Fysikk programmeringsinnlevering], align(right)[Olai Solsvik],
    )
    line(length: 100%)
  },
  footer: context {
    line(length: 100%)
    align(right)[#counter(page).display("1/1", both: true)]
  },
)

#show raw: set text(font: "JetBrainsMono NF")
#show raw.where(block: true): x => block(
  fill: luma(245),
  stroke: 1pt + luma(200),
  inset: 10pt,
  radius: 4pt,
  width: 100%,
  x,
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

#set heading(numbering: "1.1")

#let note(body) = colorbox(title: "Merk", color: "blue", body)

#let Unit(u) = [$upright(#u)$]
#let unit(u) = [$thin Unit(#u)$]

#align(center)[
  #v(1em)
  #text(size: 14pt, weight: "bold")[Simulering av kollisjoner mellom partikler]
  #v(1em)
  #text(size: 10pt)[Olai Solsvik]
  #v(2em)
]

#outline()

= Simulering i én dimensjon

I denne oppgaven skal jeg ved hjelp av Coulombs lov simulere kollisjonen mellom to partikler. Jeg starter ved å sette opp uttrykket for Coulombs lov:

$
  F = k (|q_1 dot q_2|)/r^2
$

#note[
  $k$ er Coulombs konstant, definert som
  $
    k = 8.99 dot 10^9 unit(N dot m^2 dot C^(-2))
  $
]

Videre kombinerer jeg dette med Newtons andre lov:

$
  F = m dot a
$

Og finner et uttrykk for akselerasjonen

$
  m dot a & = k (|q_1 dot q_2|)/(r^2) \
        a & = k (|q_1 dot q_2|)/(r^2 dot m)
$

Jeg bruker det at $a = r''(t)$ og setter det opp som en differensiallikning:

$
  r'' = k (|q_1 dot q_2|)/(r^2 dot m)
$

== Implementasjon

I dette tilfellet velger jeg å simulere to protoner som går mot hverandre. Jeg starter dem parallelt om sentrum med posisjon $2 dot 10^(-10)$ i hver retning. Startfarten til partiklene er satt som $5 dot 10^4 unit(m dot s^(-2))$. Kollisjonen skjer innen et ekstremt lite tidsrom.

Dette kan jeg implementere med programmering ved hjelp av eulers metode som bruker numerisk integrasjon som følger. Her bruker jeg også dynamisk programering (DP) som en metode for å effektivisere den kjøringen som i utgangspunktet er definert som en rekursiv funksjon. Med dynamisk programmering kan man løse dette i lineær tid.

```py
# Importere biblioteker og konstanter
from matplotlib import pyplot as plt
from scipy import constants
import numpy as np

k = 8.99e9

m1, q1 = constants.m_p, constants.e # Proton
m2, q2 = constants.m_p, constants.e # Proton

# Definere tidsintervallet.
# Dette skjer over et ekstremt lite tidsrom.
dt = 1e-18
T = 1e-14
MAXN = int(T / dt) + 1

# Definere DP-tabeller
ts = np.zeros(MAXN)
delta_r = np.zeros(MAXN)
r1 = np.zeros(MAXN)
r2 = np.zeros(MAXN)
dr1 = np.zeros(MAXN)
dr2 = np.zeros(MAXN)
ddr1 = np.zeros(MAXN)
ddr2 = np.zeros(MAXN)

# Startverdiene (forklart over)
r1[0] = -2e-10 # Begynner på venstre side
r2[0] = 2e-10 # Begynner på høyre side
dr1[0] = 5e4 # Bevegelsesretning mot høyre
dr2[0] = -5e4 # Bevegelsesretning mot venstre

# Beregne de andre startverdiene basert på dette
delta_r[0] = abs(r2[0] - r1[0])

# Fylle inn DP-tabellene
n = 0
while n < MAXN - 1:
    n += 1
    ts[n] = ts[n-1] + dt

    delta_r[n] = abs(r2[n-1] - r1[n-1])
    F = k * (q1 * q2) / delta_r[n] ** 2

    # Setter motsatt fortegn på kreftene
    ddr1[n] = -F / m1
    ddr2[n] = F / m2

    dr1[n] = dr1[n-1] + ddr1[n] * dt
    dr2[n] = dr2[n-1] + ddr2[n] * dt

    r1[n] = r1[n-1] + dr1[n] * dt
    r2[n] = r2[n-1] + dr2[n] * dt
```

#note[
  Teknisk sett er dette Euler-Cromers ("semi-implisitt") metode. Forskjellen er at Eulers metode ville regnet posisjonen basert på forrige tidssteg: $r_n = r_(n-1) + r'_(n-1) Delta t$, mens Euler-Cromers metode bruker resultatet fra nåværende iterasjon i stedet: $r_n = r_(n-1) + r'_(n) Delta t$. Sistnevnte gjør den mer eksakt for slike applikasjoner som dette, og gjør at energi bevares korrekt over tid i systemet.
]

Jeg kan nå plotte resultatene:

```py
fig, ax = plt.subplots()
ax.plot(ts, r1, label="Proton 1")
ax.plot(ts, r2, label="Proton 2")
ax.set_title("Kollisjon mellom to protoner")
ax.set_xlabel("Tid (s)")
ax.set_ylabel("Posisjon ı(m)")
ax.legend()
plt.show()
```

#figure(
  image("assets/proton-collision-position.png"),
  caption: [Visualisering av kollisjonen mellom to protoner],
)

Her ser jeg at de to protonene går mot hverandre nærmest lineært. Mens de nærmer seg hverandre, reduseres farten gradvis, helt til punktet der den snur og akselererer før den beveger seg lineært. Dette skjer når den kommer ut av rekkevidden til de elektriske kreftene som virker mellom partiklene. Da er det ikke lenger noen krefter som virker mellom dem, og da vil partikkelen etter Newtons første lov fortsette å bevege seg med konstant hastighet.

Dette kan vi også se tydelig når vi plotter akselerasjonen:

#figure(
  image("assets/proton-collision-acceleration.png"),
  caption: [Akselerasjon i kollisjonen],
)

Her har akselerasjonen en horisontal asymptote på $y = 0$. Det vil si at partiklene kun akselerer mens de er innenfor rekkevidden av det elektriske feltet. Siden massen til partiklene i dette tilfellet er lik, og bevegelsesmengden er bevart, vil farten være lik (men negativ) etter kollisjonen:

#figure(
  image("assets/proton-collision-velocity.png"),
  caption: [Fart i kollisjonen],
)

== Utregning av minsteavstand

Jeg vil nå beregne den korteste avstanden $r$ mellom partiklene før de snur retning. Da bruker jeg loven for bevaring av energi $sum E_"start" = sum E_"slutt"$, sammen med Coulombs lov for potensiell energi:

$
  E_P = k dot (q_1 dot q_2)/(r)
$

#note[
  Her tar jeg ikke absoluttverdien, altså avhenger den potensielle energien av fortegnet til ladningene.
]

Jeg beregner da først energien før støtet:

$
  E_"start" = 1/2 m_1 v_1^2 + 1/2 m_2 v_2^2 + k dot (q_1 dot q_2)/(r_"start")
$

Deretter skal jeg finne ut hvor mye energi det er etter støtet. Hvis partiklene har lik masse og ladning vil jeg kunne se bort fra den kinetiske energien, da denne er lik $0$ i vendepunktet. Hvis de derimot er ulike, må jeg også beregne dette med. Dette kan betraktes som et fullstendig uelastisk støt i øyeblikket støtet skjer, og de to partiklene vil få felles masse og fart. Da kan jeg bruke bevaringsloven for bevegelsesmengde til å finne den felles farten til partiklene:

$
  p_"start" & = m_1 dot v_1_"start" + m_2 dot v_2_"start" \
  p_"minsteavstand" & = (m_1 + m_2) dot v_"felles" \
  p_"start" & = p_"minsteavstand" \
  m_1 dot v_1_"start" + m_2 dot v_2_"start" &= (m_1 + m_2) dot v_"felles" \
  v_"felles" &= (m_1 dot v_1_"start" + m_2 dot v_2_"start")/(m_1 + m_2)
$

Dette kan jeg bruke sammen med likningen for kinetisk energi for å sette opp den totale energien ved minsteavstand.

$
  E_K_"slutt" & = 1/2 (m_1 + m_2) dot v_"felles"^2 \
    E_"slutt" & = E_K_"slutt" + k dot (q_1 dot q_2)/(r_"min")
$

Deretter setter jeg den totale energien ved start lik den totale energien ved minsteavstand og løser for $r_"min"$

$
  E_"start" & = E_"slutt" \
  E_"start" & = E_K_"slutt" + k dot (q_1 dot q_2) / r_"min" \
  E_"start" - E_K_"slutt" & = k dot (q_1 dot q_2) / r_"min" \
  (E_"start" - E_K_"slutt") dot r_"min" & = k dot q_1 dot q_2 \
  r_"min" &= (k dot q_1 dot q_2) / (E_"start" - E_K_"slutt")
$

Dette kan jeg da legge til i koden for å finne minsteavstanden.

```py
Estart = 1/2 * m1 * dr1[0] ** 2 + 1/2 * m2 * dr2[0] ** 2 + k * (q1 * q2)/(abs(r2[0] - r1[0]))
vfelles = (m1 * dr1[0] + m2 * dr2[0]) / (m1 + m2)
EKslutt = 1/2 * (m1 + m2) * vfelles ** 2
rmin = (k * q1 * q2) / (Estart - EKslutt)
```

Denne koden gir resultatet

$
  r_min = 4.849669142782682 dot 10^(-11) unit(m)
$

Jeg plotter så med følgende kode

```py
fig, ax = plt.subplots()
ax.plot(ts, delta_r, label="Simulert avstand")
ax.axhline(y=rmin, label=r"Teoretisk $r_{\text{min}}$")
ax.set_title("Avstand mellom to protoner")
ax.set_xlabel("Tid (s)")
ax.set_ylabel("Avstand (m)")
ax.legend()
plt.show()
```

#figure(
  image("assets/proton-collision-distance.png"),
  caption: [Minsteavstanden mellom protonene],
)

Jeg kan da bekrefte fra den horisontale linjen at avstanden som beregnes fra simuleringen min samsvarer med det jeg kommer frem til via den teoretiske utregningen.

== Bevaring av energi

Til slutt vil jeg vise at energien er bevart i kollisjonen. For å gjøre dette, igjen likningen for kinetisk energi kombinert med Coulombs lov for potensiell elektrisk energi:

$
    sum E_K & = 1/2 dot m_1 dot v_1^2 + 1/2 dot m_2 dot v_2^2 \
            & = 1/2 dot m_1 dot r'_1^2 + 1/2 dot m_2 dot r'_2^2 \
        E_P & = k dot (q_1 dot q_2)/(r) \
  E_K + E_P & = "konstant"
$

Jeg legger da til tre flere DP-tabeller i koden for å beregne den kinetiske energien til de to partiklene samt den potensielle elektriske energien mellom dem.

```py
# Legge til DP-tabeller
ek1 = np.zeros(MAXN)
ek2 = np.zeros(MAXN)
ep = np.zeros(MAXN)

# Regne ut startbetingelser
ek1[0] = 1/2 * m1 * dr1[0] ** 2
ek2[0] = 1/2 * m2 * dr2[0] ** 2
ep[0] = k * (q1 * q2) / delta_r[0]

while n < MAXN - 1:
    # ... eksisterende kode
    # Beregne kinetisk og potensiell energi
    ek1[n] = 1/2 * m1 * dr1[n] ** 2
    ek2[n] = 1/2 * m2 * dr2[n] ** 2
    ep[n] = k * (q1 * q2) / (delta_r[n])
```

Videre plotter jeg summen av disse verdiene gjennom simuleringen:

#figure(
  image("assets/proton-collision-energy.png"),
  caption: [Energibevaring i kollisjonen],
)

Denne figurn viser den kinetiske energien til det første protonet, det andre protonet, og den potensielle energien mellom de to protonene i henholdsvis blå, oransje og grønn farge. Vi kan se at i kollisjonsøyeblikket, der farten er lik $0$, er den kinetiske energien lik $0$, mens den potensielle energien er høy. Deretter ser vi at den potensielle energien blir gjort tilbake om til kinetisk energi idet partiklene beveger ser fra hverandre igjen. Viktigst av alt kan vi her se hvordan summen av energien holder seg konstant gjennom hele simuleringen. Dette bekrefter teorien om at energisummen er bevart.

== Kollisjoner mellom andre nuklider

Videre kan jeg bruke samme koden til å simulere andre kollisjoner, for eksempel kollisjonen mellom et proton og en alfapartikkel:

TODO putte inn kodeblokk, resultat og forklare hvorfor resultatet blir som det blir

= Simulering i 2D

Basert på den samme logikken, vil jeg nå utvide programmet mitt til å simulere Rutherfords forsøk. Forsøket går ut på at alfapartikler sendes mot en gullkjerne, og så ser man på hvordan alfapartikkelen avbøyes. Dette beviste da at et atom ikke er en "rosinbolle", slik man trodde før. For å gjøre dette, må jeg utvide implementasjonen min til å fungere i to dimensjoner. Da beregner jeg nå avstanden ved hjelp av pytagoras med $r = sqrt(Delta x^2 + Delta y^2)$, og kraften $F$ må dekomponeres for å kunne finne akselerasjonen (og dermed også fart og posisjon ved hjelp av numerisk integrasjon). Så kan jeg plotte banen som en kurve med x- og y- posisjonene til partiklene. Ved å variere avstanden fra sentrumlinjen som alfapartikkelen sendes inn, kan jeg plotte de ulike banene den vil avbøyes langs, slik som i Rutherfords forsøk.

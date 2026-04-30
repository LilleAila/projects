#import "@preview/colorful-boxes:1.4.2": colorbox

#set text(
  font: "DejaVu Sans",
  size: 8pt,
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

#let note(body) = colorbox(title: "Merk", color: "blue", body)

#let Unit(u) = [$upright(#u)$]
#let unit(u) = [$thin Unit(#u)$]

= Programmeringsinnlevering i fysikk (vår 2026)

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

Dette kan jeg implementere med programmering ved hjelp av eulers metode som bruker numerisk integrasjon som følger:

TODO: insert kode og resultat fra kjøringen

#note[
  Teknisk sett er dette Euler-Cromers ("semi-implisitt") metode. Forskjellen er at Eulers metode ville regnet posisjonen basert på forrige tidssteg: $r_n = r_(n-1) + r'_(n-1) Delta t$, mens Euler-Cromers metode bruker resultatet fra nåværende iterasjon i stedet: $r_n = r_(n-1) + r'_(n) Delta t$. Sistnevnte gjør den mer eksakt for slike applikasjoner som dette, og gjør at energi bevares korrekt over tid i systemet.
]

#line(length: 100%)

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

TODO putte inn kodeblokken

#line(length: 100%)

Til slutt vil jeg vise at energien er bevart i kollisjonen. For å gjøre dette, igjen likningen for kinetisk energi kombinert med Coulombs lov for potensiell elektrisk energi:

$
    sum E_K & = 1/2 dot m_1 dot v_1^2 + 1/2 dot m_2 dot v_2^2 \
            & = 1/2 dot m_1 dot r'_1^2 + 1/2 dot m_2 dot r'_2^2 \
        E_P & = k dot (q_1 dot q_2)/(r) \
  E_K + E_P & = "konstant"
$

Jeg legger dette da inn i koden for å vise bevaringen av energi:

TODO putte inn kodeblokk og resultat her også

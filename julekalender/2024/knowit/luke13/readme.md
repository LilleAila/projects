Den nordpolske toppserien i fotball er ikke så stor. Den består kun av 7
lag, som til gjengjeld spiller mot hverandre 20 ganger iløpet av en
sesong (februar til november). Det som kanskje skiller denne toppserien
fra andre toppserier, er måten spillerlønning håndteres på:

- Alle spillere mottar en flat bonus på 100,- per minutt de spiller
- Summen er deretter gjenstand for en multiplikator som avhenger av
  flere faktorer:
  - Hvis laget scorer mål får alle spillere +5%, men hvis laget
    slipper inn får de -5%.
  - Eksempel: Hvis resultatet blir 3-2, så tjener de +5%
    (5+5+5-5-5).
  - Den som får assist på målet (A) får +6% i stedet for 5%, og de
    som scorer målet (S) får +7% i stedet for 5%
  - Eksempel: Hvis resultatet blir 3-2, hvor man også scorer det ene
    målet, så tjener de +7% (7+5+5-5-5).
  - Om man blir kåret til banens beste (B), får man 2% ekstra.
  - Hvert lag man møter har også en egen multiplikator tilknyttet
    seg, som indikerer vanskelighetsgrad. Denne legges til dersom
    man vinner, og trekkes fra dersom man taper
  - Hvis man taper en kamp gis et ekstra insentiv på 3% for neste
    seier, for å motivere spillerne. Insentivet øker med 1% for
    hvert tap helt til man vinner. Da legges insentivet til resten
    av modifikatorene, før den settes tilbake til 0. Uavgjort
    viderefører insentivet uten å øke det.
  - Eksempel: Hvis laget taper 3 kamper på rad, og vinner nr 4, vil
    det ikke bety noe ekstra i første, andre eller tredje kamp. Men
    fjerde kamp får man +5% (3+1+1).
  - Alle disse variablene summeres sammen til et negativt eller
    positivt tall (eller null). Summen av disse tallene utgjøre en
    prosentsatsen som brukes til å moderere \"minuttlønna\" (se
    eksempel).

Vanskelighetsmultiplikator:

- FC Snøbenhavn (FCB): 1 %
- Julventus (J): 2 %
- Nisswich Town (NT): 1 %
- Northingham Forest (NF): 1 %
- Reinchester City (RC): 3 %
- Snøkrystall Palace (SP): 2 %
- Vinter Milan (VM): 3 %

Etter endt sesong gleder Alvin Braut Fortann seg til julehandelen. Han
spiller for Reinchester City, som har gjort det bra. Men han har ikke
hatt så god oversikt over spillerlønningen sin; den har faren hans Alv
Inge, tatt seg av. Og nå vil han gjerne vite hva han har tjent iløpet av
året, slik at han veit hvor dyre gaver han kan kjøpe til venner og
familie. Alv Inge har sendt han [denne
filen](https://julekalender-backend.knowit.no/challenges/2024-13/files/salary.txt?disposition=inline){rel="noreferrer noopener"
target="\_blank"} for å finne ut av det. Hvor mye penger har Alvin til å
handle julegaver for?

## Eksempel

Inntjening for 1 spiller i 1 kamp er gitt ved en streng på følgende
format: `J/65/B/2-1/AB`. Dette betyr:

- `J` betyr at man har møtt Julventus
- `65` betyr at man har spilt 65 minutter
- `B` betyr at man har vært bortelag (H for hjemme)
- `2-1` betyr at hjemmelaget har scoret 2 ganger, og bortelaget 1
  gang. Hjemmelaget har vunnet.
- `AB` betyr at spilleren har fått 1 assist og blitt kåret til banens
  beste

De 65 minuttene gir brutto 6500 før modifikatoren regnes ut slik: `-2`
for laget (man var bortelag, og tapte, og mister derfor
lagmodifikatoren), `-5` for hvert av målene man slapp inn, `+6` for
målet man scoret, fordi man hadde assist på det, og `+2` for å bli kåret
til banens beste, altså `-2-5-5+6+2=-4%`

Summen spilleren tjener blir dermed 6500 \* 0.96 = 6240,-

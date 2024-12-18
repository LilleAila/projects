I Nissens nye gavepapir- og adresse-presse er det et fryktelig trykk før julaften, og nå ser det ut som de holder på å gå tom for sifre! For å undersøke problemet bestemmer de seg for å printe ut hvor mange sifre det er igjen.

Ånei! En evig løkke!! Det viser seg såklart at for å printe ut sifre kreves det også sifre.

Da de startet tellingen var det 100 000 av hvert siffer 0-9 igjen. Printingen de gjør for å telle starter slik:

```
Det er 100 000 9-ere igjen, det er 100 000 8-ere igjen, det er 100 000 7-ere igjen, det er 100 000 6-ere igjen, det er 100 000 5-ere igjen, det er 100 000 4-ere igjen, det er 100 000 3-ere igjen, det er 100 000 2-ere igjen, det er 99 992 1-ere igjen, det er 99 960 0-ere igjen.
Det er 99992 9-ere igjen, det er 99999 8-ere igjen, det er 99999 7-ere igjen, det er 99998 6-ere igjen, det er 99999 5-ere igjen, det er 99999 4-ere igjen, det er 99999 3-ere igjen, det er 99997 2-ere igjen, det er 99991 1-ere igjen, det er 99958 0-ere igjen.
osv.
```

Sifre regnes som brukt opp det øyeblikket vi er ferdige med å printe et fullstendig tall. I eksempelet over er det allerede brukt 8 1-ere i det vi er klare til å printe hvor mange 1-ere det er igjen. Når vi deretter kommer til å printe 0-ere (fortsatt på første linje) har vi allerede brukt 40 0-ere. Etter å ha printet ut resterende antall 0-er er linjen ferdig.

Når det er tomt for et siffer står det bare det er 0 <x>-ere igjen for alltid. Nissens magi sørger for at det er nok magiske sifre tilgjengelig til å printe det som trengs underveis, men når alle sifre er helt helt helt tomme strekker dessverre ikke nissemagien til og printingen stopper etter den linjen hvor det gikk helt tomt.

Hvor mange linjer ble printet før vi gikk helt helt helt tom for alle sifre, og i hvilken rekkefølge gikk sifrene tomme? Oppgi svar som <linjer> <x1>,<x2>,...<x3>, f.eks. 1234 1,2,3,4,5,6,7,8,9.

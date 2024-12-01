# Gavekanonen

Julenissen har skaffet seg et nytt redskap som skal hjelpe han å levere alle verdens pakker på atter en ny rekordtid: En gavekanon! Kanonen fungerer slik at Nissen velger en lokasjon, og så leverer gavekanonen umiddelbart gaver til alle husstander i et område rundt denne lokasjonen med ikke-dødelig eksplosjonsbasert julemagi. Vidunderlig!

For å optimalisere mest vil Nissen velge den mest effektive modusen for kanonen, for den har nemlig 3. Samtidig vil han optimalisere strategien hans for hvordan han velger lokasjoner for å skyte ut gaver.

De tre modusene til gavekanonen har hver sin radius for gavelevering, tid det tar å lade om, samt hvordan redskapet overoppheter. Overopphetingen er oppgitt i prosent, hvor kanonens radius minskes med angitt prosentandel (av den opprinnelige radiusen, ikke eksponensielt) for hver omlading helt til den når 20% av sin opprinnelige radius.

Moduser:

1. Basisradius: 2km, omladingsstid: 62ns, overopphetingskoeffisient: 10%
2. Basisradius: 1km, omladingsstid: 22ns, overopphetingskoeffisient: 5%
3. Basisradius: 500m, omladingsstid: 16ns, overopphetingskoeffisient: 0.2%
Nissen har også tre måter han vil prøve å velge neste lokasjon:

1. Lokasjonen er alltid det nordligste koordinatet i lista. Dersom det er flere velges det østligste av disse.
2. Lokasjonen er det ytterste koordinatet fra lista i en himmelretning. Etter hver avfyring velges neste himmelretning med klokka. Om det er flere ytterste koordinater velges det ytterste av dem i neste himmelretning (likt som punkt 1. for nord). Han starter i nord.
3. Lokasjonen er ytterste koordinatet fra lista i en himmelretning. Etter hver avfyring velges neste himmelretning slik: Dersom antall gjenstående koordinater er delelig på 5, velg samme himmelretning; ellers, hvis de er delelig på 2, velg neste himmelretning mot klokka; ellers, velg neste himmelretning med klokka. Om det er flere ytterste koordinater velges alltid det ytterste med neste himmelretning med klokka.
Hvilken kombinasjon av kanonmodus og valg av mål tar minst tid for Nissen per by? Angi svaret i antall nanosekunder. Nissen teller ikke med omladingen for nattens siste avfyring og teller heller ikke med noe reisetid. Gavekanonen regner [Evklidsk distanse](https://en.wikipedia.org/wiki/Euclidean_distance) mellom punkter, og på breddegraden vi opererer antar vi `55 500` meter per grad i hver akse.

Nissen gjør eksperimentet sitt på et utvalg av [de snille barna i Oslo](gateadresser_oslo_koordinater_liten.txt.gz), her presentert som en liste av koordinater `<breddegrad> <lengdegrad>`.

Om du vil ha noe ekstra å bryne deg på kan du prøve [alle adressene i Oslo](gateadresser_oslo_koordinater_stor.txt.gz). Datasettet er hentet [kartkatalog.geonorge.no](https://kartkatalog.geonorge.no/metadata/matrikkelen-vegadresse/e628729b-90fc-4f32-b018-655e045c541d).

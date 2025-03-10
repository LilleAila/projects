# Valg på Nordpolen

Det har akkurat vært valg på Nordpolen. Alvene har stemt på hvilket parti de ønsker skal styre nordpolen og avgjøre julens fremtid. Kandidatene i årets valg var Kanela Holkis fra partiet Nord, Donisse Tramp fra partiet Julepartiet de Røde, Jule Bukken fra partiet Advent Alveparti og Jul E. Vask fra partiet Motpol. Din oppgave er å finne ut av hvem som vant valget!

På nordpolen er det 50 stater, og 501 valgnisser fordelt på disse statene. Den kandidaten som har fått flest valgnisser vinner valget. Valgnissene i hver stat blir fordelt proposjonalt mellom hver kandidat.

Eksempel: En stat med 10 valgnisser med 100 totalt stemmer

```
+----------+---------+--------------+------------+-------------+-----------+
| Kandidat | Stemmer | Fordeling VN | VN (floor) |    Rest     | Totalt VN |
+----------+---------+--------------+------------+-------------+-----------+
| a        | 8       | 0.8          | 0          | 0.8         | 1         |
| b        | 23      | 2.3          | 2          | 0.3         | 2         |
| c        | 49      | 4.9          | 4          | 0.9         | 5         |
| d        | 20      | 2.0          | 2          | 0.0         | 2         |
+----------+---------+--------------+------------+-------------+-----------+
|          | 100     | 10           | 8          | 2           | 10        |
+----------+---------+--------------+------------+-------------+-----------+
```

Valgnissene blir fordelt ved å dele antall kandidatstemmer med totalt antall stemmer i staten og gange opp med antall valgnisser i staten. Dette tallet blir rundet ned og vi vil få en foreløpig fordeling av valgnisser. I eksempelet ser vi at valgnissefordelingen mangler to nisser, og disse må bli fordelt på kandidatene. Kandidat c har størst rest og dermed størst krav på en ekstra valgnisse, så c får en av de resterende valgnissene. Siden det fortsatt er én valgnisse igjen, vil kandidat a med nest størst rest også få en valgnisse. Dersom det er uavgjort hvem som har størst krav på en resterende valgnisse går den til kandidaten med lavest id.

Listen over kandidater kandidater.txt

Listen over stater, antall valgnisser og stemmer finner du i filen stater.txt

Hvem vant? Svaret skal skrives på formen X - Y, hvor X er kandidatens fulle navn, og Y er totalt antall valgnisser hen fikk. Eks. Jule Bukken - 501

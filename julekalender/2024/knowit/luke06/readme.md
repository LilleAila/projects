# Gaveteller sabotasje

I nissens verksteder er det gigantiske sett med 7-segmentdisplayer som viser hvor mange pakker de har laget til årets jul. Hvert verksted har ansvar for opp til 999 999 999 pakker, så displayene kan vise tall opp til 999 999 999. Displayene er dermed satt sammen av 9 7-segmentdisplayer. Tallene vises i dette formatet:

```
+--+     +  +--+  +--+  +  +  +--+  +--+  +--+  +--+  +--+
|  |     |     |     |  |  |  |     |        |  |  |  |  |
|  |     |     |     |  |  |  |     |        |  |  |  |  |
+  +     +  +--+  +--+  +--+  +--+  +--+     +  +--+  +--+
|  |     |  |        |     |     |  |  |     |  |  |     |
|  |     |  |        |     |     |  |  |     |  |  |     |
+--+     +  +--+  +--+     +  +--+  +--+     +  +--+  +--+
```

Ubrukte displayer viser 0 dersom tallet som skal vises er lite nok til å ikke trenge alle displayenene. F.eks. vises 1024 som 000001024.

Hvert sifferdisplay er addressert ABC... fra høyre (lavere siffer) og hvert 7-segmentdisplay addresseres slik:

```
   A
   _

F|   |B
   _
   G
E|   |C
   _
   D
```

Adressen DC addresserer altså segment C i display D (tusenplassen).

Grinchen har i jul igjen tuklet med displayet slik at det viser feil tall. Han har byttet om på ledninger mellom noen av segmentene slik at de viser feil. Dette er kritisk infrastruktur og du må hjelpe til med debuggingen. Følgende segmenter har byttet plass i addresseringen:

AC <-> IB
BD <-> EG
GF <-> DE
BA <-> BB

Hvor mange av tallene displayet kan vise er de samme selv etter Grinchens tukling?

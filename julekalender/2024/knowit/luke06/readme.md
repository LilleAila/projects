# Gaveteller sabotasje

I nissens verksteder er det gigantiske sett med 7-segmentdisplayer som viser hvor mange pakker de har laget til årets jul. Hvert verksted har ansvar for opp til `999 999 999` pakker, så displayene kan vise tall opp til `999 999 999`. Displayene er dermed satt sammen av 9 7-segmentdisplayer. Tallene vises i formatet beskrevet her.

Hvert sifferdisplay er adressert ABC... fra høyre (lavere siffer) og hvert 7-segmentdisplay adresseres slik:

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

Adressen DC adresserer altså segment C i display D (tusenplassen).

Grinchen har i jul igjen tuklet med displayet slik at det viser feil tall. Han har byttet om på ledninger mellom noen av segmentene slik at de viser feil. Dette er kritisk infrastruktur og du må hjelpe til med debuggingen. Følgende segmenter har byttet plass i adresseringen:

```
AC <-> IB
BD <-> EG
GF <-> DE
BA <-> BB
```

Hvor mange av tallene displayet kan vise er de samme selv etter Grinchens tukling?

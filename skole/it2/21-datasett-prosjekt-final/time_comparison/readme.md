# Sammenligning av kjøretid med ulike løsninger

Bruker `pages.csv` fra `koreader` som en test-fil.

## Lesing av filer og konvertering av typer

Output:

```yaml
Native: 3.483839138003532
Native (precomputed types): 32.8781177639903
Native (on-demand types): 32.48213716200553
Csv: 5.4203146979853045
Csv (precomputed types): 34.863454949023435
Csv (on-demand types): 35.38986133399885
Pandas: 3.0797835330013186
Pandas (precomputed types): 5.047841730003711
```

Så pandas er raskere enn både å lese gjennom `csv`-modulen, og å gjøre det manuelt med å lese filen direkte. Det er også betydelig raskere å konvertere types ved hjelp av pandas sine innebygde og automatiske funksjoner enn å gjøre det ved hjelp av `int()`, `float()`, etc.

## Diverse funksjoner

Output:

```yaml
Native: 34.08872032101499
Pandas: 7.106926887005102
```

Totalt sett er altså pandas betydelig raskere enn native python på alle måter jeg har testet, i tillegg til at koden er mye enklere å skrive.

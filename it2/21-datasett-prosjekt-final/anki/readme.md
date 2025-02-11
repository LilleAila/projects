# Datasett-prosjekt: Anki

Anki er et program som brukes til flashcards. Det fungerer slik at når man får opp et kort, tenker man først på hva som er på baksiden. Deretter trykker man en knapp for å se baksiden, og programmet registrerer hvor lang tid det tok. Etter det velger jeg et tall fra 1-4 på hvor godt jeg mener jeg husket kortet, med alternativene "Again", "Hard", "Good" og "Easy". Dette blir brukt til å bestemme intervallet for når jeg skal se kortet neste gang, slik at kort jeg kan godt tar lengre tid før jeg må gjøre det på nytt, mens det jeg kan dårlig får jeg etter kort tid. Dette datasettet ble ekstrahert fra sqlite-filen til anki ved hjelp av `sqlitebrowser`, slik [dokumentasjonen](https://docs.ankiweb.net/stats.html#manual-analysis) sier.

## Kolonner i datasettet:

Datasettet har en rad for hver review gjort (et kort kan reviewes flere ganger). Kolonnene er som følger:

- **id**: Tid siden `1970-01-01 00:00` i millisekunder
- **cid**: Identifikator for kortet som ble reviewet
- **usn**: Intern status, har ingen nytte i analysen
- **ease**: Hvor bra jeg valgte at jeg husker kortet. Tall fra 1 til 4, fra Again til Easy
- **ivl**: Intervallet kortet fikk etter review. Positive verdier er dager, negative verdier er i sekunder
- **lastIvl**: Intervallet kortet hadde før review
- **factor**: "Ease"-faktoren til kortet etter review. Nytt intervall når man trykker good er `lastIvl * factor / 1000`.
- **time**: Tid brukt på review i millisekunder. Inkludert tid på begge sider av kortet før man velger ease.
- **type**:
  0: "Learning"
  1: "Review"
  2: "Relearning"
  3: "Cram"

## Analyse og behandling av data

- [pandas.ipynb](./pandas.ipynb) inneholder en løsning ved hjelp av pandas.
- [native.ipynb](./native.ipynb) inneholder en løsning ved hjelp av kun innebygde biblioteker i python, i tillegg til matplotlib.
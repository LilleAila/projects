# Datasett-prosjekt: Koreader

## Analyse og behandling av data

### Filer

- [pandas.ipynb](./pandas.ipynb) inneholder en løsning ved hjelp av pandas. Koden her inneholder flere kommentarer og dokumentasjon rundt kodeblokkene.
- [native.ipynb](./native.ipynb) inneholder en løsning ved hjelp av kun innebygde biblioteker i python, i tillegg til matplotlib. Her finnes den samme dataen som ved pandas, men mer "manuelt".

### Bakgrunn

[Koreader](https://github.com/koreader/koreader) er programmet jeg bruker på en e-reader til å lese bøker. Det logger statistikk om hvilke bøker jeg har lest, når sider blir lest og hvor lang tid jeg bruker på hver side.

Gjennom denne oppgaven vil jeg finne ut ulik statistikk om lesevanene mine.

### Sammenligning av alternativer

Mellom de to alternative løsningene, liker jeg best den som bruker pandas. Det gjør det mye enklere å finne den informasjonen jeg leter etter uten å først måtte skrive mange linjer med kode som går gjennom dictionaries, lister, og må kommunisere med api fra flere ulike biblioteker.

## Datasettet

Dette datasettet har ingen god offisiell dokumentasjon, da det ble ekstrahert fra en intern database som ikke egentlig er ment for at brukere skal bruke direkte. Derfor har jeg måttet tolke hva kolonnene betyr selv.

Datasettet er delt inn i to filer:

### `books.csv`

Inneholder en liste over alle bøkene som er lest, med følgende kolonner:

- **id**: Intern id på boken, brukt i `pages.csv` til å koble sider lest opp mot en bok
- **title**: Navnet på boken
- **authors**: Forfatter(e) av boken.
- **notes**: Antall notater tatt i boken
- **last_open**: Tiden boken ble sist åpnet i sekunder siden unix epoch
- **highlights**: Antall markeringer tatt i boken
- **pages**: Totalt antall sider i boken
- **series**: Hvilken serie boken er en del av
- **language**: Språket til boken
- **md5**: md5-checksum til boken. Irrelevant for meg
- **total_read_time**: Total tid brukt i boken, i sekunder
- **total_read_pages**: Totalt antall sider lest

### `pages.csv`

Inneholder en liste over alle sider lest, med følgende kolonner:

- **id_book**: Identifikator for hvilken bok denne siden ble lest i
- **page**: Sidetallet i boken som ble lest
- **start_time**: Når siden ble åpnet, i sekunder siden unix epoch
- **duration**: Tid brukt på siden, i sekunder
- **total_pages**: Totalt antall sider i boken


## Biblioteker

- python 3.11.11
- pandas
- numpy
- matplotlib
- jupyter, jupyterlab, ipykernel (for redigering av `.ipynb`-filer)

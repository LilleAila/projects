- Enhetstesting: teste enheter som klasser eller funksjoner
- Integrasjonstesting: tester om ulike enheter fungerer riktig sammen med andre deler
- Systemtesting: teste hele systemet om kravene tilfredsstilles
- Akseptansetesting: La brukere teste programmet som om det var satt i drift. Forsikrer seg om at systemet fungerer som forventet og oppfyller krav.

---

- Testenhet / test unit: enhetstest
- Testtilfelle / test case: hva som testes og hvordan. Inndata, hvordan testen utføres, forventet resultat.
- Testmetode / test function: metode innenfor testklasse for et testtilfelle (starter med `test_`)
- Testklasse / test class: klasse som inneholder testmetoder (startr med `Test`)
- Testmodul / test module: python script som inneholder testklasser / funksjoner (starter med `test_`)
- Testsamling / test suite: samling av testtilfeller som kjøres samtidig
- Testkontekst / text context: det som settes opp før tester kjører og fjernes etterpå. Klargjøre data, etc. (fixtures)
- Fixtures: funksjoner som kjøres før / etter testene. Setter opp eller rydder opp testkonteksten. `@pytest.fixture`
- Etterligning / mocking: Imitere en eksisterende funksjon som oppfører seg forutsigbart.
- Testdrevet utvikling / TDD / Test Driven Development: skrive tester først, deretter skrive koden til å passe testene.
- Refaktorering: forbedre koden etter alle tester passerer

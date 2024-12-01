# Rot-n til alt vondt

Julenissen har sendt ut ein viktig bodskap, men dei vonde kreftene har fanga den opp og krypteret den for å hindre den i å kome ut. Dei har nytta rot-n-kryptering, (det generelle tilfellet av rot-13, også kjent som Cæsarchiffer) der n er spesiell for kvart teikn i bodskapet.

Oppgåve

Den krypterte meldinga ser slik ut:
```
Ojfkyezkz bvclae zisj a guomiwly qr tmuematbcqxqv sa zmcgloz.
```

- Kvar bokstav `b[i]` i bodskapen `b` har blitt kryptert med `rot-n`, der `n` er lik `x*y`.
- Det er kun nytta A-Z og a-z her, mao ikkje æøå og ÆØÅ.
- Mellomrom og andre teikn har ikkje blitt krypterte, men må tellast med når ein finn i.
- `x` er definert som antal tvillingprimtal-par mellom 0 og 6^6+666
- `y` er det `i`-te elementet i rekka av ikkje-negative heiltal som har et partall antall 1-tall i binærrepresentasjonen sin.

Kva er bodskapen Julenissen har sendt ut?

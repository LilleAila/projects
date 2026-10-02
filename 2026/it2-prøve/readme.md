# Del A

## Oppgave 1

### a)

`//` deler og runder ned, `%` finner resten etter deling. Resultatet blir:

```
2 t og 15 min
```

### b)

`[0:4].upper()` tar de fire første bokstavene, `[-3:]` tar de siste.

```
PROGing
```

### c)

Listen sorteres i synkende rekkefølge. Lengden bevares og `[1]` er nest høyeste.

```
6 4
```

### d)

Går i intervallet $\left[ 1, 10 \right\rangle$ i steg av $10$. Finner altså summen av hvert tredje element: $1 + 4 + 7$

```
12
```

## Oppgave 2

1. Mangler `int()` rundt input (samt validering)
2. Mangler `:` etter if
3. Skriver `=` i stedet for `==` på linje med `elif`

## Oppgave 3

### a)

Programmet dobler alle partall i listen, og siste linje sjekker om summen er `> 10`.

```
[1, 4, 3, 8]
[1, 2, 3, 4]
True
```

### b)

Funksjonen lager en ny liste med elementene i samme rekkefløge, og `if`-delen bestemmer om den skal dobles eller ikke.

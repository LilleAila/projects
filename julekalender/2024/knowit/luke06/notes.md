```
AC <-> IB
BD <-> EG
GF <-> DE
BA <-> BB
```

```
ABC DEF GHI
```

For at tallene skal vises likt etter ombyttingen, må begge tallene ha samme verdi for dette segmentet:

$$
\begin{align}
    AC &= IB \implies A \in \left\{ 0, 2, 3, 5, 6, 7, 8, 9 \right\} \land I \in \left\{ 0, 1, 2, 3, 4, 7, 8, 9 \right\} &\lor A \in \left\{ 2 \right\} \land I \in \left\{ 5, 6 \right\} & \\
    BD &= EG \implies B \in \left\{ 0, 2, 3, 5, 6, 8, 9 \right\} \land E \in \left\{ 2, 3, 4, 5, 6, 8, 9 \right\} &\lor B \in \left\{ 1, 4, 7 \right\} \land E \in \left\{ 0, 1, 7 \right\} & \\
    GF &= DE \implies G \in \left\{ 0, 4, 5, 6, 8, 9 \right\} \land D \in \left\{ 0, 2, 6, 8 \right\} &\lor G \in \left\{ 1, 2, 3, 7 \right\} \land D \in \left\{ 1, 3, 4, 5, 7 \right\} & \\
    BA &= BB \implies B \in \left\{ 0, 2, 3, 7, 8, 9 \right\} &&
\end{align}
$$

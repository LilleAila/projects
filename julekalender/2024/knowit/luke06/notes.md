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
    AC &= IB \implies A \in \{ 0, 2, 3, 5, 6, 7, 8, 9 \} \land I \in \{ 0, 1, 2, 3, 4, 7, 8, 9 \} &\lor A \in \{ 2 \} \land I \in \{ 5, 6 \} \\
    BD &= EG \implies B \in \{ 0, 2, 3, 5, 6, 8, 9 \} \land E \in \{ 2, 3, 4, 5, 6, 8, 9 \} &\lor B \in \{ 1, 4, 7 \} \land E \in \{ 0, 1, 7 \} \\
    GF &= DE \implies G \in \{ 0, 4, 5, 6, 8, 9 \} \land D \in \{ 0, 2, 6, 8 \} &\lor G \in \{ 1, 2, 3, 7 \} \land D \in \{ 1, 3, 4, 5, 7 \} \\
    BA &= BB \implies B \in \{ 0, 2, 3, 7, 8, 9 \} &
\end{align}
$$

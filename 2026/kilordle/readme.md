# Optimal kilordle solver

This project contains a few of my attempts at solving [kilordle](https://jonesnxt.github.io/kilordle/) optimally. This is essentially a variant of wordle where you play 1000 games at once. If a board contains five green letters, it is automatically solved. Thus, the optimal solution will be to test all possible placements of every character in as few guesses as possible. The catch is that the guesses must be real words, so "AAAAA", "BBBBB", etc. is not possible. Hence we have to write some code which finds the optimal combination based on a dictionary. For simplicitly, i have used the exact same dictionary as the game uses.

## Result

The following is the asymptotically optimal sequence, containing 31 guesses (order does not matter):

- fixer
- glyph
- whisk
- crowd
- optic
- itchy
- twixt
- abamp
- aflaj
- aggro
- burqa
- djinn
- enzym
- howff
- jambu
- kvell
- lavvy
- mekka
- nduja
- oxbow
- pzazz
- qajaq
- redox
- schav
- skegg
- squab
- usque
- vifda
- xysti
- ympes
- zinco

> [!NOTE]
> It is possible to solve it even more efficiently using the feedback from the game. Then, it would be possible to make a greedy approach (similar to in [main.ipynb](./main.ipynb)) but which reacts to the game state and updates the information gain based on this.

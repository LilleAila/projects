import pulp
import numpy as np

words = np.loadtxt("words.csv", dtype=str)

information_gains = np.zeros((26, 5), dtype=float)
n = len(words)

def char_idx(c):
    return ord(c) - ord("a")

# Generate a list of all possible characters
chars = []
for char in range(26):
    for column in range(5):
        chars.append((char, column))

problem = pulp.LpProblem("", pulp.LpMinimize) # set that this is a minimization problem

# One LP boolean variable for each word
word_vars = {word: pulp.LpVariable(f"w_{word}", cat="Binary") for word in words}
problem += pulp.lpSum(word_vars.values()) # First objective: minimize the number of words

# Constraint: each character and position pair to exist once
for char, column in chars:
    covering_words = [w for w in words if char_idx(w[column]) == char]
    problem += pulp.lpSum(word_vars[w] for w in covering_words) >= 1

problem.solve(pulp.PULP_CBC_CMD())
guessed = [w for w in words if pulp.value(word_vars[w]) > 0] # pyright: ignore[reportOperatorIssue]

for i in guessed:
    print(i)
print(len(guessed))

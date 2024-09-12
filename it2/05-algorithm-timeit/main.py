from timeit import timeit
from random import randint


# Min versjon
# Tidskompleksitet: O(n)
def sum1():
    # items = [randint(1, 10) for _ in range(100)]
    items = list(range(1000))
    total = 0
    for i in items:
        total += 1
    return total


# Builtin i python
# Tidskompleksitet: ¯\_(ツ)_/¯
def sum2():
    # items = [randint(1, 10) for _ in range(100)]
    items = list(range(1000))
    return sum(items) # Denne er sikkert skrevet i C, og dermed er den nok raskere


# Kjøre begge 100k ganger
time1 = timeit(sum1, number=100000)
time2 = timeit(sum2, number=100000)

print(f"Min versjon brukte {time1}")
print(f"Den innebygde versjonen brukte {time2}")

raskest = "min" if time1 < time2 else "den innebygde"
print(f"Den raskeste versjonen var {raskest}")

for i in [1, 2, 3]:
    print(i)

for i in range(10):
    print(i)  # Merk at 10 ikke er inkludert

# Range returner ikke en liste, men en iterator
print(list(range(10)))

print(list(range(3, 10)))

# x e [2, 10>, x mod 2 = 0
# alle partall mellom 2 og 10, ikke inkludert 10
print(list(range(2, 10, 2)))

# Siste arg er hvor mye den skal hoppe
print(list(range(9, 0, -2)))

# Strings er iterable, men ikke list
tekst = "Hello, World"
print(tekst)
print(list(tekst))
print(iter(tekst))
print(list(iter(tekst)))

for bokstav in tekst:
    print(bokstav, end="")
print()

# List comprehension
[print(bokstav) for bokstav in tekst]

[i for i in range(1, 10) if i % 2 == 0]
# ekvivalent til:
[i for i in range(1, 10, 2)]

liste = [(i * 2 - 3) / 4 for i in range(1, 100)]
print(len(liste))
print(min(liste))
print(max(liste))
print(sum(liste))
print(sum(liste) / len(liste))

from random import randint

print([randint(1, 10) for _ in range(10)])

liste = list(range(25))
mapped = map(lambda x: x % 2, liste)
print(liste)
print(list(mapped))


def user_input():
    while True:
        try:
            return int(input("Skriv tall: "))
        except ValueError:
            print("nuh uh")
            continue

land = ["Norge", "Danmark", "Svalbard"]
print("Svalbard" in land)
if "Svalbard" in land:
    land.pop(land.index("Svalbard"))

tall = [i for i in range(1, 10)]
# pop() muterer listen
tall.pop()  # Fjerne siste idx
tall.pop(0)  # Fjerne første
tall.pop(4)  # Fjerne femte (!!)
tall.append(5) # Legge til 5 på slutten
tall.insert(2, 42) # Legge til 42 på index 2 (tredje), shifte alt etter til høyre

# Dette blir ikke en full kopi, men en referanse til dne første listen
list1 = [1, 2, 3, 4]
list2 = list1
list3 = list(list1)
list4 = [i for i in list1]
list1[2] = 5
print(list1, list2, list3, list4)

print([1, 2, 3] * 2)
print([1, 2, 3] + [4, 5, 6])

import numpy as np
tall = np.array(list(range(1, 10+1)))
print(tall * 2)

tall = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print(tall[3:6])
print(tall[:5])
print(tall[5:])
print(tall[-3:])
print(tall[:-1])
# skrive lengde på steg etter enda et :
print(tall[::2])
print(tall[1::2])
# tall[start:end:step]
print(tall[:]) # Lage kopi
print(tall[-1]) # siste ting i listen

tall = [9, 1, 5, 8, 3, 4, 10, 7, 11, 2, 6]
tall.reverse()
print(tall)
tall.sort() # muterer listen
print(tall)
tall.sort(reverse=True)
print(tall)
print(sorted(tall)) # muterer IKKE listen

import random

print(random.uniform(1, 10))

#!/usr/bin/env python3
"""
Notater fra kapittel 1A av læreboken, "Tall og tekster"

Andre gode ressurser:
- https://docs.python.org/3/library/typing.html
"""

# Printing
print("Heihei")

# Variabler
fornavn = "Olai"
print("Hei,", fornavn)
print(f"Hei, {fornavn}")
print("Hei", fornavn, sep=", ")

# Matte
a = 8
b = 2
print(a + b)  # Add
print(a - b)  # Subtract
print(a * b)  # Multiply
print(a / b)  # Divide
print(a**b)  # Exponent
print(a % b)  # Remainder
print(a // b)  # Divide and floor

# Floats
c = 72.3
print(type(b))
print(type(c))

# Konstanter (er ikke egentlig konstant men skrives slik)
PI_VERDI = 3.141592653589
E_VERDI = 2.718281828456045245360

# Dette er en kommentar
"""
Dette er en kommentar over flere linjer
"""

# Biblioteker
import math

diameter = 5
areal = math.pi * (diameter / 2) ** 2
print("areal: {areal}")

# from math import * # import alt til global namespace
from math import pi
from math import sqrt as sq

print(sq(areal / pi))

from random import randint

print(randint(1, 10))

some_string = "I'd just like to interject for a moment. What you're referring to as Linux, is in fact GNU/LInux, or as i've recently taken to calling it, GNU plus Linux. Linux is not an operating system unto itself, but rather another free component fo a fully functioning GNU system made useful by the GNU corelibs, shell utilities and vital system components comprising a full OS as defined by POSIX."
some_multiline_string = """
I'd just like to interject for a moment. What you're referring to as Linux, is in fact, GNU/Linux, or as I've recently taken to calling it, GNU plus Linux. Linux is not an operating system unto itself, but rather another free component of a fully functioning GNU system made useful by the GNU corelibs, shell utilities and vital system components comprising a full OS as defined by POSIX.

Many computer users run a modified version of the GNU system every day, without realizing it. Through a peculiar turn of events, the version of GNU which is widely used today is often called Linux, and many of its users are not aware that it is basically the GNU system, developed by the GNU Project.

There really is a Linux, and these people are using it, but it is just a part of the system they use. Linux is the kernel: the program in the system that allocates the machine's resources to the other programs that you run. The kernel is an essential part of an operating system, but useless by itself; it can only function in the context of a complete operating system. Linux is normally used in combination with the GNU operating system: the whole system is basically GNU with Linux added, or GNU/Linux. All the so-called Linux distributions are really distributions of GNU/Linux!
"""
print(some_string)
print(some_multiline_string)

short_string = "hello, world!"
print(short_string.lower())
print(short_string.upper())
print(short_string.capitalize())
print(short_string.title())
print(short_string.replace(",", "!"))
print(short_string.index("l"))
print(len(short_string))

string1 = "Hei"
string2 = "på"
string3 = "deg"

print(string1 + " " + string2 + " " + string3)
print(string1, string2, string3)
print(f"{string1} {string2} {string3}")
print(" ".join([string1, string2, string3]))

# convert fra tekst til tall
print(int("2"))
print(float("2.5"))
print(str(3))

userInput = input("skriv et tall: ")
print(userInput)
try:
    userInput = int(userInput)
    print(userInput)
    print(userInput * 2)
except ValueError:
    print("du skulle liksom skrive et tall din fjomp")

# format string
areal = 4
print(f"Arealet er {areal}")
print(f"Oi den kan bli formattet til en mengde desimaler {areal:.4f}")

# flere eksempler på f-string
verdi = 42
print(f"{verdi}")  # Print verdien direkte
print(f"{verdi:8}")  # Print med mellomrom foran slik at den totale lengden blir 8
verdi = 3.14
print(f"{verdi}")  # Print verdien direkte
print(f"{verdi:.1f}")  # Print kun en desimal
print(f"{verdi:6.1f}")  # Kombinere flere
verdi = "Hei"
print(f"{verdi}")
print(f"{verdi:<8}")  # Samme som :8 men mellomrom etter
print(f"{verdi}")
print("abcdef", end="\n")
print("Hello", end="")  # Samme som printf i andre språk (uten newline)
# for eksempel `cout << "Hello"` uten `<< endl`
print(", World!")
print("01", end=" | ")
print("02", end=" | ")
print("03")

# Man can declare types slik som dette:
# Tatt fra docs: https://docs.python.org/3/library/typing.html
def surface_area_of_cube(edge_length: float) -> str:
    return f"The surface area of the cube is {6 * edge_length ** 2}."

# Å definere egne types ble addet i python 3.12 :o
type Vector = list[float]
def scale(scalar: float, vector: Vector) -> Vector:
    return [scalar * num for num in vector]

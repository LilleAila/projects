from math import exp, pi

def f(x):
    return 1 / exp(x ** 2)

n = 100_000

a = 0
b = 2
dx = (b - a) / n

v = 0

for i in range(n):
    x = a + dx * i
    v += f(x) ** 2 * dx

result = pi * v

print(f"V = {result:.4f}")

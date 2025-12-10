a = lambda n: n ** 2 + 4 * n + 3
s = lambda n: sum(a(n) for n in range(1, n + 1))

n = 1
while s(n+1) < 2000:
    n += 1

print(n)
print(s(n))

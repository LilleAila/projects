a = 0
b = 2
n = 90
dx = (b - a) / n

f = lambda x: 2 * x ** 2 + 3

area = 0

for i in range(n):
    x1 = a + dx * i
    x2 = x1 + dx

    v = (f(x1) + f(x2)) * dx / 2 # trapes-sum
    # v = f(x1) * dx # nedre trappesum
    # v = f(x2) * dx # øvre trappesum

    area += v

print(area)

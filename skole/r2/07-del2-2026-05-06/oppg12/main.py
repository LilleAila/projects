# Antall verdier som skal regnes ut
n = 10000
a = [0] * n
s = [0] * n

# Definere startverdiene gitt i oppgaven
a[0] = 4
a[1] = 8

# Finne summen for startverdiene
s[0] = a[0]
s[1] = s[0] + a[1]

# Itererer gjennom intervallet [1, n-1>
# Dette er fordi koden berører n+1.
# Høyeste indeks i listen er n-1
# Dermed ekskluderes det fra løkken.
for i in range(1, n-1):
    a[i+1] = 3 * a[i] - 2 * a[i-1] - 3
    s[i+1] = s[i] + a[i+1]

# Skrive ut resultatene
print(f"a{n} = {a[n-1]}")
print(f"s{n} = {s[n-1]}")

for i in range(n):
    impl = 2 ** (i) + 3 * (i+1)
    if(impl != a[i]):
        raise ValueError("wtf")

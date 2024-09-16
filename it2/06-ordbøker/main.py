person = {"fornavn": "Per", "etternavn": "Christensen"}

print(person["fornavn"])
person["fødselsår"] = 1972
print(person.pop("fornavn"))  # Virker på samme måte som med lister (fjerne og lese)
print(person)

alfabet = "abcdefghijklmnopqrstuvwxyzæøå"
krypter = {j: alfabet[(i + 2) % len(alfabet)] for i, j in enumerate(alfabet)}
# krypter = dict(zip(alfabet, alfabet[2:] + alfabet[:2])) # shifte alle med to
text = "hei dette er en test oi oi oi oi oi"
kryptert = "".join([krypter[i] if i in krypter else i for i in text])
print(kryptert)
dekrypter = {j: i for i, j in krypter.items()}
dekryptert = "".join([dekrypter[i] if i in dekrypter else i for i in kryptert])
print(dekryptert)

for x in person:  # Loope over keys
    print(x)
    print(person[x])
# Disse tre gir iterators:
print(person.keys())
print(person.values())
print(person.items())  # iterator over tupler
a = ("fornavn", "Per")
print(a)
b, c = a  # expansion
print(b, c)
print(("test", "abcdef", "er det ny fff snart"))

sommer_ol = [
    {"årstall": 2004, "vinnertider": {"100 m": 10.93, "200 m": 22.06, "400 m": 49.41}},
    {"årstall": 2008, "vinnertider": {"100 m": 10.78, "200 m": 21.74, "400 m": 49.62}},
    {"årstall": 2012, "vinnertider": {"100 m": 10.75, "200 m": 21.88, "400 m": 49.55}},
    {"årstall": 2016, "vinnertider": {"100 m": 10.71, "200 m": 21.78, "400 m": 49.44}},
    {"årstall": 2020, "vinnertider": {"100 m": 10.61, "200 m": 21.53, "400 m": 48.36}},
]

for year in sommer_ol:
    print(f"Vinnertid på 200m i år {year['årstall']}: {year['vinnertider']['200 m']}s")

times_400m = [y["vinnertider"]["400 m"] for y in sommer_ol]
print(times_400m)
print(min(times_400m))
print(max(times_400m))

poeng = {"Ada": 42, "Linus": 17, "Grace": 58, "Alan": 17, "Guido": 33}

for n, p in poeng.items():
    print(f"{n} har {p} poeng.")

total = 0
winner = ""
max_p = 0
for n, p in poeng.items():
    total += p
    if p > max_p:
        max_p = p
        winner = n
print(f"""
Totalt antall poeng: {total}
Vinner: {winner} med {max_p} poeng
""".strip())

query = input("Skriv navnet på en deltaker: ")
try:
    print(f"{query} har {poeng[query]} poeng.")
except KeyError:
    print(f"Fant ingen deltaker med navnet {query}")

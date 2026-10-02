from collections import defaultdict

class Økt:
    def __init__(self, navn, varighet):
        self.navn = navn
        self.varighet = varighet

    def __repr__(self):
        return f"{self.navn}: {self.varighet} min"

økter = []

def register():
    while True:
        navn = input("Navn på aktiviteten: ")
        if len(navn) <= 0:
            print("Du må skrive noe!")
            continue
        break
    navn = navn.lower()

    while True:
        try:
            varighet = int(input("Varighet i minutter: "))
            break
        except ValueError:
            print("Skriv inn et heltall.")

    økt = Økt(navn, varighet)
    økter.append(økt)
    print(f"Registrert økt: {økt}")

def display():
    print("Alle økter:")
    for i, ø in enumerate(økter):
        print(f"{i+1}. {ø}")

def stats():
    total = sum(ø.varighet for ø in økter)
    n = len(økter)

    per_aktivitet = defaultdict(int)
    for ø in økter:
        per_aktivitet[ø.navn] += ø.varighet
    per_aktivitet = list(per_aktivitet.items())
    per_aktivitet.sort(key=lambda a: a[1], reverse=True)

    print(f"""Statistikk:
Du har trent {n} økter, i totalt {total} minutter, fordelt slik:
{"\n".join([f"{k}: {v} min" for k, v in per_aktivitet])}
Du har trent mest {per_aktivitet[0][0]}, i totalt {per_aktivitet[0][1]} min.
""")

def main() -> bool:
    choice = input("""--- Treningslogg ---
1: Registrer økt
2: Vis alle økter
3: Statistikk
4: Avslutt
Velg: """).strip()

    match choice:
        case "1":
            register()
        case "2":
            display()
        case "3":
            stats()
        case "4":
            print("Avslutter.")
            return True
        case x:
            print(f"Ugyldig valg: {x}. Prøv igjen.")

    return False

while True:
    try:
        finished = main()
        if finished:
            break
    except Exception:
        continue

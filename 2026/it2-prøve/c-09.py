# Valgte å bruke en klasse da dette er den ryddigste metoden.
# I motsetning til en dictionary, vil en klasse sørge for at ting som skrivefeil ikke oppstår og skaper problemer
class Produkt:
    def __init__(self, varenummer, navn, pris, antall, kategori):
        self.varenummer = varenummer
        self.navn = navn
        self.pris = pris
        self.antall = antall
        self.kategori = kategori

type Lager = dict[int, Produkt]
produkter = {
    1001: Produkt(1001, "Melk Tine 1L", 24.50, 50, "Meieri"),
    1002: Produkt(1002, "Grovt Brød", 39.90, 0, "Bakervare"),
    1003: Produkt(1003, "Norvegia 1kg", 149.00, 2, "Meieri"),
    1004: Produkt(1004, "Kjøttdeig Av Storfe", 89.50, 30, "Kjøtt"),
    1005: Produkt(1005, "Epler Røde (kg)", 29.90, 4, "Frukt og grønt")
}

def vis_lager(lager: Lager):
    # Hente ut dataen til en dictionary over kolonner
    columns = {
        "Varenummer": [],
        "Navn": [],
        "Pris": [],
        "Antall": [],
        "Kategori": []
    }
    n = 0
    for p in lager.values():
        columns["Varenummer"].append(str(p.varenummer))
        columns["Navn"].append(p.navn)
        columns["Pris"].append(str(p.pris))
        columns["Antall"].append(str(p.antall))
        columns["Kategori"].append(p.kategori)
        n += 1

    # Beregne maksbredder
    widths = { k: max(len(k), max(len(x) for x in v)) for k, v in columns.items() }
    rows = [
        [k.ljust(widths[k]) for k in columns.keys()], # Header row
        ["".join(["-"] * widths[k]) for k in columns.keys()] # Separator
    ]

    # Transponere matrisen og legge til mellomrom
    for i in range(n):
        row = []
        for k, v in columns.items():
            row.append(v[i].ljust(widths[k]))
        rows.append(row)

    # Skrive ut radene
    for r in rows:
        print(f"| {" | ".join(r)} |")

vis_lager(produkter)

def selg_vare(lager: Lager, varenummer: int, antall: int):
    if varenummer not in lager:
        print(f"Varen med varenummer {varenummer} finnes ikke!")
        return False

    p = lager[varenummer]
    if p.antall < antall:
        print("Det er ikke nok varer på lager!")
        return False

    p.antall -= antall
    print(f"Du må betale {p.pris*antall:.1f} kr.")
    return True

selg_vare(produkter, 1002, 1)
selg_vare(produkter, 1002, 0) # Oppgaven ba ikke om å håndtere denne edge casen
selg_vare(produkter, 1004, 1)
selg_vare(produkter, 1004, 10)

def lav_beholdning(lager: Lager, grense: int=5):
    result = []
    for p in lager.values():
        if p.antall < grense:
            result.append(p)
    result.sort(key=lambda p: p.antall)
    result = {p.varenummer: p for p in result}
    vis_lager(result)

lav_beholdning(produkter)


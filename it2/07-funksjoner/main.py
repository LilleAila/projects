# En metode er en funksjon som hører til et objekt: `objektnavn.metode()`
# Definisjon: parametre
# Bruk: argumenter (selve verdien)
def siHeiTil(navn):
    print(f"Hei, {navn}!")


siHeiTil("Jesper")


def _arealRektangel(lengde, bredde):
    areal = lengde * bredde
    print(f"Arealet av rektanglet er {areal}.")


def navneskilt(navn):
    border = "".join(["-" for _ in navn])
    print(f"+-{border}-+")
    print(f"| {navn} |")
    print(f"+-{border}-+")


navneskilt("Olai")


def arealRektangel(lengde, bredde):
    return lengde * bredde


def sjekkMyndig(alder):
    if alder >= 18:
        return True
    else:
        return False


def _sjekkMyndig(alder):
    if alder >= 18:
        return True
    return False


import random


def toTerninger():
    terning1 = random.randint(1, 6)
    terning2 = random.randint(1, 6)

    return (terning1, terning2)


t1, t2 = toTerninger()


def rekursiv_sum(n):
    if n <= 1:
        return n
    else:
        return n + rekursiv_sum(n - 1)


print(rekursiv_sum(10))


def terning(antallSider: int) -> int:
    """Returnerer et tilfeldig heltall i intervallet [1, antallSider], med begge endepunktente inkludert."""
    return random.randint(1, antallSider)


print(terning(10))

tall = 2


def hmm():
    tall = 3
    print(tall)


hmm()
print(tall)

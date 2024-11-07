"""
Enkle klasser
"""


class Planet:
    def __init__(self, navn: str, solavstand: float, radius: float, ringer: int = 0):
        self.navn = navn
        self.solavstand = solavstand
        self.radius = radius
        self.ringer = ringer

    def __str__(self):
        """
        Brukes når man gjør om til string, for eksempel i print()
        """
        return f"{self.navn}, {self.solavstand} fra solen, {self.radius} radius, {self.ringer} ringer"

    def __repr__(self):
        """
        Blir brukt når man skal representere, men ikke gjøres om til string.
        For eksempel:
            - skrive expressions i python3 -i og se output
            - printe liste av slike klasser
            - etc.
        """
        return "Dette er liksom en instans av planet"


mars = Planet("Mars", 227.9, 3389.5)
print(mars)
print(type(mars))


class Rektangel:
    def __init__(self, lengde, bredde):
        """Dette er konstruktøren til klassen"""
        self.lengde = lengde
        self.bredde = bredde

    def areal(self):
        return self.lengde * self.bredde

    def __str__(self):
        return f"Lengde: {self.lengde}, bredde: {self.bredde}, areal: {self.areal()}"


class Kvadrat(Rektangel):
    """Extende en eksisterende klasse"""

    def __init__(self, sidelengde):
        super().__init__(sidelengde, sidelengde)


print(Rektangel(10, 13))
print(Kvadrat(4))
print(Kvadrat(32).areal())

rektangler = [Rektangel(2, 5), Rektangel(4, 3), Rektangel(5, 6)]
[print(r) for r in rektangler]

"""
Getters, setters og private variabler
"""


class Person:
    def __init__(self, age: int) -> None:
        # Man kan *delvis* bruke innkapsling i python, ved å prefixe en variabel med __
        # Det vil gjøre at den i praksis bytter navn til _Klassenavn__variabelnavn
        # (i dette tilfellet _Person__age), men kan fortsatt endres fra utsiden.
        self.__age = age

    def getAge(self) -> int:
        return self.__age

    def setAge(self, age: int) -> None:
        self.__age = age


p1 = Person(10)
try:
    print(p1.__age)
except AttributeError as e:
    print(e)

"""
Dette vil virke (men LSP klager)
"""
print(p1._Person__age)


class Person2:
    """
    Python sin innebygd emåte å håndtere getters / setters
    """

    # Sier hvilke variabler klassen har.
    # Det er ikke "lov" å bruke attributter som ikke er i __slots__
    __slots__ = "__age"

    def __init__(self, age: int) -> None:
        self.__age = age

    @property
    def age(self) -> int:
        """
        Funksjonen vil kjøres når man leser .age
        """
        print("Getteren blir kjørt!")
        return self.__age

    @age.setter
    def age(self, age: int) -> None:
        """
        Funksjonen vil kjøres når man setter .age til noe med .age =
        """
        print("Setteren blir kjørt!")
        self.__age = age


p2 = Person2(10)
print(p2.age)
p2.age = 1
print(p2.age)
# Det er fortsatt mulig å gjøre dette:
# p2._Person2__age =
print(p2.age)

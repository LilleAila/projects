class Planet:
    def __init__(self, navn: str, solavstand: float, radius: float, ringer: int = 0):
        self.navn = navn
        self.solavstand = solavstand
        self.radius = radius
        self.ringer = ringer

    def __str__(self):
        return f"{self.navn}, {self.solavstand} fra solen, {self.radius} radius, {self.ringer} ringer"


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

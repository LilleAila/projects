"""
Klasser for bil og parkeringsplass, med enkle metoder for å legge til og fjerne
"""


class Car:
    def __init__(self, licenseplate, name, color) -> None:
        self.licenseplate = licenseplate.upper()
        self.name = name.capitalize()
        self.color = color.lower()

    def __repr__(self) -> str:
        return f"{self.color} {self.name} med reg.nr. {self.licenseplate}"


class Spot:
    def __init__(self) -> None:
        self.occupied = False
        self.car = None

    def add_car(self, car: Car) -> None:
        self.occupied = True
        self.car = car
        print(f"Kjørt inn {self.car}")

    def remove_car(self) -> None:
        if not self.occupied:
            print("Det står ingen bil på denne plassen!")
            return
        print(f"Kjørt ut {self.car}")
        self.occupied = False
        self.car = None

    def __repr__(self) -> str:
        if not self.occupied:
            return "Ledig parkeringsplass"
        return f"Opptatt parkeringsplass med {self.car}"


spot = Spot()
car = Car("AB12345", "Tesla", "rød")
print(car)
print(spot)
spot.add_car(car)
print(spot)
spot.remove_car()
print(spot)

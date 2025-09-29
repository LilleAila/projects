"""
Lagt til tid bilen har kjørt inn
"""

import datetime


class Car:
    def __init__(self, licenseplate, name, color) -> None:
        self.licenseplate = licenseplate.upper()
        self.name = name.capitalize()
        self.color = color.lower()

    def __repr__(self) -> str:
        return f"{self.color} {self.name} med reg.nr. {self.licenseplate}"


class Spot:
    def __init__(self, price_per_minute: float = 1.5) -> None:
        self.occupied = False
        self.car = None
        self.in_time = None
        self.price_per_minute = price_per_minute

    def add_car(self, car: Car, time: datetime.datetime) -> None:
        self.occupied = True
        self.car = car
        self.in_time = time
        print(f"Kjørt inn {self.car}, {time}")

    def remove_car(self, time: datetime.datetime) -> None:
        if not self.occupied:
            print("Det står ingen bil på denne plassen!")
            return
        print(f"Kjørt ut {self.car}")
        assert isinstance(self.in_time, datetime.datetime)
        # https://stackoverflow.com/a/39181657
        time_parked = (time - self.in_time).total_seconds() / 60.0
        price = self.price_per_minute * time_parked
        print(f"Prisen blir {price:.1f} kr.")
        self.occupied = False
        self.car = None
        self.in_time = None

    def __repr__(self) -> str:
        if not self.occupied:
            return "Ledig parkeringsplass"
        return f"Opptatt parkeringsplass med {self.car}"


spot = Spot()
car = Car("AB12345", "Tesla", "rød")
print(car)
print(spot)
in_time = datetime.datetime(2023, 11, 29, 13, 37)
spot.add_car(car, in_time)
print(spot)
out_time = datetime.datetime(2023, 11, 29, 14, 7)
spot.remove_car(out_time)
print(spot)

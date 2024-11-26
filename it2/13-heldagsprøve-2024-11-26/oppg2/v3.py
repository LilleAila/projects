"""
Lagt til parkeringshuset.

Car, Spot og Parking har alle en __repr__ metode, slik at det er lett å printe og gi info til brukeren.
Objektorientert med klasser for biler, plasser, parkeringshus
"""

import datetime


class Car:
    """Informasjon om biler."""
    def __init__(
        self, licenseplate: str, name: str, color: str, price_per_minute: float = 1.5
    ) -> None:
        self.licenseplate = licenseplate.upper()
        self.name = name.capitalize()
        self.color = color.lower()
        self.price_per_minute = price_per_minute

    # Det som vises når jeg bruker print(), str(), etc.
    def __repr__(self) -> str:
        return f"{self.color} {self.name} med reg.nr. {self.licenseplate}"


class BigCar(Car):
    # Arver fra Car. Eneste forskjell er at her er price_per_minute default som 3.0
    def __init__(self, licenseplate: str, name: str, color: str, price_per_minute=3.0):
        super().__init__(licenseplate, name, color, price_per_minute)


class Spot:
    """Hver parkeringsplass har sin egen instans av denne klassen."""
    def __init__(self, index: int) -> None:
        self.index = index
        self.occupied = False
        self.car = None
        self.in_time = None

    def add_car(self, car: Car, time: datetime.datetime) -> None:
        self.occupied = True
        self.car = car
        self.in_time = time
        print(f"Kjørt inn {self.car}, {time}")

    def remove_car(self, time: datetime.datetime) -> None:
        if not self.occupied:
            print("Det står ingen bil på denne plassen!")
            return
        assert isinstance(self.in_time, datetime.datetime)
        assert isinstance(self.car, Car)
        # https://stackoverflow.com/a/39181657
        time_parked = (time - self.in_time).total_seconds() / 60.0
        price = self.car.price_per_minute * time_parked
        print(
            f"Kjørt ut {self.car}. Prisen blir {price:.1f} kr etter {time_parked} min."
        )
        self.occupied = False
        self.car = None
        self.in_time = None

    # Hjelpefunksjoner for å lese attributter om bilen som står.
    def has_color(self, color) -> bool:
        # Må sjekke om plassen er opptatt eller ikke, fordi man kan ikke finne informasjon om en bil som ikke er der.
        if not self.occupied:
            return False
        # Hvis plassen er opptatt, men det ikke står en bil der, har noe galt skjedd.
        assert isinstance(self.car, Car)
        return self.car.color == color

    def has_licenseplate(self, licenseplate) -> bool:
        if not self.occupied:
            return False
        assert isinstance(self.car, Car)
        return self.car.licenseplate == licenseplate

    def get_car_info(self) -> str:
        if not self.occupied:
            return ""
        assert isinstance(self.car, Car)
        return f"{self.car.name}, {self.car.licenseplate}"

    # Slik at jeg ikke trenger å implementere mange egne funksjoner som formaterer klassen der den skal printes
    def __repr__(self) -> str:
        if not self.occupied:
            return f"{self.index:3}: Ledig parkeringsplass"
        return f"{self.index:3}: Opptatt parkeringsplass med {self.car}"


class Parking:
    """Parkeringsplass med alle plassene."""
    def __init__(self, num_spots: int) -> None:
        # spots har en index arg og det blir addet til start av __repr__
        self.num_spots = num_spots
        # Gir alle plassene en unik index.
        self.__spots = [Spot(i + 1) for i in range(num_spots)]

    @property
    def spots(self) -> str:
        # Parkeringsplasser, formatert som en string.
        return "\n".join(map(str, self.__spots))

    @property
    def free_spots(self) -> int:
        # return len([spot for spot in self.__spots if not spot.occupied])
        # https://stackoverflow.com/a/15375122
        return sum(not spot.occupied for spot in self.__spots)

    def print_available_spots(self) -> None:
        plural = self.free_spots > 1
        print(
            f"Det er {self.free_spots} ledig{'e' if plural else ''} plass{'er' if plural else ''} igjen!"
        )

    # Finne den første ledige plassen, og legge til en bil her.
    def add_car(self, car: Car, time: datetime.datetime):
        for spot in self.__spots:
            if spot.occupied:
                continue
            spot.add_car(car, time)
            self.print_available_spots()
            return
        print("Ingen ledige parkeringsplasser!")

    # Kjøre ut bilen fra en parkeringsplass
    def remove_car_by_index(self, index: int, time: datetime.datetime):
        if index > len(self.__spots) or index <= 0:
            print(
                f"Ugyldig parkeringsplass, {index}. Velg et tall mellom 1 og {len(self.__spots)}"
            )
            return
        self.__spots[index - 1].remove_car(time)
        self.print_available_spots()

    def remove_car(self, licenseplate: str, time: datetime.datetime) -> None:
        # Selv liker jeg bedre å bruke nummer for parkeringsplasser, heller enn registreringsnummer, men for å følge API fra oppgaven implementerer jeg dette.
        # https://stackoverflow.com/questions/7270321/finding-the-index-of-elements-based-on-a-condition-using-python-list-comprehensi
        index = [
            i
            for i, spot in enumerate(self.__spots)
            if spot.has_licenseplate(licenseplate)
        ]
        if len(index) == 0:
            print("Det er ingen biler med dette nummeret!")
            return
        self.remove_car_by_index(index[0] + 1, time)

    def get_cars_with_color(self, color: str) -> None:
        cars_with_color = filter(lambda spot: spot.has_color(color), self.__spots)
        cars = "\n".join([f"\t{spot.get_car_info()}" for spot in cars_with_color])
        print(f"Parkerte biler med fargen {color}:\n{cars}")

    def __repr__(self) -> str:
        return self.spots


if __name__ == "__main__":
    # Min originale kode for testing
    """
    # Litt rotete output, men hvis man kommenterer inn og ut deler av koden, er det ikke så vanskelig å forstå det.
    parking = Parking(20)
    print(parking)
    print(parking.free_spots)
    car = Car("AB12345", "Tesla", "rød")
    print(car)
    in_time = datetime.datetime(2023, 11, 29, 13, 37)
    parking.add_car(car, in_time)
    print(parking)
    for i in range(25):
        parking.add_car(car, in_time)
    print(parking)
    out_time = datetime.datetime(2023, 11, 29, 14, 7)
    parking.remove_car_by_index(18, out_time)
    parking.remove_car_by_index(18, out_time)
    parking.remove_car_by_index(25, out_time)
    print(parking)
    print(parking.free_spots)
    car2 = BigCar("TV12345", "Lastebil", "hvit")
    parking.add_car(car2, in_time)
    parking.get_cars_with_color("rød")
    parking.remove_car_by_index(18, out_time)
    """

    # Eksempelet fra oppgaven
    phus = Parking(20)
    bil = Car("AB12345", "Tesla", "r¢d")
    tid = datetime.datetime(2023, 11, 29, 13, 37)
    phus.add_car(bil, tid)

    bil = Car("EK13@02", "eGolf", "hvit")
    tid = datetime.datetime(2023, 11, 29, 13, 45)
    phus.add_car(bil, tid)

    tid = datetime.datetime(2023, 11, 29, 14, 41)
    phus.remove_car("AB12345", tid)

    bil = Car("TV12345", "Ford", "hvit")
    tid = datetime.datetime(2023, 11, 29, 14, 58)
    phus.add_car(bil, tid)

    bil = Car("VD77077", "Ford", "blå")
    tid = datetime.datetime(2023, 11, 29, 14, 55)
    phus.add_car(bil, tid)

    phus.get_cars_with_color("hvit")

    print(phus)

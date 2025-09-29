import datetime
from collections import defaultdict

type Temp = float
type State = bool
type Room = str


class RoomUnit:
    __slots__ = "__name", "__room"

    def __init__(
        self,
        name: str,
        room: Room,
    ) -> None:
        self.__name = name
        self.__room = room

    @property
    def name(self) -> str:
        return self.__name

    @property
    def room(self) -> Room:
        return self.__room

    def __repr__(self) -> str:
        return f"{self.name} i {self.room}"


class Light(RoomUnit):
    __slots__ = "__state"

    def __init__(
        self,
        room: Room,
        state: State = False,
    ) -> None:
        super().__init__("Lys", room)
        self.__state = state

    @property
    def state(self) -> State:
        return self.__state

    @state.setter
    def state(self, state: State) -> None:
        self.__state = state

    def toggle(self) -> None:
        self.state = not self.state

    def on(self) -> None:
        self.state = True

    def off(self) -> None:
        self.state = False

    @staticmethod
    def state_str(state: State) -> str:
        return "på" if state else "av"

    def __repr__(self) -> str:
        return f"{super().__repr__()}: {self.state_str(self.state)}"


class Thermostat(RoomUnit):
    def __init__(
        self,
        room: Room,
        temp: Temp = 22,
        min_temp: Temp = 5,
        max_temp: Temp = 30,
    ) -> None:
        super().__init__("Termostat", room)
        self.__temp = temp
        self.__min_temp = min_temp
        self.__max_temp = max_temp

    @property
    def temp(self) -> Temp:
        return self.__temp

    @temp.setter
    def temp(self, temp: Temp) -> None:
        if not self.__min_temp <= temp <= self.__max_temp:
            raise ValueError(f"Temperature {temp} is out of bounds!")
        self.__temp = temp

    def __str__(self):
        return f"Termostat i {self.room}: {self.temp}°C"


class SmartHouse:
    def __init__(
        self,
    ) -> None:
        self.__lights = defaultdict(list)
        self.__thermostats = dict()

    def add_light(self, room: Room, *args, **kwargs) -> None:
        self.__lights[room].append(Light(room, *args, **kwargs))

    def set_lights(self, room: Room, state: State) -> None:
        ls = self.__lights[room]
        if len(ls) == 0:
            raise ValueError(f"Det er ikke noen lys i {room}!")
        for l in ls:
            l.state = state
        print(f"Satte {len(ls)} lys i {room} til {Light.state_str(state)}")

    def set_all_lights(self, state: State) -> None:
        for k in self.__lights:
            self.set_lights(k, state)

    @property
    def lights(self) -> dict[str, list[Light]]:
        return dict(self.__lights)

    def add_thermostat(self, room: Room, *args, **kwargs) -> None:
        self.__thermostats[room] = Thermostat(room, *args, **kwargs)

    def set_temperature(self, room: Room, temp: Temp) -> None:
        t = self.__thermostats.get(room)
        if t is None:
            raise ValueError(f"Det er ikke en termostat i {room}!")
        t.temp = temp
        print(f"Satte temperatur i {room} til {t.temp}°C")

    @property
    def thermostats(self) -> dict[str, Thermostat]:
        return self.__thermostats

    def __repr__(self):
        return f"""
Smarthus:
Termostater:
{"\n".join(map(str, self.thermostats.values()))}

Lys:
{"\n".join(map(lambda l: "\n".join(map(str, l)), self.lights.values()))}"""


class AutomatedHouse(SmartHouse):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.add_units()

    def add_units(self):
        self.add_light("stue", True)
        self.add_light("stue", True)
        self.add_light("stue", True)

        self.add_light("kjøkken", False)
        self.add_light("kjøkken", False)

        self.add_light("bad", True)
        self.add_light("bad", True)

        self.add_light("soverom", False)
        self.add_light("soverom", False)
        self.add_light("soverom", False)
        self.add_light("soverom", False)

        self.add_thermostat("soverom", 23)
        self.add_thermostat("stue", 22)

    def update(self, time: datetime.time):
        # Ulike regler for oppdateringer
        # nedre grense med <=, øvre med <

        if datetime.time(22, 0) <= time or time < datetime.time(7, 0):  # over natten
            self.set_all_lights(False)
        elif datetime.time(7, 0) <= time < datetime.time(10, 0):
            self.set_lights("kjøkken", True)
            self.set_lights("soverom", True)
            self.set_lights("bad", True)
        elif datetime.time(10, 0) <= time < datetime.time(22, 0):
            self.set_lights("soverom", False)
            self.set_lights("stue", True)

        if datetime.time(23, 30) <= time or time < datetime.time(7, 0):
            self.set_temperature("soverom", 18)
            self.set_temperature("stue", 18)
        elif datetime.time(7, 0) <= time < datetime.time(23, 30):
            self.set_temperature("soverom", 23)
            self.set_temperature("stue", 22)


if __name__ == "__main__":
    automated_house = AutomatedHouse()
    print(automated_house)

    print()
    automated_house.update(datetime.time(6, 0))
    print(automated_house)

    print()
    automated_house.update(datetime.time(14, 0))
    print(automated_house)

    print()
    automated_house.update(datetime.time(23, 47))
    print(automated_house)

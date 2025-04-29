import datetime
from typing import Callable
from random import randint


class Unit:
    __slots__ = ("room", "name")

    def __init__(self, room: str, name: str) -> None:
        self.room = room
        self.name = name


class Light(Unit):
    def __init__(self, room: str, state=False):
        super().__init__(room, "Light")
        self.state = state

    def set_state(self, state):
        self.state = state

    def __repr__(self):
        return f"Light {self.state}"


class Thermostat(Unit):
    def __init__(self, room: str, temp=22):
        super().__init__(room, "Light")
        self.temp = temp

    def set_temp(self, temp):
        self.temp = temp

    def __repr__(self):
        return f"Thermostat {self.temp}°C"


class MovementSensor(Unit):
    def __init__(self, room: str, action: Callable) -> None:
        super().__init__(room, "Movement sensor")
        self.action = action

    def poll(self):
        if randint(1, 10) == 5:
            self.action()


class SmartHouse:
    def __init__(self):
        self.lights = list()
        self.thermostat: Thermostat

    def add_light(self, *args, **kwargs):
        self.lights.append(Light(*args, **kwargs))

    def set_lights(self, state):
        for l in self.lights:
            l.set_state(state)

    def set_thermostat(self, *args, **kwargs):
        self.thermostat = Thermostat(*args, **kwargs)

    def set_temp(self, temp):
        self.thermostat.set_temp(temp)

    def __repr__(self):
        return "\n".join(map(str, self.lights)) + "\n" + str(self.thermostat)


class AutomatedHouse(SmartHouse):
    def __init__(self):
        super().__init__()
        self.add_units()

    def add_units(self):
        self.set_thermostat("stue", 21)
        self.add_light("stue", True)
        self.add_light("stue", True)

    def update(self, time):
        if datetime.time(7, 0) < time < datetime.time(22, 0):
            self.set_lights(True)
            self.set_thermostat(22)
        else:
            self.set_lights(False)
            self.set_thermostat(20)

    def run(self):
        while True:
            time = datetime.datetime.now().time()
            self.update(time)


if __name__ == "__main__":
    house = AutomatedHouse()
    # house.run()

    # print(house)
    # house.update(datetime.time(3, 0))
    # print(house)
    # house.update(datetime.time(14, 0))
    # print(house)

    sensor = MovementSensor("garasje", self.handle_camera)
    [sensor.poll() for i in range(100)]

    sensor = MovementSensor("kjøkken", lambda: print("kjøkken!!!!!"))
    [sensor.poll() for i in range(100)]


def add(a, b):
    return a + b


_add = lambda a, b: a + b

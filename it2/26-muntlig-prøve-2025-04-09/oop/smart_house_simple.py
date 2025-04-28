import datetime


class Light:
    def __init__(self, state=False):
        self.state = state

    def set_state(self, state):
        self.state = state

    def __repr__(self):
        return f"Light {self.state}"


class Thermostat:
    def __init__(self, temp=22):
        self.temp = temp

    def set_temp(self, temp):
        self.temp = temp

    def __repr__(self):
        return f"Thermostat {self.temp}°C"


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
        self.set_thermostat(21)
        self.add_light(True)
        self.add_light(True)

    def update(self, time):
        if datetime.time(7, 0) < time < datetime.time(22, 0):
            self.set_lights(True)
            self.set_thermostat(22)
        else:
            self.set_lights(False)
            self.set_thermostat(20)


if __name__ == "__main__":
    house = AutomatedHouse()
    print(house)
    house.update(datetime.time(3, 0))
    print(house)
    house.update(datetime.time(14, 0))
    print(house)

import datetime
import pytest
from smart_house import (
    SmartHouse,
    AutomatedHouse,
)


def test_house_lights():
    house = SmartHouse()
    house.add_light("stue")
    assert len(house.lights["stue"]) == 1
    assert house.lights["stue"][0].state is False

    house.set_lights("stue", True)
    assert house.lights["stue"][0].state is True

    house.set_all_lights(False)
    assert house.lights["stue"][0].state is False

    with pytest.raises(ValueError, match="Det er ikke noen lys i kjeller!"):
        house.set_lights("kjeller", True)


def test_house_thermostat():
    house = SmartHouse()
    house.add_thermostat("bad", temp=20)
    assert house.thermostats["bad"].temp == 20

    house.set_temperature("bad", 25)
    assert house.thermostats["bad"].temp == 25

    with pytest.raises(ValueError, match="Det er ikke en termostat i loft!"):
        house.set_temperature("loft", 22)


def test_automated_house():
    house = AutomatedHouse()

    assert len(house.lights["stue"]) > 0
    assert len(house.lights["kjøkken"]) > 0
    assert len(house.lights["bad"]) > 0
    assert len(house.lights["soverom"]) > 0
    assert "soverom" in house.thermostats
    assert "stue" in house.thermostats

    house.update(datetime.time(8, 0))
    assert all(light.state for light in house.lights["kjøkken"])
    assert all(light.state for light in house.lights["soverom"])
    assert all(light.state for light in house.lights["bad"])
    assert house.thermostats["soverom"].temp == 23
    assert house.thermostats["stue"].temp == 22

    house.update(datetime.time(14, 0))
    assert all(not light.state for light in house.lights["soverom"])
    assert all(light.state for light in house.lights["stue"])
    assert house.thermostats["soverom"].temp == 23
    assert house.thermostats["stue"].temp == 22

    house.update(datetime.time(23, 45))
    assert house.thermostats["soverom"].temp == 18
    assert house.thermostats["stue"].temp == 18

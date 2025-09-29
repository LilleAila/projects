import pytest
from smart_house import (
    Thermostat,
)


def test_thermostat():
    thermostat = Thermostat("stue", 22, min_temp=5, max_temp=30)

    thermostat.temp = 10
    assert thermostat.temp == 10

    with pytest.raises(ValueError, match="Temperature 40 is out of bounds!"):
        thermostat.temp = 40

from smart_house import (
    Light,
)


def test_light():
    light = Light("kjøkken")
    assert light.state is False

    light.on()
    assert light.state is True

    light.off()
    assert light.state is False

    light.toggle()
    assert light.state is True

    assert Light.state_str(False) == "av"
    assert Light.state_str(True) == "på"

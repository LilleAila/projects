from main import Toggle


class TestToggle:
    def test_init(self):
        toggle = Toggle()
        assert toggle is not None
        assert isinstance(toggle, Toggle)
        assert toggle.enabled == False

    def test_enable(self):
        toggle = Toggle()
        toggle.enable()
        assert toggle.enabled == True

    def test_disable(self):
        toggle = Toggle()
        toggle.enable()
        toggle.disable()
        assert toggle.enabled == False

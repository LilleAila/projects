import pytest


class TestExceptions:
    def test_zero_division(self):
        with pytest.raises(ZeroDivisionError):
            3 / 0  # type: ignore

    def test_zero_division2(self):
        with pytest.raises(ZeroDivisionError, match="division by zero"):
            3 / 0  # type: ignore

    def test_zero_division3(self):
        with pytest.raises((ZeroDivisionError, ValueError)):
            3 / 0  # type: ignore

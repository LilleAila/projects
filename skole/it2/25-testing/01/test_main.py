# Testing with pytest
# Run `pytest`, will discover tests automatically
# , when file starts with `test_`

import main

# Pytest import is *not* necessary to run tests
# Is only necessary when using functions specifically from pytest
import pytest

import sys


def test_increment():
    assert main.increment(3) == 4


def test_decrement():
    assert main.decrement(5) == 4


@pytest.mark.parametrize("a,b", [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6)])
def test_increement(a, b):
    assert main.increment(a) == b


@pytest.mark.skip(reason="i no no want")
def test_linux():
    print("Will only run on linux")
    assert True


@pytest.mark.skipif(sys.version_info < (3, 10), reason="requires python 3.10 or higher")
def test_abc():
    print("abc")


@pytest.mark.xfail
def test_hmm():
    raise ValueError("abc")


@pytest.mark.xfail
def test_hmm2():
    assert True == True and False != True and False == False and isinstance(False, bool)

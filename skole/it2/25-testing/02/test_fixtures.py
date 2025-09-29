# NOTE: `pytest -sv` for more verbose logging
# Can also run `pytest -sv test_fixtures.py::TestFixtures::test_1`
# to run only one test
# `pytest --self-contained-html=testreport.html`
# requires pytest-html
# creates html file of test report

import pytest


# Scope bestemmer når dette skal kjører - for hver funksjon, hver class etc
# "class" kjører kun for hele testklassen, ikke per funksjon
# "class", "module", "package", "session"
@pytest.fixture(scope="class")
def fixture1():
    print("\nClass setUp")
    yield
    print("\nClass tearDown")


# Runs for every function in the class
# Passes the value from `yield` as an argument
# with the same name as the fixture name
@pytest.fixture
def fixture2():
    print("\nFunction setUp")
    yield 3
    print("\nFunction tearDown")


# Needs to be specified to use the class-scope fixtures
# Function fixtures are automatically applied
# (only if the argument with the same name is present)
@pytest.mark.usefixtures("fixture1")
class TestFixtures:
    def test_1(self, fixture2):
        print("test 1")
        assert True == True
        assert fixture2 == 3

    def test_2(self):
        print("test 2")
        assert False == False

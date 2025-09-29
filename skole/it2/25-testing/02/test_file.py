class TestFile:
    # tmp_path is a predefined fixture
    def test_write(self, tmp_path):
        path = tmp_path.joinpath("test.txt")
        with open(path, "w") as file:
            file.write("abc")
        with open(path, "r") as file:
            contents = file.read()
            assert contents == "abc"

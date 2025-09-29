import sys


class TestStdout:
    # capsys is a builtin fixture to pytest
    def test_stdout(self, capsys):
        print("abc")
        print("def", file=sys.stderr)
        result = capsys.readouterr()
        assert result.out == "abc\n"
        assert result.err == "def\n"

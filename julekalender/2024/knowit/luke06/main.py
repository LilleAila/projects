"""Tried python as well because the haskell solution didn't seem to work"""
"""Crashed after ~14 minutes"""

type display = list[bool]
type pos = tuple[int, int]

numbers: dict[str, display] = {
    "0": [True, True, True, True, True, True, False],
    "1": [False, True, True, False, False, False, False],
    "2": [True, True, False, True, True, False, True],
    "3": [True, True, True, True, False, False, True],
    "4": [False, True, True, False, False, True, True],
    "5": [True, False, True, True, False, True, True],
    "6": [True, False, True, True, True, True, True],
    "7": [True, True, True, False, False, False, False],
    "8": [True, True, True, True, True, True, True],
    "9": [True, True, True, True, False, True, True],
}

def number_to_segments(number: int) -> list[display]:
    number_string = f"{number:09d}"
    return [numbers[i] for i in number_string]

def swap_segments(displays: list[display], pos1: pos, pos2: pos) -> None:
    """Swap two segments in place (mutate the original matrix)"""
    # Just assume that this is within bounds
    # Remember: row first, then col
    # translates to display then segment
    row1, col1 = pos1
    row2, col2 = pos2

    displays[row1][col1], displays[row2][col2] = displays[row2][col2], displays[row1][col1]

def is_valid(number: int) -> bool:
    segments = number_to_segments(number)
    # Displays are 0-indexed from the right, segments are 0-indexed
    swap_segments(segments, (8, 2), (0, 1)) # AC <-> IB
    swap_segments(segments, (7, 3), (4, 6)) # BD <-> EG
    swap_segments(segments, (2, 5), (5, 4)) # GF <-> DE
    swap_segments(segments, (7, 0), (7, 1)) # BA <-> BB
    return segments == number_to_segments(number)

valid_numbers = [i for i in range(0, 1_000_000_000) if is_valid(i)]
print(len(valid_numbers))

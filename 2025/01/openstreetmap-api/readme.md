# Read data about buildings from OpenStreetMap

Inspired by [touch mapper](https://touch-mapper.org/en/).

## Usage

1. Get the coordinates.

```sh
python3 -m http.server
```

, then to go `localhost:8000/area.html` and draw a square to get the coordinates of the selected area.

2. Generate the 3d-model

```sh
python3 main.py
```

Enter the copied coordinates

3. Import `output.stl` to a slicer of choice

4. Profit

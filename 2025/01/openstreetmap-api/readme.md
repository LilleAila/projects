# Read data about buildings from OpenStreetMap

Create a `.stl` file containing a 3d model of a selected area on a map.
Inspired by [touch mapper](https://touch-mapper.org/en/).

Contains a simple website (99% written by ChatGPT) to select a rectangle from a map.

Dependencies are listed in `shell.nix`. Names can very between package managers.

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

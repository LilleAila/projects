from PIL import Image, ImageDraw
import re

with open("input.txt") as file:
    input = file.read().split()

def hex_to_rgb(hex):
    return tuple(int(hex[i:i+2], 16) for i in (0, 2, 4))

pixels = []
for i in input:
    match = re.search(r"\[(\d+),(\d+)\]\((\w+)\)", i)
    if match:
        pixels.append((int(match.group(1)), int(match.group(2)), hex_to_rgb(match.group(3))))

img = Image.new("RGB", (1024, 1024), (0, 0, 0))
draw = ImageDraw.Draw(img)

for x, y, color in pixels:
    img.putpixel((x, y), color)

img.save("output.png")
img.show()

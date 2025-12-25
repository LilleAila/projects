from PIL import Image, ImageDraw

hints = [
    (562, 292),
    (415, 318),
    (395, 339),
    (215, 458),
    (283, 553),
    (675, 478),
    (943, 500),
    (486, 586),
    (69, 906),
    (821, 717),
    (274, 900),
    (701, 723),
    (671, 812),
]

img = Image.open("juletreet.png")
draw = ImageDraw.Draw(img)

def draw_circle(draw, center, radius):
    cx, cy = center
    w, h = img.size

    # bounding square
    box = (
        max(0, cx - radius),
        max(0, cy - radius),
        min(w - 1, cx + radius),
        min(h - 1, cy + radius),
    )

    draw.ellipse(box, outline=(255, 0, 0, 255), width=3)

for i, c in enumerate(hints):
    try:
        p = img.getpixel(c)
        print(c, p)
        draw_circle(
            draw,
            c,
            50,
        )
        draw.point(c)
        draw.text(c, str(i), fill=(0, 0, 0, 255))
    except IndexError:
        print(c, "Out of bounds")

img.save("test.png")

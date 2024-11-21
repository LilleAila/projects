"""
Extemely simple pygame code, showing various ways to handle events
"""

import pygame as pg
import pygame.locals as kc

BG = (40, 40, 40)
FG = (235, 219, 178)

pg.init()
screen = pg.display.set_mode((800, 600))
pg.display.set_caption("Test of various events")
clock = pg.time.Clock()

running = True
while running:
    for event in pg.event.get():
        if event.type == pg.QUIT:
            running = False
        elif event.type == pg.KEYDOWN:
            """Only when key down, cannot repeat"""
            print(f"Key pressed: {pg.key.name(event.key)}")
        elif event.type == pg.MOUSEBUTTONUP:
            print(f"Mouse button {event.button} pressed at {event.pos}")
        elif event.type == pg.MOUSEMOTION:
            print(
                f"Mouse moved in direction {event.rel} to {event.pos}, with buttons {event.buttons} pressed."
            )

    """
    Will get all pressed keys, making it possible to handle repeating events
    """
    pressed_keys = pg.key.get_pressed()
    if pressed_keys[kc.K_a]:
        print("Key a is pressed!")

    screen.fill(BG)

    pg.display.update()
    clock.tick(60)

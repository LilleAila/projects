"""
Extemely simple pygame code, showing various ways to handle events
"""

import pygame as pg
import pygame.locals as kc

BG = (40, 40, 40)
FG = (235, 219, 178)

pg.init()
screen = pg.display.set_mode((800, 600))
pg.display.set_caption("balls")
clock = pg.time.Clock()
sprites = pg.sprite.Group()


class Ball(pg.sprite.Sprite):
    def __init__(self, x, y, velocity=(0, 0), *args):
        super().__init__(*args)

        self.velocity = velocity

        self.image = pg.Surface((50, 50), pg.SRCALPHA).convert_alpha()
        self.rect = self.image.get_rect()

        self.rect.x = x
        self.rect.y = y

        pg.draw.circle(self.image, FG, (25, 25), 25)

    def update(self, sprites):
        self.rect.move_ip(self.velocity)
        hits = [s for s in pg.sprite.spritecollide(self, sprites, False) if s != self]
        for hit in hits:
            hit.flip()
        if self.rect.x <= 0 or self.rect.x >= 800 - 50:
            self.flip_x()
        if self.rect.y <= 0 or self.rect.y >= 600 - 50:
            self.flip_y()

    def flip(self):
        vx, vy = self.velocity
        self.velocity = (-vx, -vy)

    def flip_y(self):
        vx, vy = self.velocity
        self.velocity = (vx, -vy)

    def flip_x(self):
        vx, vy = self.velocity
        self.velocity = (-vx, vy)


sprites.add(Ball(10, 70, (2, 1)))
sprites.add(Ball(80, 100, (-2, 1)))
sprites.add(Ball(80, 200, (1, 1)))
sprites.add(Ball(200, 80, (2, 1)))
sprites.add(Ball(500, 128, (2, 1)))
sprites.add(Ball(724, 388, (-2, -2)))
sprites.add(Ball(438, 512, (1, -1)))


running = True
while running:
    for event in pg.event.get():
        if event.type == pg.QUIT:
            running = False

    screen.fill(BG)

    sprites.update(sprites)
    sprites.draw(screen)

    pg.display.update()
    clock.tick(60)

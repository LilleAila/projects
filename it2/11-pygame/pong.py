import pygame as pg
import pygame.locals as kc
import random as rd

type Geometry = tuple[int, int]
type Position = tuple[int, int]
type Color = tuple[int, int, int]

class Colors:
    """Definere de ulike fargene jeg skal bruke"""
    BACKGROUND: Color = (40, 40, 40)
    FOREGROUND: Color = (235, 219, 178)


class Screen:
    """Ulike verdier om skjermen. Gjør heller dette enn å ha globale variabler."""
    __slots__ = ("dimensions", "padding", "fps", "surface", "available_area")

    def __init__(self, dimensions: Geometry = (600, 400), padding: int = 24, fps: int = 60) -> None:
        self.dimensions = dimensions
        self.padding = padding
        self.fps = fps

        (width, height) = self.dimensions
        self.available_area = ((padding, width - padding), (padding, height - padding))

    def open(self):
        self.surface = pg.display.set_mode(self.dimensions)

    @property
    def width(self):
        return self.dimensions[0]

    @property
    def height(self):
        return self.dimensions[1]

    @property
    def center(self) -> Position:
        # Dele og runde ned til int
        return (self.width // 2, self.height // 2)

class Sprite(pg.sprite.Sprite):
    __slots__ = ("screen", "__geometry")

    def __init__(self, position: Position, geometry: Geometry, screen: Screen) -> None:
        super().__init__()
        self.screen = screen
        self.__geometry = geometry

        # Gjennomsiktig bakgrunn: https://stackoverflow.com/a/328067
        self.image = pg.Surface(self.__geometry, pg.SRCALPHA).convert_alpha()
        self.rect = self.image.get_rect()

        (x, y) = position
        self.rect.x = x
        self.rect.y = y

    @property
    def width(self) -> int:
        return self.__geometry[0]

    @property
    def height(self) -> int:
        return self.__geometry[1]

    def handle_edge_collision(self) -> None:
        ((min_x, max_x), (min_y, max_y)) = self.screen.available_area
        x, y = self.rect.x, self.rect.y
        self.rect.x = min(max(x, min_x), max_x - self.rect.width)
        self.rect.y = min(max(y, min_y), max_y - self.rect.height)

    def update(self) -> None:
        self.handle_edge_collision()
        self.image.blit(self.image, self.rect)

class TextSprite(pg.sprite.Sprite):
    __slots__ = ("screen", "text", "__font", "__position")

    def __init__(self, screen: Screen, text: None | str = None, position: None | Position = None) -> None:
        super().__init__()
        self.screen = screen

        if text is not None:
            self.text = text
        if position is not None:
            self.__position = position

        self.__font = pg.font.SysFont("Arial", 30)
        self.image = self.__font.render(self.text, True, Colors.FOREGROUND)
        self.rect = self.image.get_rect()

    def update(self) -> None:
        self.image = self.__font.render(self.text, True, Colors.FOREGROUND)
        self.rect = self.image.get_rect()
        self.rect.topleft = self.position

    @property
    def position(self):
        return self.__position

class Score(TextSprite):
    def __init__(self, screen: Screen):
        self.score = (0, 0)
        super().__init__(screen)

    @property
    def position(self) -> Position:
        return (self.screen.center[0] - self.rect.width // 2, self.screen.padding)

    @property
    def score(self) -> str:
        return self.text

    @score.setter
    def score(self, scores: tuple[int, int]) -> None:
        score_l, score_r = scores
        self.text = f"{score_l} : {score_r}"

class Paddle(Sprite):
    __slots__ = ("__points")

    def __init__(self, position: Position, geometry: Geometry, screen: Screen) -> None:
        super().__init__(position, geometry, screen)
        self.handle_edge_collision()

        pg.draw.rect(self.image, Colors.FOREGROUND, (0, 0, self.width, self.height))

        self.__points = 0

    @property
    def points(self) -> int:
        return self.__points

    def gain_point(self) -> None:
        self.__points += 1

    def move(self, direction: Position) -> None:
        # In place, muterer rect direkte
        self.rect.move_ip(direction)

class Ball(Sprite):
    __slots__ = ("__velocity", "__radius")

    def __init__(self, position: Position, screen: Screen, radius: int, velocity: int) -> None:
        self.__radius = radius
        self.__velocity = (rd.choice([-velocity, velocity]), rd.choice([-velocity, velocity]))

        super().__init__(position, (radius * 2, radius * 2), screen)

        pg.draw.circle(self.image, Colors.FOREGROUND, (radius, radius), radius)

    def update(self) -> None:
        self.rect.move_ip(self.__velocity)
        self.handle_edge_collision()

    def handle_edge_collision(self) -> None:
        if self.rect.x >= self.screen.width - 2 * self.__radius or self.rect.x <= 0:
            self.flip_x()
        # TODO: should lose when hitting the right / left edges
        if self.rect.y >= self.screen.height - 2 * self.__radius or self.rect.y <= 0:
            self.flip_y()

    def flip_x(self) -> None:
        self.__velocity = (-self.__velocity[0], self.__velocity[1])

    def flip_y(self) -> None:
        self.__velocity = (self.__velocity[0], -self.__velocity[1])

    # TODO: if ball hits top / bottom of paddle, it bounces back and forth because of the flip_x
    # should flip y if it hits the bottom
    def check_paddle_collision(self, paddle: Paddle) -> bool:
        if self.rect.colliderect(paddle.rect):
            self.flip_x()
            paddle.gain_point()
            return True
        return False

class Game:
    __slots__ = ("__properties", "__running", "__clock", "__screen", "__sprites", "__paddle1", "__paddle2", "__ball1", "__score")

    def __init__(self, screen: Screen) -> None:
        self.__screen = screen
        self.__running = False

    def __handle_events(self) -> None:
        for event in pg.event.get():
            match event.type:
                case pg.QUIT:
                    self.__running = False
        self.__handle_keyboard()

    def __handle_keyboard(self) -> None:
        keys = pg.key.get_pressed()
        if keys[kc.K_ESCAPE]:
            self.__running = False
        if keys[kc.K_j]:
            self.__paddle2.move((0, 10))
        if keys[kc.K_k]:
            self.__paddle2.move((0, -10))
        if keys[kc.K_f]:
            self.__paddle1.move((0, 10))
        if keys[kc.K_d]:
            self.__paddle1.move((0, -10))

    def __update(self) -> None:
        # Linjen under ville sendt keys til alle sprites. Jeg velger heller å håndtere all input direkte i __handle_keyboard.
        # self.__sprites.update(pg.key.get_pressed())
        self.__sprites.update()
        self.__ball1.check_paddle_collision(self.__paddle1)
        self.__ball1.check_paddle_collision(self.__paddle2)
        self.__score.score = (self.__paddle1.points, self.__paddle2.points)

    def __draw(self) -> None:
        self.__screen.surface.fill(Colors.BACKGROUND)
        self.__sprites.draw(self.__screen.surface)
        pg.display.flip()

    def run(self) -> None:
        pg.init()
        self.__running = True
        self.__screen.open()
        self.__clock = pg.time.Clock()
        self.__sprites = pg.sprite.Group()

        # Initialiserer sprites helt i hjørnet. Kollisjon blir sjekket i __init__, slik at den plasseres riktig
        self.__paddle1 = Paddle((0, 0), (15, 100), self.__screen)
        self.__sprites.add(self.__paddle1)
        self.__paddle2 = Paddle((self.__screen.width, 0), (15, 100), self.__screen)
        self.__sprites.add(self.__paddle2)
        self.__ball1 = Ball(self.__screen.center, self.__screen, 15, 5)
        self.__sprites.add(self.__ball1)
        self.__score = Score(self.__screen)
        self.__sprites.add(self.__score)

        while self.__running:
            self.__handle_events()
            self.__update()
            self.__draw()
            self.__clock.tick(self.__screen.fps)


if __name__ == "__main__":
    app = Game(Screen())
    app.run()

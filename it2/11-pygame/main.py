import pygame as pg
import pygame.locals as kc

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

class Paddle(pg.sprite.Sprite):
    __slots__ = ("__position", "__geometry", "__screen", "__rect")

    def __init__(self, position: Position, geometry: Geometry, screen: Screen) -> None:
        super().__init__()
        self.__position = position
        self.__geometry = geometry
        self.__screen = screen

    @property
    def position(self) -> Position:
        return self.__position

    def move(self, direction: Position):
        (dir_x, dir_y) = direction
        (pos_x, pos_y) = self.__position
        (width, height) = self.__geometry
        ((min_x, max_x), (min_y, max_y)) = self.__screen.available_area
        self.__position = (min(max(pos_x + dir_x, min_x), max_x - width), min(max(pos_y + dir_y, min_y), max_y - height))

    def update(self, keys):
        print(keys)

    def draw(self) -> None:
        (x_1, y_1) = self.__position
        (width, height) = self.__geometry
        x_2, y_2 = x_1 + width, y_1 + height
        self.__rect = pg.draw.rect(self.__screen.surface, Colors.FOREGROUND, (x_1, y_1, x_2, y_2))


class App:
    __slots__ = ("__properties", "__running", "__clock", "__screen", "__sprites", "__paddle1")

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
        if keys[kc.K_j]:
            self.__paddle1.move((0, 10))
        if keys[kc.K_k]:
            self.__paddle1.move((0, -10))

    def __update(self) -> None:
        self.__sprites.update(pg.key.get_pressed())

    def __draw(self) -> None:
        self.__screen.surface.fill(Colors.BACKGROUND)
        self.__sprites.draw(self.__screen.surface)

        self.__paddle1.draw()

        pg.display.flip()

    def run(self) -> None:
        pg.init()
        self.__running = True
        self.__screen.open()
        self.__clock = pg.time.Clock()
        self.__sprites = pg.sprite.Group()

        self.__paddle1 = Paddle((50, 50), (1, 100), self.__screen)

        while self.__running:
            self.__handle_events()
            self.__update()
            self.__draw()
            self.__clock.tick(self.__screen.fps)


if __name__ == "__main__":
    app = App(Screen())
    app.run()

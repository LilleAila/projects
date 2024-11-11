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
    __slots__ = ("dimensions", "padding", "fps", "surface")

    def __init__(self, dimensions: Geometry = (600, 400), padding: int = 24, fps: int = 60) -> None:
        self.dimensions = dimensions
        self.padding = padding
        self.fps = fps

    def open(self):
        self.surface = pg.display.set_mode(self.dimensions)

class Paddle:
    __slots__ = ("__position", "__geometry", "__screen", "__rect")

    def __init__(self, position: Position, geometry: Geometry, screen: Screen) -> None:
        self.__position = position
        self.__geometry = geometry
        self.__screen = screen

    @property
    def position(self) -> Position:
        return self.__position

    @position.setter
    def position(self, new_position: Position) -> Position:
        self.__position = new_position
        return self.__position

    def draw(self) -> None:
        (x_1, y_1) = self.__position
        (width, height) = self.__geometry
        x_2, y_2 = x_1 + width, y_1 + height
        # x_1, y_1 = x - width / 2, y - height / 2
        # x_2, y_2 = x + width / 2, y + height / 2
        self.__rect = pg.draw.rect(self.__screen.surface, Colors.FOREGROUND, (x_1, y_1, x_2, y_2))


class App:
    __slots__ = ("__properties", "__running", "__clock", "__screen", "__sprites")

    def __init__(self, screen: Screen) -> None:
        self.__screen = screen
        self.__running = False

    def __handle_events(self) -> None:
        for event in pg.event.get():
            match event.type:
                case pg.QUIT:
                    self.__running = False

    def __update(self) -> None:
        self.__sprites.update()

    def __draw(self) -> None:
        self.__screen.surface.fill(Colors.BACKGROUND)
        self.__sprites.draw(self.__screen.surface)

        paddle1 = Paddle((50, 50), (25, 100), self.__screen)
        paddle1.draw()

        pg.display.flip()

    def run(self) -> None:
        pg.init()
        self.__running = True
        self.__screen.open()
        self.__clock = pg.time.Clock()
        self.__sprites = pg.sprite.Group()
        while self.__running:
            self.__handle_events()
            self.__update()
            self.__draw()
            self.__clock.tick(self.__screen.fps)


if __name__ == "__main__":
    app = App(Screen())
    app.run()

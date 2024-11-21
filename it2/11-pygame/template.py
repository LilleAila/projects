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

class Game:
    __slots__ = ("__properties", "__running", "__clock", "__screen", "__sprites")

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

    def __update(self) -> None:
        self.__sprites.update()

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

        while self.__running:
            self.__handle_events()
            self.__update()
            self.__draw()
            self.__clock.tick(self.__screen.fps)


if __name__ == "__main__":
    app = Game(Screen())
    app.run()

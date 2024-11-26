"""
Oppgave 1. Bruker sprites for å holde styr på flere objekter i spillet.
Basert på min egen template på github under it2/11-pygame.
"""

import pygame as pg
import pygame.locals as kc
from math import floor

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

    def __init__(
        self, dimensions: Geometry = (600, 400), padding: int = 24, fps: int = 60
    ) -> None:
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


class TextSprite(pg.sprite.Sprite):
    __slots__ = ("screen", "text", "__font", "__position")

    def __init__(
        self,
        screen: Screen,
        text: None | str = None,
        position: None | Position = None,
        size: int = 30,
    ) -> None:
        super().__init__()
        self.screen = screen

        if text is not None:
            self.text = text
        if position is not None:
            self.__position = position

        self.__font = pg.font.SysFont("Arial", size)
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
    __slots__ = "__score"

    def __init__(self, screen: Screen) -> None:
        self.score = 0
        super().__init__(screen)

    @property
    def position(self) -> Position:
        return (self.screen.center[0] - self.rect.width // 2, self.screen.padding)

    @property
    def score(self) -> int:
        return self.__score

    @score.setter
    def score(self, score) -> None:
        self.__score = score
        self.text = str(self.__score)


class Time(TextSprite):
    __slots__ = "__time"

    def __init__(self, screen: Screen) -> None:
        self.time = 0
        super().__init__(screen)

    @property
    def position(self) -> Position:
        return (
            self.screen.center[0] - self.rect.width // 2,
            self.screen.height - self.screen.padding - self.rect.height,
        )

    @property
    def time(self) -> int:
        return self.__time

    @time.setter
    def time(self, time: int) -> None:
        self.__time = time
        # https://stackoverflow.com/a/339013
        seconds = floor(self.__time / 1000)
        self.text = f"{seconds:02}"


class Result(TextSprite):
    def __init__(self, screen: Screen) -> None:
        super().__init__(screen, size=50, text="")

    @property
    def position(self) -> Position:
        return (
            self.screen.center[0] - self.rect.width // 2,
            self.screen.center[1] - self.rect.height // 2,
        )

    def show_result(self, score: int) -> None:
        self.text = f"Du fikk {score} poeng!"

    def hide_result(self) -> None:
        self.text = ""


class Game:
    __slots__ = (
        "__running",
        "__clock",
        "__screen",
        "__sprites",
        "__score",
        "__time",
        "__result",
        "__game_running",
    )

    def __init__(self, screen: Screen) -> None:
        self.__screen = screen
        self.__running = False

    def __handle_events(self) -> None:
        for event in pg.event.get():
            match event.type:
                case pg.QUIT:
                    self.__running = False
                case pg.MOUSEBUTTONUP:
                    if self.__game_running:
                        self.__score.score += 1
        self.__handle_keyboard()

    def __handle_keyboard(self) -> None:
        keys = pg.key.get_pressed()
        if keys[kc.K_ESCAPE]:
            self.__running = False

    def __update(self) -> None:
        if self.__game_running:
            self.__time.time += self.__clock.get_time()
        if self.__time.time >= 10000:
            self.__game_running = False
            self.__result.show_result(self.__score.score)
        self.__sprites.update()

    def __draw(self) -> None:
        self.__screen.surface.fill(Colors.BACKGROUND)
        self.__sprites.draw(self.__screen.surface)
        pg.display.flip()

    def run(self) -> None:
        pg.init()
        self.__running = True
        self.__screen.open()
        pg.display.set_caption("KlikkKlikkKlikk")
        self.__clock = pg.time.Clock()
        self.__sprites = pg.sprite.Group()

        self.__score = Score(self.__screen)
        self.__sprites.add(self.__score)

        self.__time = Time(self.__screen)
        self.__sprites.add(self.__time)

        self.__result = Result(self.__screen)
        self.__sprites.add(self.__result)

        self.__game_running = True

        while self.__running:
            self.__handle_events()
            self.__update()
            self.__draw()
            self.__clock.tick(self.__screen.fps)


if __name__ == "__main__":
    game = Game(Screen())
    game.run()

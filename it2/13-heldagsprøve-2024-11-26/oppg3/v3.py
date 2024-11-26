"""
Oppgave 3. Spilleren må trykke innenfor et rektangel.
"""

import pygame as pg
import pygame.locals as kc
from math import floor
from random import randint  # Bakgrunnsfarge
import datetime  # Lagring av resultater

# Definerer typer for å få bedre oversikt
type Geometry = tuple[int, int]
type Position = tuple[int, int]
type Color = tuple[int, int, int]


# Definere ulike konstanter
class Colors:
    BACKGROUND: Color = (40, 40, 40)
    FOREGROUND: Color = (235, 219, 178)


ROUND_DURATION: int = 10
PAUSE_DURATION: int = 3
# Kan endre vanskelighetsgrad med å endre størrelsen eller hvor hyppig den flytter på seg
TARGET_MOVE_TIME: int = 2
TARGET_SIZE: Geometry = (100, 100)

# Hjelpefunksjoner
def random_color(min: int, max: int) -> Color:
    return (randint(min, max), randint(min, max), randint(min, max))


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


class Sprite(pg.sprite.Sprite):
    """Klasse for å gjøre det litt enklere å jobbe med sprites"""
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

    def update(self) -> None:
        self.image.blit(self.image, self.rect)


class TextSprite(pg.sprite.Sprite):
    """Det samme som over, men for tekst"""
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


class Time(TextSprite):
    """Tekst-sprite for å telle tiden"""
    __slots__ = "__time"

    def __init__(self, screen: Screen) -> None:
        self.time = 0
        super().__init__(screen)

    @property
    def position(self) -> Position:
        return (self.screen.center[0] - self.rect.width // 2, self.screen.padding)

    @property
    def time(self) -> int:
        return self.__time

    @time.setter
    def time(self, time: int) -> None:
        self.__time = time
        # Visningen skal telle nedover heller enn oppover.
        seconds = ROUND_DURATION - floor(self.time / 1000)
        # https://stackoverflow.com/a/339013
        self.text = f"{seconds:02}"


class Result(TextSprite):
    """En sprite som vises på slutten av runden for å si resultatet."""
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


class Target(Sprite):
    """Sprite for rektangelet man skal trykke på"""
    def __init__(self, geometry: Geometry, screen: Screen) -> None:
        super().__init__((0, 0), geometry, screen)
        pg.draw.rect(self.image, Colors.FOREGROUND, (0, 0, self.width, self.height))
        self.move()

    def move(self) -> None:
        ((min_x, max_x), (min_y, max_y)) = self.screen.available_area
        max_x -= self.rect.width
        max_y -= self.rect.height
        self.rect.x = randint(min_x, max_x)
        self.rect.y = randint(min_y, max_y)


class Game:
    """Selve spillet"""
    # Definerer alle ulike attributter her i __slots__ (kun for å få bedre oversikt)
    __slots__ = (
        "__running",
        "__clock",
        "__screen",
        "__sprites",
        "__score",
        "__time",
        "__result",
        "__game_running",
        "__bg",
        "__round_end",
        "__best_score",
        "__target",
        "__last_move",
    )

    def __init__(self, screen: Screen) -> None:
        self.__screen = screen
        self.__running = False

    def __handle_events(self) -> None:
        # Gå gjennom events.
        for event in pg.event.get():
            match event.type:
                case pg.QUIT:
                    self.__running = False
                case pg.MOUSEBUTTONUP:
                    if self.__game_running:
                        # Gi poeng hvis musen trykker innenfor rektangelet.
                        if self.__target.rect.collidepoint(event.pos):
                            self.__score += 1
        self.__handle_keyboard()

    def __handle_keyboard(self) -> None:
        keys = pg.key.get_pressed()
        # Spillet kan også stoppes med escape
        if keys[kc.K_ESCAPE]:
            self.__running = False

    def __update(self) -> None:
        if self.__game_running:
            # Øke tiden på klokken.
            self.__time.time += self.__clock.get_time()
            if self.__time.time - self.__last_move >= TARGET_MOVE_TIME * 1000:
                # Flytte rektangelet hvis det har gått lang nok tid
                self.__target.move()
                self.__last_move = self.__time.time
            if self.__time.time >= ROUND_DURATION * 1000:
                # Stoppe spillet etter runden er ferdig, vise resultat
                self.__game_running = False
                self.__result.show_result(self.__score)
                self.__best_score = max(self.__score, self.__best_score) # Lagre den beste poengsummen
                self.__round_end = pg.time.get_ticks()
        elif pg.time.get_ticks() - self.__round_end >= PAUSE_DURATION * 1000:
            # Starte spillet igjen etter pausetiden har gått ut.
            # Resette variabler for en ny runde
            self.__result.hide_result()
            self.__game_running = True
            self.__bg = random_color(0, 100)
            self.__score = 0
            self.__time.time = 0
            self.__last_move = 0
        self.__sprites.update()

    def __draw(self) -> None:
        self.__screen.surface.fill(self.__bg)
        self.__sprites.draw(self.__screen.surface)
        pg.display.flip()

    def run(self) -> None:
        pg.init()
        # Initialisere variabler for spillet
        self.__running = True
        self.__screen.open()
        pg.display.set_caption("KlikkKlikkKlikk")
        self.__clock = pg.time.Clock()
        self.__sprites = pg.sprite.Group()
        self.__bg = Colors.BACKGROUND

        self.__last_move = 0
        self.__round_end = 0

        self.__score = 0
        self.__best_score = 0

        # Legge til alle sprites
        self.__time = Time(self.__screen)
        self.__sprites.add(self.__time)

        self.__result = Result(self.__screen)
        self.__sprites.add(self.__result)

        self.__target = Target(TARGET_SIZE, self.__screen)
        self.__sprites.add(self.__target)

        self.__game_running = True

        while self.__running:
            self.__handle_events()
            self.__update()
            self.__draw()
            self.__clock.tick(self.__screen.fps)

        # Printe beste poengsum og skrive til en fil (beholder tidligere resultater)
        print(f"\nBeste poengsum: {self.__best_score}")
        if self.__best_score > 0:
            with open("./high_scores.txt", "a") as file:
                # https://stackoverflow.com/a/28147286
                file.write(
                    f"Beste poengsum {datetime.datetime.now().replace(microsecond=0).isoformat()} - {self.__best_score}\n"
                )


if __name__ == "__main__":
    # Kjøre spillet!!!!
    game = Game(Screen())
    game.run()

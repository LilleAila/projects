import pygame as pg
import pygame.locals as kc

WIDTH, HEIGHT = 400, 600
FPS = 24


class ScreenProperties:
    __slots__ = ("dimensions", "fps")

    def __init__(self, dimensions: tuple[int, int], fps: int) -> None:
        self.dimensions = dimensions
        self.fps = fps


class App:
    __slots__ = ("__properties", "__running", "__clock", "__screen", "__sprites")

    def __init__(self, properties: ScreenProperties) -> None:
        self.__properties = properties
        self.__running = False

    def Handle_events(self) -> None:
        for event in pg.event.get():
            match event.type:
                case pg.QUIT:
                    self.__running = False

    def Update(self) -> None:
        self.__sprites.update()

    def Draw(self) -> None:
        self.__screen.fill("black")
        self.__sprites.draw(self.__screen)
        pg.display.update()

    def Run(self) -> None:
        pg.init()
        self.__running = True
        self.__screen = pg.display.set_mode(self.__properties.dimensions)
        self.__clock = pg.time.Clock()
        self.__sprites = pg.sprite.Group()
        while self.__running:
            self.Handle_events()
            self.Update()
            self.Draw()
            self.__clock.tick(self.__properties.fps)


if __name__ == "__main__":
    app = App(ScreenProperties((400, 600), 24))
    app.Run()

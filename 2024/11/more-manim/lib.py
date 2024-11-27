from manim import *
from pydub import AudioSegment
from contextlib import contextmanager


@contextmanager
def play_audio(scene: Scene, audio_file: str):
    """Play audio while animation is playing inside with"""
    audio = AudioSegment.from_file(audio_file)
    audio_duration = len(audio) / 1000.0
    scene.add_sound(audio_file)
    start_time = scene.renderer.time
    yield audio_duration  # Executes code inside the "with"
    delta_time = scene.renderer.time - start_time
    remaining_time = max(0, audio_duration - delta_time)
    print(delta_time, remaining_time)
    scene.wait(remaining_time)


class DiscontinuousExcl(VMobject):
    def __init__(self, size=0.4, **kwargs):
        super().__init__(**kwargs)

        l1 = Line(ORIGIN, size * RIGHT)
        l2 = Line(ORIGIN, size * UP)
        # Invisible lines, because it by default sets the origin to the center, rather than corner.
        l3 = Line(ORIGIN, size * LEFT).set_opacity(0)
        l4 = Line(ORIGIN, size * DOWN).set_opacity(0)
        self.add(l1, l2, l3, l4)


class DiscontinuousIncl(VMobject):
    def __init__(self, size=0.3, **kwargs):
        super().__init__(**kwargs)

        l1 = Line(ORIGIN, size * UP)
        l2 = Line(ORIGIN, size * DOWN)
        l3 = Line(size * UP, size * UP + size / 2 * RIGHT)
        l4 = Line(size * DOWN, size * DOWN + size / 2 * RIGHT)
        l5 = Line(ORIGIN, size / 2 * LEFT).set_opacity(0)
        self.add(l1, l2, l3, l4, l5)


class PointOnGraph(VMobject):
    __slots__ = ("tracker", "point", "__name", "__f")

    def __init__(
        self, ax, f, name, initial_value, scale=0.5, direction=DR, color=WHITE, **kwargs
    ):
        # Possible improvement: use MoveAlongPath
        # https://docs.manim.community/en/stable/reference/manim.animation.movement.MoveAlongPath.html#movealongpath
        # The only problem is it would be harder to position the label.
        # Could maybe make the label + dot a "static" mobject and move the entire thing?
        super().__init__(**kwargs)

        self.__name = name
        self.__f = f

        self.tracker = ValueTracker(initial_value)
        self.point = Dot(
            point=ax.c2p(self.tracker.get_value(), f(self.tracker.get_value())),
            color=color,
        )
        self.point.add_updater(
            lambda x: x.move_to(
                ax.c2p(self.tracker.get_value(), f(self.tracker.get_value()))
            )
        )
        point_label = (
            Text(self.make_label(), color=color)
            .scale(scale)
            .move_to(
                ax.c2p(self.tracker.get_value(), f(self.tracker.get_value()))
                + 0.5 * direction
            )
        )

        point_label.add_updater(
            lambda x: x.become(
                Text(self.make_label(), color=color).scale(scale)
            ).move_to(
                ax.c2p(self.tracker.get_value(), f(self.tracker.get_value()))
                + 0.5 * direction
            )
        )

        self.add(self.point, point_label)

    def make_label(self) -> str:
        return f"{self.__name} = ({self.tracker.get_value():.1f}, {self.__f(self.tracker.get_value()):.1f})"

    def move_point(self, x):
        return self.tracker.animate.set_value(x)

    def get_dot_center(self):
        return self.point.get_center()


class PointOnGraphFixedLabel(PointOnGraph):
    __slots__ = "__label"

    def __init__(self, label, ax, f, initial_value, **kwargs):
        self.__label = label
        super().__init__(ax, f, "", initial_value, **kwargs)

    def make_label(self) -> str:
        return self.__label

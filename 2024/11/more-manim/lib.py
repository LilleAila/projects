from manim import *


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
    __slots__ = ("tracker", "point")

    def __init__(self, ax, f, name, initial_value, direction=DR, color=WHITE, **kwargs):
        # Possible improvement: use MoveAlongPath
        # https://docs.manim.community/en/stable/reference/manim.animation.movement.MoveAlongPath.html#movealongpath
        # The only problem is it would be harder to position the label.
        # Could maybe make the label + dot a "static" mobject and move the entire thing?
        super().__init__(**kwargs)

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
            Text(
                f"{name} = ({self.tracker.get_value():.1f}, {f(self.tracker.get_value()):.1f})",
                color=color,
            )
            .scale(0.5)
            .move_to(
                ax.c2p(self.tracker.get_value(), f(self.tracker.get_value()))
                + 0.5 * direction
            )
        )

        point_label.add_updater(
            lambda x: x.become(
                Text(
                    f"{name} = ({self.tracker.get_value():.1f}, {f(self.tracker.get_value()):.1f})",
                    color=color,
                ).scale(0.5)
            ).move_to(
                ax.c2p(self.tracker.get_value(), f(self.tracker.get_value()))
                + 0.5 * direction
            )
        )

        self.add(self.point, point_label)

    def move_point(self, x):
        return self.tracker.animate.set_value(x)

    def get_dot_center(self):
        return self.point.get_center()

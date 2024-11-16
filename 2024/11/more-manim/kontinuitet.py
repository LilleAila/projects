from manim import *
from math import radians

f1 = lambda x: x
f2 = lambda x: x + 1
f = lambda x: f1(x) if x < 0 else f2(x)


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


class Scene(MovingCameraScene):
    def construct(self):
        self.camera.frame.save_state()

        ax = Axes(
            x_range=[-3, 3, 1],
            y_range=[-2, 2, 1],
            axis_config={"color": GREEN, "include_numbers": True},
            tips=False,
        )

        labels = ax.get_axis_labels()

        graph = ax.plot(f, discontinuities=[0], dt=0.01, color=BLUE)
        graph_label = ax.get_graph_label(
            graph,
            r"\displaystyle f \left( x \right) = \begin{cases} x & \text{, } x < 0 \\ x + 1 & \text{, } x \geq 0 \end{cases}",
            x_val=1,
            direction=DOWN + RIGHT,
        ).scale(0.8)

        m1 = (
            DiscontinuousExcl()
            .set_color(BLUE)
            .move_to(ax.c2p(0, f1(0)))
            .rotate(radians(180))
        )
        m2 = (
            DiscontinuousIncl()
            .set_color(BLUE)
            .move_to(ax.c2p(0, f2(0)))
            .rotate(radians(45))
        )

        self.play(Create(ax), Create(labels))
        self.wait()
        self.play(Create(graph_label))
        self.play(Create(graph), run_time=2)
        # self.play(Create(dot_left), Create(dot_right))
        self.play(Create(m1), Create(m2))

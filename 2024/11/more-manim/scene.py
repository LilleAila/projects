from manim import *

f = lambda x: 1 / 2 * np.power(x, 3) - 3 * x


class Scene(Scene):
    def construct(self):
        ax = Axes(
            x_range=[-4, 4, 1],
            y_range=[-4, 4, 1],
            axis_config={"color": GREEN, "include_numbers": True},
            tips=False,
        )

        labels = ax.get_axis_labels()

        graph = ax.plot(f, color=BLUE)
        graph_label = ax.get_graph_label(
            graph, "f(x) = \\frac{1}{2} x^3-3x", x_val=-2, direction=UP + LEFT
        )

        # https://docs.manim.community/en/stable/examples.html#argminexample
        t1 = ValueTracker(3)
        dot1 = Dot(point=[ax.c2p(t1.get_value(), f(t1.get_value()))])
        dot1.add_updater(lambda x: x.move_to(ax.c2p(t1.get_value(), f(t1.get_value()))))

        t2 = ValueTracker(2)
        dot2 = Dot(point=[ax.c2p(t2.get_value(), f(t2.get_value()))])
        dot2.add_updater(lambda x: x.move_to(ax.c2p(t2.get_value(), f(t2.get_value()))))

        l1 = Line(dot1.get_center(), dot2.get_center()).set_length(100).set_color(RED)
        l1.add_updater(
            lambda x: x.become(
                Line(dot1.get_center(), dot2.get_center())
                .set_length(100)
                .set_color(RED)
            )
        )

        dot1_label = Text(f"x={t1.get_value():1f}", color=WHITE).scale(0.5)
        dot1_label.add_updater(
            lambda x: x.become(
                Text(f"x={t1.get_value():.1f}", color=WHITE).scale(0.5)
            ).move_to(ax.c2p(t1.get_value(), f(t1.get_value())) + 0.5 * DR)
        )

        dot2_label = Text(f"x={t2.get_value():1f}", color=WHITE).scale(0.5)
        dot2_label.add_updater(
            lambda x: x.become(
                Text(f"x={t2.get_value():.1f}", color=WHITE).scale(0.5)
            ).move_to(ax.c2p(t2.get_value(), f(t2.get_value())) + 0.5 * DR)
        )

        labels = VGroup(labels, graph_label)
        self.add(ax, graph, labels, dot1, dot1_label, dot2, dot2_label, l1)

        self.play(t1.animate.set_value(2.5), run_time=2)

        self.wait(1)

        self.play(t1.animate.set_value(2.01), run_time=2)

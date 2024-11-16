from manim import *

f = lambda x: 1 / 2 * np.power(x, 3) - 3 * x


class Scene(MovingCameraScene):
    def construct(self):
        self.camera.frame.save_state()

        ax = Axes(
            x_range=[-4, 8, 1],
            y_range=[-4, 4, 1],
            axis_config={"color": GREEN, "include_numbers": True},
            tips=False,
        )

        labels = ax.get_axis_labels()

        graph = ax.plot(f, color=BLUE)
        graph_label = ax.get_graph_label(
            graph, "f(x) = \\frac{1}{2} x^3-3x", x_val=-2, direction=UP + LEFT
        ).scale(0.6)

        # https://docs.manim.community/en/stable/examples.html#argminexample
        t1 = ValueTracker(3)
        dot1 = Dot(point=[ax.c2p(t1.get_value(), f(t1.get_value()))])
        dot1.add_updater(lambda x: x.move_to(ax.c2p(t1.get_value(), f(t1.get_value()))))

        t2 = ValueTracker(2)
        dot2 = Dot(point=[ax.c2p(t2.get_value(), f(t2.get_value()))])
        dot2.add_updater(lambda x: x.move_to(ax.c2p(t2.get_value(), f(t2.get_value()))))

        l1 = Line(dot1.get_center(), dot2.get_center()).set_length(15).set_color(RED)
        l1.add_updater(
            lambda x: x.become(
                Line(dot1.get_center(), dot2.get_center()).set_length(15).set_color(RED)
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

        dots = VGroup(dot1, dot1_label, dot2, dot2_label)

        self.play(Create(ax))
        self.play(Create(labels))
        self.wait()
        self.play(Create(graph), run_time=2)
        self.play(Create(graph_label))
        self.wait(0.4)
        self.play(Create(dots))
        self.wait(0.5)
        self.play(Create(l1), run_time=1)

        equations = [
            r"f' \left( x \right) = \lim_{\Delta x \to 0} \frac{\Delta f \left( x \right)}{\Delta x}",
            r"f' \left( x \right) = \lim_{\Delta x \to 0} \frac{f \left( x + \Delta x \right) - f \left( x \right)}{\Delta x}",
            r"f' \left( x \right) = \lim_{h \to 0} \frac{f \left( x + h \right) - f \left( x \right)}{h}",
        ]

        eq = MathTex(equations[0]).scale(0.6).next_to(dot1, RIGHT)
        self.play(self.camera.frame.animate.scale(0.7).move_to(eq))

        self.play(Create(eq))
        self.wait(2)
        self.play(Transform(eq, MathTex(equations[1]).scale(0.6).next_to(dot1, RIGHT)))
        self.wait(4)
        self.play(Transform(eq, MathTex(equations[2]).scale(0.6).next_to(dot1, RIGHT)))
        self.wait(2)

        self.play(Restore(self.camera.frame))
        self.play(Uncreate(eq))

        self.wait(0.5)

        self.play(t1.animate.set_value(2.5), run_time=2)

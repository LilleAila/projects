from manim import *
from math import radians

from lib import DiscontinuousExcl, DiscontinuousIncl, PointOnGraph


class Kontinuitet(MovingCameraScene):
    def construct(self):
        # Save the camera state for moving it
        # https://docs.manim.community/en/stable/examples.html#special-camera-settings
        self.camera: MovingCamera  # LSP doesn't understand that
        self.camera.frame.save_state()

        ### Draw axes and the graph of the function.
        f1 = lambda x: x
        f2 = lambda x: x + 1
        f = lambda x: f1(x) if x < 0 else f2(x)

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
        self.wait(1)
        self.play(Create(graph_label))
        self.play(Create(graph), run_time=2)
        self.play(Create(m1), Create(m2))
        self.wait(3)

        ### Slide to the right, find center of available space
        self.play(self.camera.frame.animate.move_to([8.7, 0, 0]))
        center = (ax.get_right() + self.camera.frame.get_right()) / 2
        self.wait(2)

        ### Find value of f at 0
        f_values = [
            r"f \left( x \right)",
            r"f \left( x \right) = x + 1",
            r"f \left( 0 \right) = 0 + 1",
            r"f \left( 0 \right) = 1",
        ]
        f_value = MathTex(f_values[0]).move_to(center + UP * 3)
        self.play(Write(f_value))
        for i in f_values[1:]:
            self.wait(1)
            self.play(Transform(f_value, MathTex(i).move_to(center + UP * 3)))
        f_value_box = SurroundingRectangle(f_value, color=BLUE, buff=0.3)
        self.play(Write(f_value_box))
        self.wait(2)

        ### Find value of limits
        lim1s = [
            r"\lim_{x \to 0^{-}} f \left( x \right) \stackrel{?}{=} \lim_{x \to 0^{+}} f \left( x \right)",
            r"\lim_{x \to 0^{-}} f \left( x \right) \neq \lim_{x \to 0^{+}} f \left( x \right)",
        ]
        lim1 = MathTex(lim1s[0]).move_to(center)

        self.play(Write(lim1))

        self.wait(3)

        self.play(Restore(self.camera.frame))
        self.wait(2)

        ### Two points on function moving towards each other
        p1 = PointOnGraph(ax, f1, "A", -1.5)
        p2 = PointOnGraph(ax, f2, "B", 1.5)

        self.play(Create(p1), Create(p2))
        self.wait(1)
        self.play(p1.move_point(0), p2.move_point(0), run_time=4)
        self.wait(2)

        ### Slide back to side, show limits algebraically
        self.play(self.camera.frame.animate.move_to([8.7, 0, 0]))
        self.play(lim1.animate.move_to(center + DOWN * 3))
        self.wait(1)
        lim2s = [
            (
                r"\lim_{x \to 0^{-}} f \left( x \right)",
                r"\lim_{x \to 0^{+}} f \left( x \right)",
            ),
            (r"\lim_{x \to 0^{-}} x", r"\lim_{x \to 0^{+}} x + 1"),
            (r"\lim_{x \to 0^{-}} x = 0", r"\lim_{x \to 0^{+}} x + 1 = 1"),
        ]
        lim2_a = MathTex(lim2s[0][0]).move_to(center + UP)
        lim2_b = MathTex(lim2s[0][1]).move_to(center + DOWN)
        self.play(GrowFromCenter(lim2_a), GrowFromCenter(lim2_b))
        for lim in lim2s:
            (lim_a, lim_b) = lim
            self.wait(2)
            self.play(
                Transform(lim2_a, MathTex(lim_a).move_to(center + UP)),
                Transform(lim2_b, MathTex(lim_b).move_to(center + DOWN)),
            )
        self.wait(3)
        self.play(ShrinkToCenter(lim2_a), ShrinkToCenter(lim2_b))
        self.play(lim1.animate.move_to(center))
        self.play(Transform(lim1, MathTex(lim1s[1]).move_to(center)))

        self.wait(2)

        self.play(Unwrite(lim1), Unwrite(f_value_box))
        self.play(f_value.animate.move_to(center))
        self.play(
            Transform(
                f_value,
                MathTex(
                    r"f \left( 0 \right) \neq \lim_{x \to 0} f \left( 0 \right)"
                ).move_to(center),
            )
        )

        self.wait(2)
        self.play(
            Unwrite(f_value),
            Unwrite(graph_label),
            Unwrite(ax),
            Unwrite(labels),
            Unwrite(graph),
        )
        self.wait()


class Derivasjon(MovingCameraScene):
    def construct(self):
        self.camera: MovingCamera
        self.camera.frame.save_state()

        ### Draw function
        f = lambda x: 1 / 2 * np.power(x, 3) - 3 * x

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

        ### Draw points and secant line
        # https://docs.manim.community/en/stable/examples.html#argminexample
        dot1 = PointOnGraph(ax, f, "A", 3, direction=DOWN * 0.6 + RIGHT * 2.7)
        dot2 = PointOnGraph(ax, f, "B", 2, direction=DOWN * 0.6 + RIGHT * 2.7)

        l1 = (
            Line(dot1.get_dot_center(), dot2.get_dot_center())
            .set_length(15)
            .set_color(RED)
        )
        l1.add_updater(
            lambda x: x.become(
                Line(dot1.get_dot_center(), dot2.get_dot_center())
                .set_length(15)
                .set_color(RED)
            )
        )

        dots = VGroup(dot1, dot2)

        self.play(Create(ax))
        self.play(Create(labels))
        self.wait()
        self.play(Create(graph), run_time=2)
        self.play(Create(graph_label))
        self.wait(0.4)
        self.play(Create(dots))
        self.wait(0.5)
        self.play(Create(l1), run_time=1)

        ### Write the definition of the derivative
        # bad code, output also looks bad
        r"""
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
        """

        self.wait(0.5)

        self.play(dot1.move_point(2.5), run_time=2)
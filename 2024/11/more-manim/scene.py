from manim import *


class Scene(Scene):
    def construct(self):
        equations = [
            r"ax^2+bx+c}=0",
            r"x^2+\frac{b}{a}x+\frac{c}{a}=0",
            r"x^2+\frac{b}{a}x=-\frac{c}{a}",
            r"x^2+\frac{b}{a}x+\left(\frac{b}{2a}\right)^{2}=-\frac{c}{a}+\left(\frac{b}{2a}\right)^{2}",
            r"\left(x+\frac{b}{2a}\right)^{2}=-\frac{c}{a}+\left(\frac{b}{2a}\right)^{2}",
            r"x+\frac{b}{2a}=\pm\sqrt{-\frac{c}{a}+\left(\frac{b}{2a}\right)^{2}}",
            r"x=-\frac{b}{2a}\pm\sqrt{-\frac{c}{a}+\left(\frac{b}{2a}\right)^{2}}",
            r"x=\left(-\frac{b}{2a}\pm\sqrt{-\frac{c}{a}+\left(\frac{b}{2a}\right)^{2}}\right) \times \frac{2a}{2a}",
            r"x=\frac{-b\pm\sqrt{-\frac{c}{a} \times \left(2a\right)^{2} + \left(\frac{b}{2a}\right)^{2} \times \left(2a\right)^{2}}}{2a}",
            r"x=\frac{-b\pm\sqrt{b^{2}-4ac}}{2a}",
        ]

        equation = MathTex(equations[0])
        self.play(Create(equation))
        for eq in equations[1:]:
            self.wait(2)
            self.play(Transform(equation, MathTex(eq)), run_time=0.8)
        self.wait(2)
        self.play(FadeOut(equation), run_time=1)

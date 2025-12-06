# https://github.com/ManimCommunity/manim/issues/4005
# Have to render with `manim -pql main.py Kontinuitet --disable_caching`

from manim import *
from math import radians

from lib import (
    play_audio,
)


class Scene(MovingCameraScene):
    def construct(self):
        # Save the camera state for moving it
        # https://docs.manim.community/en/stable/examples.html#special-camera-settings
        self.camera: MovingCamera  # LSP doesn't understand that
        self.camera.frame.save_state()

        # a0 Her er mitt svar på videoinnleveringen til heldagsprøven i fysikk. Jeg skal finne ut om reklamen for luftgevær presenterer realistiske verdier og at påstanden deres stemmer.
        with play_audio(self, "assets/a0.wav"):
            t = Text("Olai - Fysikk videoinnlevering")
            t.scale(1.5)
            t.set_color_by_gradient(BLUE, TEAL)
            self.play(Write(t))
            self.wait(0.5)
            self.play(t.animate.to_edge(UP, buff=0.5).scale(0.7))
        self.wait(0.5)

        eq1s = [
            r"v_0 = 1000 \,\mathrm{ft/s}",
            r"v_0 = 1000 \,\mathrm{ft/s} \cdot 304.8\,\mathrm{cm}",
            r"v_0 = 1000 \,\mathrm{ft/s} \cdot 3.048\,\mathrm{m}",
            r"v_0 = 304.8 \,\mathrm{m/s}",
        ]
        mkeq1 = lambda x: MathTex(x).scale(2)
        eq1 = mkeq1(eq1s[0])
        # a1 Jeg får oppgitt verdien for hastighet i fot per sekund, så jeg bruker forholdstallet på 30.48 centimeter for å konvertere som vist her.
        with play_audio(self, "assets/a1.wav"):
            self.play(Write(eq1))
            self.wait(1)
            self.play(Transform(eq1, mkeq1(eq1s[1])))
            self.wait(1)
            self.play(Transform(eq1, mkeq1(eq1s[2])))
            self.wait(1)
            self.play(Transform(eq1, mkeq1(eq1s[3])))
            self.wait(1)
            self.play(eq1.animate.scale(0.5).move_to(UP*1.5 + LEFT*5))
            self.wait(1)

        # a2 Jeg skal gjøre forsøket ved å henge en plastilinakule i en pendel og skyte denne med luftgeværet.
        with play_audio(self, "assets/a2.wav"):
            pass
        self.wait(1)

        # a3 Da bruker jeg disse verdiene for massen til henholdsvis kulen på et halvt gram og plastilinakulen på 200 gram.
        with play_audio(self, "assets/a3.wav"):
            eq2s = [
                r"m = 0.50 \,\mathrm{g}",
                r"m = 0.0005 \,\mathrm{kg}",
            ]
            mkeq2 = lambda x: MathTex(x).scale(2)
            eq2 = mkeq2(eq2s[0])
            self.play(Write(eq2))
            self.wait(1)
            self.play(Transform(eq2, mkeq2(eq2s[1])))
            self.wait(1)
            self.play(eq2.animate.scale(0.5).move_to(UP * 0.5 + LEFT * 5))
            self.wait(1)

            eq3s = [
                r"m = 200 \,\mathrm{g}",
                r"m = 0.2 \,\mathrm{kg}",
            ]
            mkeq3 = lambda x: MathTex(x).scale(2)
            eq3 = mkeq3(eq3s[0])
            self.play(Write(eq3))
            self.wait(1)
            self.play(Transform(eq3, mkeq3(eq3s[1])))
            self.wait(1)
            self.play(eq3.animate.scale(0.5).move_to(DOWN * 0.5 + LEFT * 5))
        self.wait(1)

        eq4s = [
            r"p_0 = m \cdot v",
            r"p_0 = 0.0005 \cdot 304.8",
            r"p_0 = 0.15204",
        ]
        mkeq4 = lambda x: MathTex(x).scale(1.5)
        eq4 = mkeq4(eq4s[0])
        # a4 For å finne ut hvilken fart den har etter støtet, må jeg først finne bevegelsesmengden kulen har rett etter den blir skutt ut.
        with play_audio(self, "assets/a4.wav"):
            self.play(Write(eq4))
            self.wait(1)
            self.play(Transform(eq4, mkeq4(eq4s[1])))
            self.wait(1)
            self.play(Transform(eq4, mkeq4(eq4s[2])))
            self.wait(1)
            self.play(eq4.animate.scale(2/3).move_to(UP * 1.5 + RIGHT * 4))
        self.wait(1)

        eq5s = [
            r"p_1 = m \cdot v",
            r"p_1 = mv + mV",
            r"p_1 = \left( m + M \right)v",
            r"p_1 = \left( 0.0005 + 0.2 \right)v",
            r"p_1 = 0.2005 v",
        ]
        mkeq5 = lambda x: MathTex(x).scale(1.5)
        eq5 = mkeq5(eq5s[0])
        # a5 Og så finner jeg bevegelsesmengden etter støtet.
        with play_audio(self, "assets/a5.wav"):
            self.play(Write(eq5))
        self.wait(1)
        # a6 Dette er et fullstendig uelastisk støt, altså blir legemene sittende sammen med en felles masse og fart. Så her er utregningen for bevegelsesmengden etter støtet.
        with play_audio(self, "assets/a6.wav"):
            self.play(Transform(eq5, mkeq5(eq5s[1])))
            self.wait(1)
            self.play(Transform(eq5, mkeq5(eq5s[2])))
            self.wait(1)
            self.play(Transform(eq5, mkeq5(eq5s[3])))
            self.wait(1)
            self.play(Transform(eq5, mkeq5(eq5s[4])))
            self.wait(1)
            self.play(eq5.animate.scale(2/3).move_to(UP * 0.5 + RIGHT * 4))
        self.wait(1)

        eq6s = [
            r"p_1 = p_0",
            r"0.2005v = 0.15204",
            r"v = \frac{0.15204}{0.2005}",
            r"v = 0.76 \,\mathrm{m/s}"
        ]
        mkeq6 = lambda x: MathTex(x).scale(1.5)
        eq6 = mkeq6(eq6s[0])
        # a7 Nå bruker jeg bevaringsloven for bevegelesemengde, som sier at bevegelsesmengden alltid er bevart. Jeg ser vekk fra all luftmotstand og friksjon.
        with play_audio(self, "assets/a7.wav"):
            self.play(Write(eq6))
            self.wait(1)
            self.play(Transform(eq6, mkeq6(eq6s[1])))
            self.wait(1)
            self.play(Transform(eq6, mkeq6(eq6s[2])))
        self.wait(1)
        # a8 Fra dette får jeg at farten til kulen etter støtet vil være 0.76 meter per sekund.
        with play_audio(self, "assets/a8.wav"):
            self.play(Transform(eq6, mkeq6(eq6s[3])))
            self.wait(1)
            self.play(Unwrite(eq4), Unwrite(eq5))
            self.wait(1)
            self.play(eq6.animate.scale(2/3).move_to(UP * 1.5 + RIGHT * 4))
        self.wait(1)

        origin = ORIGIN + UP * 1.5
        length = 3

        vertical_line = DashedLine(origin, origin + length * DOWN, color=GRAY)
        pendulum_line = Line(origin, origin + length * DOWN, color=BLUE, stroke_width=4)
        bob = Circle(radius=0.2, color=BLUE, fill_opacity=1).move_to(origin + length * DOWN)

        arrow = Arrow(
            start=bob.get_bottom() + DOWN * 0.2 + LEFT * 0.5,
            end=bob.get_bottom() + DOWN * 0.2 + RIGHT * 0.5,
            buff=0,
            color=BLUE,
        )

        vector_label = MathTex(r"\vec{v}").next_to(arrow.get_center(), DOWN * 0.5)

        # a9 Nå kan jeg bruke disse verdiene jeg har funnet til å finne ut hvor stort utslaget i pendelen blir. Jeg starter med å finne den kinetiske energien.
        with play_audio(self, "assets/a9.wav"):
            self.play(Create(vertical_line))
            self.play(Create(pendulum_line))
            self.play(Create(bob))
            self.wait(1)
            self.play(Write(arrow))
            self.play(Write(vector_label))

        eq7s = [
            r"E_K0 = \frac{1}{2} m v^2",
            r"E_K0 = \frac{1}{2} \cdot 0.2005 \cdot 0.76^2",
            r"E_K0 = 0.0579 \,\mathrm{J}",
        ]
        mkeq7 = lambda x: MathTex(x).scale(1.5).move_to(RIGHT * 3.5)
        eq7 = mkeq7(eq7s[0])
        # a10 Den kinetiske energien får jeg med å bruke den nye massen og farten jeg fant isted. Nå får jeg at den kinetiske energien idet støtet skjer er 0.0579 joule.
        with play_audio(self, "assets/a10.wav"):
            self.play(Write(eq7))
            self.wait(1)
            self.play(Transform(eq7, mkeq7(eq7s[1])))
            self.wait(1)
            self.play(Transform(eq7, mkeq7(eq7s[2])))
            self.wait(1)
            self.play(eq7.animate.scale(2/3).move_to(DOWN * 2 + RIGHT * 4))
        self.wait(1)

        # a11 Videre kan jeg bruke bevaringsloven for mekanisk energi der jeg igjen ser vekk fra luftmotstand og friksjon. Den sier at når jeg velger nullpunktet for høyden som det laveste punktet, altså der støtet skjer, er den kinetiske energien på bunnen det samme som den potensielle energien på toppen. Dette kan jeg da bruke til å finne høyden.
        with play_audio(self, "assets/a11.wav"):
            eq8s = [
                r"E_P = mgh",
                r"E_P = 0.2005 \cdot 9.81 \cdot h",
                r"E_P = 1.966905 \cdot h",
            ]
            mkeq8 = lambda x: MathTex(x).scale(1.5).move_to(RIGHT * 3.5)
            eq8 = mkeq8(eq8s[0])
            self.play(Write(eq8))
            self.wait(1)
            self.play(Transform(eq8, mkeq8(eq8s[1])))
            self.wait(1)
            self.play(Transform(eq8, mkeq8(eq8s[2])))
            self.wait(1)
            self.play(eq8.animate.scale(2/3).move_to(DOWN * 1 + RIGHT * 4))
            self.wait(1)

            eq9s = [
                r"E_P = E_K0",
                r"1.966905 \cdot h = 0.0579",
                r"h = 0.029437"
            ]
            mkeq9 = lambda x: MathTex(x).scale(1.5).move_to(RIGHT * 3.5)
            eq9 = mkeq9(eq9s[0])
            self.play(Write(eq9))
            self.wait(1)
            self.play(Transform(eq9, mkeq9(eq9s[1])))
            self.wait(1)
            self.play(Transform(eq9, mkeq9(eq9s[2])))
            self.wait(1)
            self.play(Unwrite(eq7), Unwrite(eq8))
            self.wait(1)
            self.play(eq9.animate.scale(2/3).move_to(DOWN * 1.5 + RIGHT * 4))
        self.wait(1)

        self.play(Unwrite(arrow), Unwrite(vector_label))
        self.wait(1)

        # a12 Etter støtet vil da kulen flytte seg oppover sånn som dette her. Nå skal jeg finne vinkelen, og for å gjøre dette må jeg velge en lengde for pendelen. Jeg velger en halv meter siden det virker som et bra tall å bruke.
        with play_audio(self, "assets/a12.wav"):
            pendulum_label = MathTex(r"L = 0.5 \,\mathrm{m}").next_to(pendulum_line.get_center(), LEFT * 1.5)
            self.play(Write(pendulum_label))
            self.wait(1)

            angle = 19.75 * DEGREES
            bob_pos = origin + length * np.array([np.sin(angle), -np.cos(angle), 0])
            pendulum_line2 = Line(origin, bob_pos, color=BLUE, stroke_width=4)
            self.play(Uncreate(pendulum_line))
            self.wait(1)
            self.play(Create(pendulum_line2))
            self.play(bob.animate.move_to(bob_pos))
            self.play(pendulum_label.animate.next_to(pendulum_line2.get_center(), RIGHT * 2 + DOWN))
        self.wait(1)

        eq10s = [
            r"H = L-h",
            r"H = 0.5 - 0.029437",
            r"H = 0.470536",
        ]
        # a13 For å finne den hosliggende kateten kan jeg da ta lengden på pendelen og trekke fra høyden til kulen som jeg fikk ved hjelp av energibevaringsloven. Jeg kaller denne stor H.
        with play_audio(self, "assets/a13.wav"):
            mkeq10 = lambda x: MathTex(x).scale(0.8).next_to(vertical_line.get_center(), LEFT * 1.3)
            eq10 = mkeq10(eq10s[0])
            self.play(Write(eq10))
            self.wait(1)
            self.play(Transform(eq10, mkeq10(eq10s[1])))
            self.wait(1)
            self.play(Transform(eq10, mkeq10(eq10s[2])))
        self.wait(1)

        eq11s = [
            r"\cos{\alpha} = \frac{H}{L}",
            r"\cos{\alpha} = \frac{0.470563}{0.5}",
            r"\cos{\alpha} = 0.941126",
            r"\alpha = 19.7584^\circ",
            r"\alpha = 20^\circ"
        ]
        mkeq11 = lambda x: MathTex(x).move_to(RIGHT * 3.5 + UP * 0.4)
        eq11 = mkeq11(eq11s[0])
        # a14 Til slutt kan jeg da bruke at cosinus av vinkelen alpha er lik den hosliggende kateten delt på hypotenusen. Jeg løser den trigonometriske likningen ved hjelp av digitale hjelpemidler og får ut at vinkel alpha er lik 19.7584 grader, eller 20 grader med to signifikante sifre.
        with play_audio(self, "assets/a14.wav"):
            self.play(Write(eq11))
            self.wait(1)
            self.play(Transform(eq11, mkeq11(eq11s[1])))
            self.wait(1)
            self.play(Transform(eq11, mkeq11(eq11s[2])))
            self.wait(1)
            self.play(Transform(eq11, mkeq11(eq11s[3])))
            self.wait(1)
            self.play(Transform(eq11, mkeq11(eq11s[4])))
            self.wait(1)
            self.play(eq11.animate.move_to(RIGHT * 4))
        self.wait(1)
        # a15 Nå har jeg funnet vinkelen, og jeg synes at dette virker som et realistisk utslag å få når man har skutt luftgeværet, så da vil jeg si at påstanden er sann og at reklamen holder det de lover.
        with play_audio(self, "assets/a15.wav"):
            box = SurroundingRectangle(eq11, color=BLUE, buff=0.1)
            self.play(Create(box))
        self.wait(1)

        # a16 Takk for meg
        with play_audio(self, "assets/a16.wav"):
            pass
        self.wait(1)

        all_objects = VGroup(t, eq1, eq2, eq3, eq6, eq9, eq10, eq11, vertical_line, pendulum_line2, pendulum_label, bob, box)
        self.play(Uncreate(all_objects))
        self.wait(1)

        # self.wait()
        # with play_audio(self, "assets/a01.wav"):
        #     pass
        # self.wait()

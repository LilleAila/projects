import sys
import tkinter as tk
from tkinter import font


class App:
    def __init__(self):
        self.inputs_per_row = 4

        self.root = tk.Tk()
        self.root.title("Temperaturstyring")
        self.root.geometry("300x200")
        self.default_font = font.nametofont("TkDefaultFont")
        self.default_font.configure(family="Arial", size=15)

        # TODO: make the frame properly use the full available size of the window, and resize when resized
        self.frame = tk.Frame(self.root)
        self.frame.place(relx=0.5, rely=0.5, anchor=tk.CENTER)

        # Make button expand: https://stackoverflow.com/a/53079377
        tk.Button(
            self.frame,
            text="Av/På",
        ).grid(row=0, column=0, rowspan=4, padx=4, pady=4, sticky=tk.NSEW)

        temp_display = tk.Frame(
            self.frame,
            highlightbackground="gray",
            highlightthickness=1,
        )
        temp_display.grid(row=1, column=1, rowspan=2, columnspan=2, padx=4, pady=4)

        tk.Label(
            temp_display,
            text="Temperatur:",
        ).grid(row=0, column=0, padx=2, pady=2)

        # NOTE: Oppgaven ber om å bruke grid, derfor gjør jeg det slik.
        # hvis jeg skulle laget samme UI selv, hadde jeg puttet hele høyre kolonne
        # i en egen frame, heller enn å abuse `columnspan` og `rowspan` overalt.
        self.temp = 18.5
        self.temp_str = tk.StringVar(self.root, f"{self.temp}°C")
        tk.Label(
            temp_display,
            textvariable=self.temp_str,
        ).grid(row=1, column=0, padx=2, pady=2)

        tk.Button(
            self.frame,
            text="↑",
            command=lambda: self.modify_temp(0.5),
        ).grid(row=0, column=3, padx=2, pady=2, sticky=tk.NSEW)

        tk.Button(
            self.frame,
            text="Sett",
            command=self.commit_temp,
        ).grid(row=1, column=3, rowspan=2, padx=2, pady=2, sticky=tk.NSEW)

        tk.Button(
            self.frame,
            text="↓",
            command=lambda: self.modify_temp(-0.5),
        ).grid(row=3, column=3, padx=2, pady=2, sticky=tk.NSEW)

        self.status = tk.StringVar(self.root, "")
        tk.Label(
            self.frame,
            textvariable=self.status,
            bg="white",
            fg="black",
            borderwidth=2,
            relief="ridge",
        ).grid(row=4, column=0, columnspan=4, padx=4, pady=4, sticky=tk.NSEW)

    def modify_temp(self, diff):
        self.temp += diff
        self.temp_str.set(f"{self.temp}°C")

    def commit_temp(self):
        self.status.set(f"Temperatur satt til {self.temp_str.get()}")

    def run(self):
        self.root.mainloop()


if __name__ == "__main__" and not sys.flags.interactive:
    app = App()
    app.run()

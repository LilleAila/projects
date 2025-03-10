import sys
import tkinter as tk
from tkinter import font


class App:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Tvers")
        self.root.geometry("300x200")
        self.default_font = font.nametofont("TkDefaultFont")
        self.default_font.configure(family="Arial", size=15)

        self.frame = tk.Frame(self.root)
        self.frame.place(relx=0.5, rely=0.5, anchor=tk.CENTER)

        self.inputs = tk.Frame(self.frame)
        self.inputs.grid(row=0, column=0)

        self.characters = tk.Frame(self.frame, width=300, height=100)
        self.characters.grid(row=1, column=0)
        self.characters.pack_propagate(False)

        [self.make_button(i) for i in list("ABCDEFGHI")]

    def make_button(self, label: str):
        frame = tk.Frame(
            self.characters,
            height=50,
            width=50,
            bg="white",
        )
        frame.pack(side=tk.LEFT, padx=5, pady=5)
        frame.pack_propagate(False)
        tk.Label(
            frame,
            text=label,
            bg="white",
            fg="black",
        ).pack(expand=True)

    def run(self):
        self.root.mainloop()


if __name__ == "__main__" and not sys.flags.interactive:
    app = App()
    app.run()

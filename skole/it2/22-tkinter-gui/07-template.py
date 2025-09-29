import sys
import tkinter as tk
from tkinter import font


class App:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Mal")
        self.root.geometry("300x200")
        self.default_font = font.nametofont("TkDefaultFont")
        self.default_font.configure(family="Arial", size=15)

        self.frame = tk.Frame(self.root)
        self.frame.place(relx=0.5, rely=0.5, anchor=tk.CENTER)

    def run(self):
        self.root.mainloop()


if __name__ == "__main__" and not sys.flags.interactive:
    app = App()
    app.run()

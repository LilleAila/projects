import tkinter as tk
from tkinter import font

root = tk.Tk()
default_font = font.nametofont("TkDefaultFont")
default_font.configure(family="Arial", size=15)
root.title("Radioknapper")

tk.Label(root, text="Velg farge:", bg="darkgrey", width=20, padx=5, pady=5).grid(
    row=0, column=0, padx=5, pady=5
)

colors = ["red", "blue", "green"]
color = tk.IntVar(value=-1)


def choose_color():
    c = color.get()
    if c != -1:
        choice.configure(text=colors[c], fg="white", bg=colors[c])


cs = tk.Frame(root)
cs.grid(row=1, column=0)

for i, c in enumerate(colors):
    tk.Radiobutton(
        cs,
        text=c,
        variable=color,
        value=i,
        command=choose_color,
        width=10,
        anchor="w",
    ).grid(row=i, column=0)

choice = tk.Label(root, text="")
choice.grid(row=2, column=0, padx=5, pady=5, sticky="NSEW")

root.mainloop()

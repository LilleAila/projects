import tkinter as tk
from tkinter import font
from tkinter import ttk

root = tk.Tk()
root.title("Combobox")
default_font = font.nametofont("TkDefaultFont")
default_font.configure(family="Arial", size=15)

frame = ttk.Frame(root)
frame.place(relx=0.5, rely=0.5, anchor="center")

tk.Label(frame, text="Velg valg:").grid(row=0, column=0, padx=5, pady=5)

xs = ["abc", "def", "ghi", "jkl", "mno"]
choice = tk.StringVar()
combobox = ttk.Combobox(
    frame,
    textvariable=choice,
    values=xs,
    state="readonly",
    width=15,
    height=3,
)
combobox.grid(row=0, column=1, padx=5, pady=5)
combobox.current(0)

out = tk.Label(frame, text="")
out.grid(row=1, column=0, columnspan=2, padx=5, pady=5)


def update_text(*_):
    i = combobox.current()
    if i > 0:
        c = xs[i]
        out.configure(text=f"Chose {i}: {c}")


choice.trace_add("write", update_text)

"""
# alt
out = tk.Label(frame, textvariable=choice)
out.grid(row=1, column=0, columnspan=2, padx=5, pady=5)
"""

root.mainloop()

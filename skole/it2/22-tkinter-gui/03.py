import tkinter as tk
from tkinter import font


def mul():
    try:
        ans = float(n1.get()) * float(n2.get())
    except ValueError:
        n1.delete(0, tk.END)
        n2.delete(0, tk.END)
    else:
        res.configure(text=f"{ans:.0f}")


root = tk.Tk()
root.title("Spinbox")

default_font = font.nametofont("TkDefaultFont")
default_font.configure(family="Arial", size=20)

n1 = tk.Spinbox(root, from_=0, to=10, increment=1, width=5, font=("Arial 20"))
n1.grid(row=0, column=0, padx=10, pady=10)

tk.Label(root, text="*").grid(row=0, column=1, padx=10, pady=10)

n2 = tk.Spinbox(root, from_=0, to=10, increment=1, width=5, font=("Arial 20"))
n2.grid(row=0, column=2, padx=10, pady=10)

tk.Button(root, text="=", command=mul, width=5).grid(row=0, column=3, padx=10, pady=10)

res = tk.Label(root, width=5)
res.grid(row=0, column=4, padx=10, pady=10)

root.mainloop()

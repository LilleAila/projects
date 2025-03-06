import tkinter as tk
from tkinter import font
from PIL import Image, ImageTk


root = tk.Tk()

default_font = font.nametofont("TkDefaultFont")
default_font.configure(family="Arial", size=20)

root.title("Inndata/Utdata")

bilde = Image.open("02.jpg")
bilde = bilde.resize((150, 150), Image.Resampling.LANCZOS)
bilde = ImageTk.PhotoImage(bilde)
tk.Label(root, image=bilde).grid(row=0, column=0, padx=5, pady=5)

frame = tk.Frame(root, padx=10, pady=10)
frame.grid(row=0, column=1, padx=5, pady=15)

navn = tk.Entry(frame, width=15, font=("Arial 20"))
navn.insert(0, "Hva heter du? ")
navn.grid(row=0, column=0, padx=5, pady=13)

skriv_ut = lambda: hilsen.configure(text=f"Hallo {navn.get()}!")
tk.Button(frame, text="Les navn", command=skriv_ut, width=10).grid(
    row=0, column=1, padx=5
)

hilsen = tk.Label(frame, width=25, bg="#282828", fg="#ebdbb2", pady=3)
hilsen.grid(row=1, column=0, columnspan=2, pady=13)

root.mainloop()

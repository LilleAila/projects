import tkinter as tk
from tkinter import font

root = tk.Tk()
root.title("Listbox")
default_font = font.nametofont("TkDefaultFont")
default_font.configure(family="Arial", size=15)

frame = tk.Frame(root)
scrollbar = tk.Scrollbar(frame, orient="vertical")
listbox = tk.Listbox(
    frame, height=3, width=10, yscrollcommand=scrollbar.set, selectmode="single"
)

xs = ["abc", "def", "ghi", "jkl", "mno"]
for x in xs:
    listbox.insert("end", x)

frame.pack(pady=25)
scrollbar.configure(command=listbox.yview)
scrollbar.pack(side="right", fill="y")

listbox.pack()

sel = tk.StringVar(value="")
out = tk.Label(root, textvariable=sel)
out.pack(pady=10)


def show_choice(e):
    selection = listbox.curselection()
    if selection:
        sel.set(xs[selection[0]])
    # alt:
    # out.configure(text=xs[listbox.curselection()[0]])


listbox.bind("<<ListboxSelect>>", show_choice)

root.mainloop()

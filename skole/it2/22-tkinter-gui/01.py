import tkinter as tk

root = tk.Tk()
tk.Label(root, text="Hello, World!").pack()

navn = tk.Entry(root)
navn.insert(0, "Hva heter du? ")
navn.pack()

skriv_ut = lambda: hilsen.configure(text=f"Hello {navn.get()}")
tk.Button(root, text="Les navn", command=skriv_ut).pack()

hilsen = tk.Label(root)
hilsen.pack()

root.mainloop()

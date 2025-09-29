import sys
import tkinter as tk
from tkinter import font


class App:
    def __init__(self):
        self.inputs_per_row = 4

        self.root = tk.Tk()
        self.root.title("Tvers")
        self.root.geometry("300x200")
        self.default_font = font.nametofont("TkDefaultFont")
        self.default_font.configure(family="Arial", size=15)

        self.frame = tk.Frame(self.root)
        self.frame.place(relx=0.5, rely=0.5, anchor=tk.CENTER)

        self.inputs = tk.Frame(self.frame)
        self.inputs.grid(row=0, column=0)

        self.characters = tk.Frame(self.frame)
        self.characters.grid(row=1, column=0)
        self.characters.pack_propagate(False)

        self.make_buttons("ABCDEFGH")

    def make_buttons(self, text):
        self.input_boxes = []
        for char in text:
            frame = tk.Frame(
                self.characters,
                height=50,
                width=50,
                bg="white",
            )
            frame.pack_propagate(False)
            label = tk.Label(
                frame,
                text=char,
                bg="white",
                fg="black",
            )
            label.pack(expand=True)

            frame.bind("<ButtonPress-1>", self.on_start)
            frame.bind("<B1-Motion>", self.on_drag)
            frame.bind("<ButtonRelease-1>", self.on_drop)

            label.bind("<ButtonPress-1>", self.on_start)
            label.bind("<B1-Motion>", self.on_drag)
            label.bind("<ButtonRelease-1>", self.on_drop)

            self.input_boxes.append(frame)
        self.move_inputs()

    def move_inputs(self):
        for i, frame in enumerate(self.input_boxes):
            frame.grid(
                row=i // self.inputs_per_row,
                column=i % self.inputs_per_row,
                padx=5,
                pady=5,
            )

    def on_start(self, event):
        widget = event.widget
        if isinstance(widget, tk.Label):
            widget = widget.master
            assert isinstance(widget, tk.Frame)
        self._drag = (event.x, event.y)
        self._drag_start = (widget.winfo_x(), event.widget.winfo_y())
        widget.lift()

    def on_drag(self, event):
        widget = event.widget
        if isinstance(widget, tk.Label):
            widget = widget.master
            assert isinstance(widget, tk.Frame)
        x, y = self._drag
        x = widget.winfo_x() - x + event.x
        y = widget.winfo_y() - y + event.y
        widget.place(x=x, y=y)

    def on_drop(self, event):
        widget = event.widget
        if isinstance(widget, tk.Label):
            widget = widget.master
            assert isinstance(widget, tk.Frame)
        x, y = self._drag_start
        widget.place(x=x, y=y)
        [f.destroy() for f in self.input_boxes]
        self.make_buttons("ABCDEFGH")

    def run(self):
        self.root.mainloop()


if __name__ == "__main__" and not sys.flags.interactive:
    app = App()
    app.run()

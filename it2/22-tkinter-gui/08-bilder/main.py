import os
import sys
import tkinter as tk
from tkinter import font
from PIL import Image, ImageTk


class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Image viewer")
        self.default_font = font.nametofont("TkDefaultFont")
        self.default_font.configure(family="Arial", size=15)
        self.geometry("600x600")
        self.focus_force()

        self.frame = tk.Frame(self)
        self.frame.place(relx=0.5, rely=0.5, anchor=tk.CENTER)

        tk.Label(
            self.frame,
            text="Image Viewer!!!",
            font=("Arial", 20),
        ).grid(row=0, column=0, pady=10)

        self.image_label = tk.Label(self.frame, pady=10)
        self.image_label.grid(row=1, column=0)
        self.images = [
            "images/img0.png",
            "images/img1.png",
            "images/img2.png",
            "images/img3.png",
            "images/img4.png",
            "images/img5.png",
            "images/img6.png",
            "images/img7.png",
            "images/img8.png",
            "images/img9.png",
            "images/img10.png",
            "images/img11.png",
            "images/img12.png",
            "images/img13.png",
            "images/img14.png",
            "images/img15.png",
            "images/img16.png",
            "images/img17.png",
            "images/img18.png",
            "images/img19.png",
            "images/img20.png",
        ]
        self.image_idx = 0
        self.update_image()

        self.controls = tk.Frame(self.frame)
        self.controls.grid(row=2, column=0, pady=10)

        tk.Button(
            self.controls,
            text="Previous Image",
            command=self.previous_image,
        ).grid(row=0, column=0)

        tk.Button(
            self.controls,
            text="Next Image",
            command=self.next_image,
        ).grid(row=0, column=1)

    def set_image(self, image_path, max_width=300, max_height=300):
        path = os.path.join(os.path.dirname(__file__), image_path)
        image = Image.open(path)
        image = image.resize((150, 150), Image.Resampling.LANCZOS)
        w, h = image.size
        scale = min(max_width / w, max_height / h)
        image = image.resize((int(w * scale), int(h * scale)), Image.Resampling.LANCZOS)
        self.image = ImageTk.PhotoImage(image)  # to keep it in memory or something
        self.image_label.configure(image=self.image)

    def previous_image(self):
        self.image_idx = max(0, self.image_idx - 1)
        self.update_image()

    def next_image(self):
        self.image_idx = min(len(self.images) - 1, self.image_idx + 1)
        self.update_image()

    def update_image(self):
        self.set_image(self.images[self.image_idx])

    def run(self):
        self.mainloop()


if __name__ == "__main__" and not sys.flags.interactive:
    app = App()
    app.run()

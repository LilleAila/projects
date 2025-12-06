# import logging
# import edifice as ed
# from PySide6 import QtCore, QtGui
#
# logging.getLogger("Edifice").setLevel(logging.INFO)
#
#
# @ed.component
# def Main(self):
#     with ed.Window().render():
#         ed.Label("Hello, World!")
#
#
# if __name__ == "__main__":
#     ed.App(Main()).start()
#
#
from edifice import App, Label, TextInput, View, Window, component


@component
def MyApp(self):
    with Window():  # Top of every App must be a Window
        with View(layout="row"):
            Label("Measurement in meters:")
            TextInput("")
            Label("Measurement in feet:")


if __name__ == "__main__":
    App(MyApp()).start()

# ruff: noqa: F405

import threading
import time
import atexit
import mido
from pynput.keyboard import Controller as KbController, Key
from pynput.mouse import Controller as MouseController, Button
from Quartz import (
    CGMainDisplayID,
    CGDisplayPixelsHigh,
    CGDisplayPixelsWide,
    CGEventCreate,
    CGEventGetLocation,
    CGEventCreateMouseEvent,
    CGEventPost,
    CGEventSetIntegerValueField,
    kCGEventMouseMoved,
    kCGHIDEventTap,
    kCGMouseButtonLeft,
    kCGMouseEventDeltaX,
    kCGMouseEventDeltaY,
)

from midi_notes import * # noqa: F403

# Print available devices
# print(mido.get_input_names())

def move_mouse_relative(dx, dy, locked):
    display_id = CGMainDisplayID()
    screen_width = CGDisplayPixelsWide(display_id)
    screen_height = CGDisplayPixelsHigh(display_id)
    center_x = screen_width / 2
    center_y = screen_height / 2
    loc = CGEventGetLocation(CGEventCreate(None))
    new_x = loc.x + dx
    new_y = loc.y + dy

    center_x = screen_width / 2
    center_y = screen_height / 2

    event = CGEventCreateMouseEvent(
        None,
        kCGEventMouseMoved,
        (center_x, center_y) if locked else (new_x, new_y),
        kCGMouseButtonLeft
    )

    CGEventSetIntegerValueField(event, kCGMouseEventDeltaX, int(dx))
    CGEventSetIntegerValueField(event, kCGMouseEventDeltaY, int(dy))
    CGEventPost(kCGHIDEventTap, event)

def move_mouse_relative2(dx, dy):
    # Pass a static dummy point (0, 0) because Minecraft ignores it anyway,
    # and let the delta fields do 100% of the work.
    event = CGEventCreateMouseEvent(
        None,
        kCGEventMouseMoved,
        (0, 0),
        kCGMouseButtonLeft
    )

    CGEventSetIntegerValueField(event, kCGMouseEventDeltaX, int(dx))
    CGEventSetIntegerValueField(event, kCGMouseEventDeltaY, int(dy))
    CGEventPost(kCGHIDEventTap, event)

keyboard = KbController()
mouse = MouseController()

pressed_keys = set()
pressed_mouse = set()
def release_all():
    if pressed_keys:
        for key in pressed_keys:
            try:
                keyboard.release(key)
            except Exception:
                pass
        pressed_keys.clear()
    if pressed_mouse:
        for btn in pressed_mouse:
            try:
                mouse.release(btn)
            except Exception:
                pass
        pressed_mouse.clear()
atexit.register(release_all)

# TODO map midi events properly
# keyboard.press("a")
# keyboard.release("a")

# Mouse options
INPUT_NAME = "USB-MIDI" # Kawai VPC1
MOUSE_NOTES = {
    C5: [-1, 0],
    E5: [0, 1],
    F5: [0, -1],
    A5: [1, 0],
}
SENSITIVITY = 0.2
POLL_RATE = 0.01
active_mouse = {}

# Keyboard mappings
KEY_MAPPINGS = {
    # Vim-style movement
    C3: "a",
    E3: "s",
    F3: "w",
    A3: "d",
    # Hotbar slots
    A1: "1",
    B1: "2",
    C2: "3",
    D2: "4",
    E2: "5",
    F2: "6",
    G2: "7",
    A2: "8",
    B2: "9",
    # Misc
    D3: "q",
    G3: "e",
    G5: " ",
    AB5: Key.shift,
    B3: Key.ctrl,
    D6: Key.esc,
}

MOUSE_BUTTONS = {
    PEDAL_LEFT: Button.left,
    PEDAL_MIDDLE: Button.middle,
    PEDAL_RIGHT: Button.right,
}

mouse_locked = False

def midi_listener():
    with mido.open_input(INPUT_NAME) as port:
        print(f"Listening to MIDI messages on {port.name}")

        for msg in port:
            if msg.type == "note_on" or msg.type == "note_off":
                on = msg.type == "note_on"

                if msg.note in MOUSE_NOTES:
                    if on and msg.velocity > 0:
                        active_mouse[msg.note] = msg.velocity
                    else:
                        try:
                            del active_mouse[msg.note]
                        except KeyError:
                            pass
                elif msg.note in KEY_MAPPINGS:
                    key = KEY_MAPPINGS[msg.note]
                    if on:
                        if key not in pressed_keys:
                            pressed_keys.add(key)
                            keyboard.press(key)
                    else:
                        try:
                            pressed_keys.remove(key)
                        except KeyError:
                            pass
                        keyboard.release(key)
                elif msg.note == C6 and on:
                    global mouse_locked
                    mouse_locked = not mouse_locked
            elif msg.type == "control_change":
                if msg.control in MOUSE_BUTTONS:
                    btn = MOUSE_BUTTONS[msg.control]
                    if msg.value > 0:
                        if btn not in pressed_mouse:
                            pressed_mouse.add(btn)
                            mouse.press(btn)
                    else:
                        try:
                            pressed_mouse.remove(btn)
                        except KeyError:
                            pass
                        mouse.release(btn)

def mouse_loop():
    while True:
        dx = 0
        dy = 0

        for note, velocity in active_mouse.items():
            dx_, dy_ = MOUSE_NOTES[note]

            dx += dx_ * velocity * SENSITIVITY
            dy += dy_ * velocity * SENSITIVITY

        if dx != 0 or dy != 0:
            move_mouse_relative(int(dx), int(dy), mouse_locked)
            # mouse.move(int(dx), int(dy))

        time.sleep(POLL_RATE)

if __name__ == "__main__":
    midi_listener_thread = threading.Thread(target=midi_listener, daemon=True)
    midi_listener_thread.start()

    mouse_loop_thread = threading.Thread(target=mouse_loop, daemon=True)
    mouse_loop_thread.start()

    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print("Stopped")

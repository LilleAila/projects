from manim import *
from pydub import AudioSegment
from contextlib import contextmanager


@contextmanager
def play_audio(scene: Scene, audio_file: str):
    """Play audio while animation is playing inside with"""
    audio = AudioSegment.from_file(audio_file)
    audio_duration = len(audio) / 1000.0
    scene.add_sound(audio_file)
    start_time = scene.renderer.time
    yield audio_duration  # Executes code inside the "with"
    delta_time = scene.renderer.time - start_time
    remaining_time = max(0, audio_duration - delta_time)
    if remaining_time > 0:
        scene.wait(remaining_time)

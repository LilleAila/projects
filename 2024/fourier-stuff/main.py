import numpy as np
import librosa
import mido
from mido import Message, MidiFile, MidiTrack

# Function to map frequency to the nearest piano key
def frequency_to_midi(frequency):
    if frequency == 0:
        return None
    midi_note = 69 + 12 * np.log2(frequency / 440.0)
    return round(midi_note)

# Function to normalize amplitude to MIDI velocity (1 to 127)
def amplitude_to_velocity(amplitude, global_max_amplitude):
    return int((amplitude / global_max_amplitude) * 126) + 1

# Function to write MIDI file
def write_midi(notes_with_velocity, segment_duration, output_midi="output.mid"):
    midi = MidiFile()
    track = MidiTrack()
    midi.tracks.append(track)

    # Time per segment in MIDI ticks (adjustable)
    ticks_per_segment = int(segment_duration * 1000)

    for segment in notes_with_velocity:
        # Add "note_on" for all notes in the segment
        for note, velocity in segment:
            if note and 21 <= note <= 108:  # Valid piano key range
                track.append(Message('note_on', note=note, velocity=velocity, time=0))

        # Add "note_off" after the duration
        for note, velocity in segment:
            if note and 21 <= note <= 108:
                track.append(Message('note_off', note=note, velocity=velocity, time=ticks_per_segment))

    # Save the MIDI file
    midi.save(output_midi)
    print(f"MIDI file saved as {output_midi}")

# Main Function
def audio_to_piano_notes_fft(audio_file, output_midi="output.mid", segment_duration=0.1):
    # Load the audio file
    y, sr = librosa.load(audio_file, sr=None)

    # Calculate the number of samples per segment
    samples_per_segment = int(segment_duration * sr)

    # Compute global maximum amplitude across all segments
    global_max_amplitude = 0
    for start in range(0, len(y), samples_per_segment):
        segment = y[start:start + samples_per_segment]
        if len(segment) < samples_per_segment:
            break  # Skip incomplete segments at the end

        # Perform FFT
        fft_result = np.fft.fft(segment)
        magnitudes = np.abs(fft_result)
        global_max_amplitude = max(global_max_amplitude, np.max(magnitudes))

    if global_max_amplitude == 0:
        raise ValueError("Audio has no significant amplitude (silence).")

    # List to store notes with velocity
    notes_with_velocity = []

    # Process each segment and normalize with global maximum
    for start in range(0, len(y), samples_per_segment):
        segment = y[start:start + samples_per_segment]
        if len(segment) < samples_per_segment:
            break  # Skip incomplete segments at the end

        # Perform FFT
        fft_result = np.fft.fft(segment)
        freqs = np.fft.fftfreq(len(segment), d=1/sr)

        # Get magnitude of frequencies and filter out negative frequencies
        magnitudes = np.abs(fft_result)
        positive_freq_indices = np.where(freqs > 0)
        freqs = freqs[positive_freq_indices]
        magnitudes = magnitudes[positive_freq_indices]

        # Find top 5 dominant frequencies and their amplitudes
        dominant_indices = magnitudes.argsort()[-5:][::-1]
        dominant_freqs = freqs[dominant_indices]
        dominant_magnitudes = magnitudes[dominant_indices]

        # Map frequencies to MIDI notes and calculate velocity
        segment_notes = [
            (frequency_to_midi(freq), amplitude_to_velocity(amp, global_max_amplitude))
            for freq, amp in zip(dominant_freqs, magnitudes)
            if freq > 0
        ]
        notes_with_velocity.append(segment_notes)

    # Write the MIDI file
    write_midi(notes_with_velocity, segment_duration, output_midi)

# Example usage
audio_file = "test.wav"  # Replace with your audio file
audio_to_piano_notes_fft(audio_file, output_midi="output_with_velocity_global.mid")

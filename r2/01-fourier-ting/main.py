import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import find_peaks

Fs = 1000
T = 1 / Fs
t = np.arange(0, 1, T)

waves = [
    A * np.sin(2 * np.pi * f * t)
    for (f, A) in [
        (50, 1.0),
        (120, 0.5),
        (300, 0.3)
    ]
]

wave = sum(waves)

# Input wave
plt.subplot(3, 1, 1)
plt.plot(t, wave)
plt.title("Composite wave")
plt.grid(True)

# FFT
N = len(wave)
wave_fft = np.fft.fft(wave)
freq = np.fft.fftfreq(N, T)
positive_freqs = freq[:N//2]
fft_magnitude = np.abs(wave_fft[:N//2]) * 2 / N

plt.subplot(3, 1, 2)
plt.stem(positive_freqs, fft_magnitude)
plt.title("Frequency Spectrum (FFT)")

# Reconstruction
peaks, _ = find_peaks(fft_magnitude, height=0.1)
peak_freqs = positive_freqs[peaks]

reconstructed_waves = []
for pf in peak_freqs:
    mask = np.zeros(N, dtype=complex)
    idx = np.argmin(np.abs(freq - pf))
    mask[idx] = wave_fft[idx]
    mask[-idx] = wave_fft[-idx]
    reconstructed_wave = np.fft.ifft(mask).real
    reconstructed_waves.append(reconstructed_wave)

plt.subplot(3, 1, 3)
for i, rw in enumerate(reconstructed_waves):
    plt.plot(t, rw, label=f"{peak_freqs[i]:.1f} Hz")
plt.title("Reconstructed harmonics")
plt.legend()

plt.show()

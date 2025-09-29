"""
Alternativ måte å bruke subplots, "globalt"
"""

import matplotlib.pyplot as plt
import numpy as np

xs = np.linspace(0, 20, 50)

plt.subplot(1, 2, 1)
ys = 0.5 * xs**2
plt.plot(xs, ys)
plt.grid()

plt.subplot(1, 2, 2)
ys = -0.3 * xs**3
plt.plot(xs, ys)
plt.grid()

plt.show()

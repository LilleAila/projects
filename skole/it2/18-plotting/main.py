import matplotlib.pyplot as plt
import numpy as np

# https://matplotlib.org/stable/api/axes_api.html
fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)

# https://matplotlib.org/stable/api/figure_api.html
fig.set_facecolor("#DDDDDD")
fig.set_linewidth(10)
fig.set_edgecolor("#282828")

xs = [0, 1, 2, 3, 4]
ys = [0, 1, 4, 9, 16]
ax1.set_title("plt.plot")
ax1.plot(xs, ys)
ax2.set_title("plt.scatter")
ax2.scatter(xs, ys)

f = lambda x: x**2

xs = list(range(10))
ys = [f(x) for x in xs]
ax3.set_title("punkter som liste")
ax3.plot(xs, ys)

xs = np.linspace(0, 10, 11)
ys = f(xs)
ax4.set_title("punkter som array")
ax4.plot(xs, ys)

ax4.set_xlabel("$x$")
ax4.set_ylabel("$y$")
ax4.axhline(0, color="red")
ax4.axvline(0, color="red")
ax4.axhline(25, 0.2, 0.8, color="green", linestyle="dashed")
ax4.grid()
ax4.set_xlim(-1, 11)
ax4.set_ylim(-10, 110)
ax4.set_facecolor("#282828")

plt.savefig("result.png", dpi=300)
plt.show()

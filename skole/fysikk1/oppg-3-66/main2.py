#!/usr/bin/env python3

from matplotlib import pyplot as plt
import numpy as np

def stoppelengde(m):
    g = 9.81 # m/s^2
    v = 30 / 3.6 # m/s
    mu = 0.08 # friction
    k = 0.3 # air resistance

    G = g * m
    N = G
    R = mu * N

    dt = 0.01
    t = 0
    s = 0

    while v > 0:
        L = k * v ** 2
        F = - (R + L)
        a = F / m
        v += a * dt
        s += v * dt
        t += dt

    return s

stoppelengde_vec = np.vectorize(stoppelengde)

xs = np.linspace(1, 1000, 1000)
ys = stoppelengde_vec(xs)

plt.title("Stoppelengde")
plt.xlabel("masse (kg)")
plt.ylabel("strekning (m)")
plt.plot(xs, ys)
plt.show()

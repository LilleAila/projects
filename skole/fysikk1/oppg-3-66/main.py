#!/usr/bin/env python3

g = 9.81 # m/s^2
v = 30 / 3.6 # m/s
m = 100 # kg
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

print(t, s)

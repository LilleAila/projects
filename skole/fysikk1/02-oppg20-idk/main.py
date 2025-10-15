m = 0.0055 # kg
g = 9.81 # m/s^2
k = 0.47

s = 0 # m
v = 0
t = 0
dt = 0.001

while s < 0.2:
    G = m * g
    L = k * v
    a = (G - L) / m
    v = v + a * dt
    s = s + v * dt
    t = t + dt

print(t)

import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("vanntemperatur-kaldtvann.csv", sep=";", comment="#")
time = df["Tid"].tolist()
temp = df["Temperatur"].tolist()

dx = 1

k = -0.015
T = 20

x, y = time[0], temp[0]

xs, ys = [], []

while x <= time[-1]:
    xs.append(x)
    ys.append(y)

    x += dx
    y += dx * k * (y - T)

plt.scatter(time, temp, label="Datapunkter")
plt.plot(xs, ys, label="Modell")
plt.xlabel("Tid")
plt.ylabel("Temperatur")
plt.legend()
plt.title("Euler metoden modell vann")
plt.show()

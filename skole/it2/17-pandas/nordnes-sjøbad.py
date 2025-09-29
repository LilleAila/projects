import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_csv("nordnes-sjøbad.csv")
df[df == "NaN"] = np.nan
df[df == "-"] = np.nan
df["temperatur"] = pd.to_numeric(df["temperatur"])
df["lufttemperatur"] = pd.to_numeric(df["lufttemperatur"])
df["tid"] = pd.to_datetime(df["tid"])

print(df)

fig, (ax1, ax2, ax3) = plt.subplots(3, 1)
df.plot(
    ax=ax1,
    x="tid",
    y=["temperatur"],
    title="Temperatur i nordnes sjøbad over tid",
    legend=False,
)
ax1.set_xlabel("Tid")
ax1.set_ylabel("Temperatur (°C)")
df.plot(
    ax=ax2,
    x="tid",
    y=["lufttemperatur"],
    title="Lufttemperatur i nordnes sjøbad over tid",
    legend=False,
)
ax2.set_xlabel("Tid")
ax2.set_ylabel("Lufttemperatur (°C)")
df.plot(
    ax=ax3,
    x="tid",
    y=["temperatur", "lufttemperatur"],
    title="Temperatur i nordnes sjøbad over tid",
)
ax3.set_xlabel("Tid")
ax3.set_ylabel("Temperatur (°C)")

plt.tight_layout()
plt.subplots_adjust(hspace=0.5)
plt.show()

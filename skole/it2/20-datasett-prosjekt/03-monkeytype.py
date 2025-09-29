import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("03-monkeytype-all.csv")
df["date"] = pd.to_datetime(df["timestamp"])

# print(df)

fig, ax = plt.subplots()

df["wpm"].plot(
    ax=ax,
    title="Words per minute over time",
    xlabel="Number of tests",
    ylabel="Words per minuts",
)
pbs_x = df.index[df["isPb"] == True]
pbs_y = df["wpm"][pbs_x]
ax.scatter(pbs_x, pbs_y, color="red", label="Personal best", zorder=3)
ax.legend()

plt.show()

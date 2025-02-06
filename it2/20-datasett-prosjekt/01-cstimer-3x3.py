import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import re

df = pd.read_csv("01-cstimer-3x3.csv", delimiter=";")
df["Date"] = pd.to_datetime(df["Date"])


def parse_time(time):
    pattern = r"(?:(\d)+:)?(\d+)\.(\d+)"
    match = re.match(pattern, time)
    if match:
        mins = int(match.group(1)) if match.group(1) else 0
        seconds = int(match.group(2))
        hundredths = int(match.group(3))
        milliseconds = hundredths * 10 + seconds * 1000 + mins * 60000
        return pd.Timedelta(milliseconds, "ms")
    else:
        return np.nan


df["Time"] = df["Time"].apply(parse_time)
df = df.dropna(subset=["Time"])
df["Seconds"] = df["Time"].dt.total_seconds()
print(df)

fig, ax = plt.subplots()

df.plot(
    ax=ax,
    x="No.",
    y=["Seconds"],
    title="3x3 Rubik's cube solves over time",
    xlabel="Solve number",
    ylabel="Time in seconds",
)

trend_line = np.polyval(np.polyfit(df["No."], df["Seconds"], 1), df["No."])
ax.plot(df["No."], trend_line, linestyle="--", label="Trend line")
ax.legend()
plt.show()

import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("monkeytype.csv")
df["timestamp"] = pd.to_datetime(df["timestamp"])
# df = df[::-1]

print(df)
df.plot(y=["wpm"])
plt.show()

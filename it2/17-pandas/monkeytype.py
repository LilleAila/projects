import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("monkeytype.csv")
df["timestamp"] = pd.to_datetime(df["timestamp"])

print(df)
df.plot(y=["wpm"])
plt.show()

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from matplotlib.ticker import MaxNLocator

df = pd.read_csv("02-studietid-2.csv")
df["Dato"] = pd.to_datetime(df["Dato"], format="%d.%m.%Y")

print(df)

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)

df["Rom"].value_counts().plot(ax=ax1, kind="pie", ylabel="", title="Rom")
df["Fag"].value_counts().plot(ax=ax2, kind="pie", ylabel="", title="Fag")
df["Lærer"].value_counts().plot(ax=ax3, kind="pie", ylabel="", title="Lærer")
df["Dato"].hist(ax=ax4, bins=16, grid=False)
ax4.set_title("Studietimer fordeling over tid")
ax4.xaxis.set_major_locator(MaxNLocator(integer=True, prune="both", nbins=16))
ax4.tick_params(axis="x", rotation=45)

plt.tight_layout()
plt.show()

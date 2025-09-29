"""
Sett inn en flerlinjet kommentar her
"""

import pandas as pd
import matplotlib.pyplot as plt

pd.set_option("display.float_format", "{:,.0f}".format)
df = pd.read_csv("01-youtube.csv")

# land = df["Country"].value_counts().head(10)
# print(land)

# land_data = df.groupby("Country")[["subscribers", "video views"]].mean()
# print(land_data)

land = (
    df.groupby("Country")
    .agg(
        Channels=("Country", "count"),
        Subscribers=("subscribers", "mean"),
        Views=("video views", "mean"),
    )
    .reset_index()  # .agg creates two levels, I want the result of the aggregation function which is applied
    .sort_values("Channels", ascending=False)
    .head(10)
    .reset_index(drop=True)
)
print(land)

# fig, (ax1, ax2, ax3) = plt.subplots(1, 3)
# land.plot(ax=ax1, kind="bar", x="Country", y=["Channels"])
# land.plot(ax=ax2, kind="bar", x="Country", y=["Subscribers"])
# land.plot(ax=ax3, kind="bar", x="Country", y=["Views"])
# plt.show()

land.plot(
    kind="bar",
    x="Country",
    y=["Channels", "Subscribers", "Views"],
    subplots=True,
)
plt.show()

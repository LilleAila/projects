"""
https://docs.ankiweb.net/stats.html#manual-analysis
"""

import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("04-anki-reviews.csv")
df["date"] = pd.to_datetime(df["id"], unit="ms")
df["time"] = df["time"] / 1000  # ms -> s
df["ivl"] = df["ivl"].apply(
    lambda x: pd.to_timedelta(x, unit="D") if x > 0 else pd.to_timedelta(-x, unit="s")
)

cards = df.drop_duplicates(subset="cid", keep="last")

fig, ((ax1, ax2, ax3), (ax4, ax5, ax6)) = plt.subplots(2, 3)
fig.suptitle("Anki review statistics")

df["date"].hist(ax=ax1, bins=20, grid=False)
ax1.set_title("Review distribution over time")
ax1.tick_params("x", labelsize=8)
ax1.set_xticks(ax1.get_xticks())  # needed to stop a warning on the next line /shrug
ax1.set_xticklabels(ax1.get_xticklabels(), rotation=45, ha="right")

cards["type"].value_counts().plot(ax=ax2, kind="bar", xlabel="")
ax2.set_xticklabels(["Learning", "Review", "Relearning", "Cram"], rotation=0)
ax2.set_title("Card type")

df["ease"].value_counts().sort_index().plot(ax=ax3, kind="bar", xlabel="")
ax3.set_title("Ease")
ax3.set_xticklabels(["Again", "Hard", "Good", "Easy"], rotation=0)

df["time"].hist(ax=ax4, grid=False)
ax4.set_title("Time spent per card in seconds")

cards["ivl"].dt.days.hist(ax=ax5, grid=False)
ax5.set_title("Card interval distribution")
ax5.set_xlabel("Interval in days")

df["date"].dt.weekday.value_counts().sort_index().plot(ax=ax6, kind="bar", xlabel="")
ax6.set_title("Total reviews by weekday")
ax6.set_xticklabels(
    ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
    rotation=45,
    ha="right",
)

print(df["cid"].value_counts())

plt.show()

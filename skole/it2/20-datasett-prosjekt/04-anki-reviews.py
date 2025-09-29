"""
https://docs.ankiweb.net/stats.html#manual-analysis
"""

import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("04-anki-reviews.csv")
df["date"] = (
    pd.to_datetime(df["id"], unit="ms")
    .dt.tz_localize("UTC")
    .dt.tz_convert("Europe/Oslo")
)
df["hour"] = df["date"].dt.hour
df["time"] = pd.to_timedelta(df["time"], unit="ms")  # type: ignore # lsp complains again, idk why
df["ivl"] = df["ivl"].apply(
    lambda x: pd.to_timedelta(x, unit="D") if x > 0 else pd.to_timedelta(-x, unit="s")
)
df["lastIvl"] = df["lastIvl"].apply(
    lambda x: pd.to_timedelta(x, unit="D") if x > 0 else pd.to_timedelta(-x, unit="s")
)
df["efficiency"] = df["ease"] / df["time"].dt.total_seconds()

cards = df.drop_duplicates(subset="cid", keep="last")

fig, ((ax1, ax2, ax3, ax4), (ax5, ax6, ax7, ax8)) = plt.subplots(2, 4)
fig.suptitle("Anki review statistics")
fig.subplots_adjust(wspace=0.3, hspace=0.25)

df["date"].hist(ax=ax1, bins=20, grid=False)
ax1.set_title("Review distribution over time")
ax1.tick_params("x", labelsize=7)
ax1.set_xticks(ax1.get_xticks())  # needed to stop a warning on the next line /shrug
ax1.set_xticklabels(ax1.get_xticklabels(), rotation=45, ha="right")
ax1.set_ylabel("Reviews")

cards["type"].value_counts().plot(ax=ax2, kind="bar", xlabel="")
ax2.set_xticklabels(["Learning", "Review", "Relearning", "Cram"], rotation=0)
ax2.set_title("Card type distribution")
ax2.set_ylabel("Cards")

df["ease"].value_counts().sort_index().plot(ax=ax3, kind="bar", xlabel="")
ax3.set_title("Review ease distribution")
ax3.set_xticklabels(["Again", "Hard", "Good", "Easy"], rotation=0)
ax3.set_ylabel("Reviews")

df["time"].dt.total_seconds().hist(ax=ax4, grid=False)
ax4.set_title("Time spent per review in seconds")
ax4.set_xlabel("Seconds")
ax4.set_ylabel("Reviews")

cards["ivl"].dt.days.hist(ax=ax5, bins=20, grid=False)
ax5.set_title("Card interval distribution")
ax5.set_xlabel("Interval in days")
ax5.set_ylabel("Cards")

df["date"].dt.weekday.value_counts().sort_index().plot(ax=ax6, kind="bar", xlabel="")
ax6.set_title("Reviews by weekday")
ax6.set_xticklabels(
    ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
    rotation=45,
    ha="right",
)
ax6.set_ylabel("Reviews")

df["hour"].value_counts().sort_index().plot(ax=ax7, kind="bar", xlabel="")
ax7.set_title("Reviews by time of day")
ax7.set_xlabel("Hour")
ax7.set_ylabel("Reviews")

df.groupby("hour")["efficiency"].mean().plot(ax=ax8, kind="bar")
ax8.set_title("Review efficiency time of day")
ax8.set_xlabel("Hour")
ax8.set_ylabel("Average efficiency (ease / time)")

plt.show()

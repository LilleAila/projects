"""
https://github.com/koreader/koreader/issues/6454#issuecomment-666569750

Exported the following collections:
- book: All read books on the device
- page_stat_data: Statistics about each page read, related to a book by ID
from kindle (1st ereader), and kobo (2nd ereader)

The data was manually modified to combine the two datasets and remove unimportant data (books that are not actually read, etc.)
Original data is still available, and the combined data is under the files called `-combined`

The original file is sorted by time first opened.

Unfortunately, the limit for time per page is 120 seconds, so no data is recorded above that.
"""

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

books = pd.read_csv("05-koreader-combined-books.csv")
pages = pd.read_csv("05-koreader-combined-pages.csv")
pages["date"] = (
    pd.to_datetime(pages["start_time"], unit="s")
    .dt.tz_localize("UTC")
    .dt.tz_convert("Europe/Oslo")
)
pages["hour"] = pages["date"].dt.hour

# Visualize minutes per page, for each book.
books["minutes_per_page"] = books["total_read_time"] / books["total_read_pages"] / 60
books = books.sort_values("minutes_per_page")

fig, ((ax1, ax2), (ax3, ax4), (ax5, ax6)) = plt.subplots(3, 2)
fig.suptitle("Book reading statistics")
fig.subplots_adjust(wspace=0.2, hspace=0.3)

books.plot(
    ax=ax1, kind="barh", x="title", y="minutes_per_page", ylabel="", legend=False
)
# plt.subplots_adjust(left=0.4)
# ax1.set_position([0.35, 0.1, 0.15, 0.8])
pos = ax1.get_position()
ax1.set_position([pos.x0 + 0.15, pos.y0, pos.width - 0.15, pos.height])
ax1.set_yticklabels(ax1.get_yticklabels())
ax1.set_xlabel("Minutes per pgae")
ax1.set_title("Average time spent per page by book")

# Visualize time spent on each page over time
# pages_fr = pages[pages["id_book"] == 7]
pages_by_book = pages.groupby("id_book")
pages_book = pages_by_book.get_group(6).sort_values("page")  # type: ignore # thhgttg, lsp complains?
pages_book.plot(ax=ax2, kind="line", x="page", y="duration", legend=False)
pages_book["rolling_avg"] = pages_book["duration"].rolling(window=20).mean()
pages_book.plot(
    ax=ax2,
    kind="line",
    x="page",
    y="rolling_avg",
    color="orange",
    linewidth=3,
    legend=False,
)
trend_line = np.polyval(
    np.polyfit(pages_book["page"], pages_book["duration"], 1), pages_book["page"]
)
ax2.plot(pages_book["page"], trend_line, linestyle="--", color="red", linewidth=3)
ax2.set_xlabel("")
ax2.set_ylabel("Seconds per page")
ax2.set_title("Time spent per page: The hitchhiker's guide to the galaxy")

# Most productive days
pages["weekday"] = pages["date"].dt.day_name()
weekday_order = [
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday",
]
weekdays = pages["weekday"].value_counts().reindex(weekday_order)
weekdays.plot(ax=ax3, kind="bar", xlabel="")
ax3.set_xticklabels(ax3.get_xticklabels(), rotation=0)
ax3.set_ylabel("Total pages")
ax3.set_title("Pages read by day")

pages["date"].hist(ax=ax4, bins=20, grid=False)
ax4.set_title("Pages read over time")

pages["hour"].value_counts().sort_index().plot(ax=ax5, kind="bar", xlabel="")
ax5.set_xlabel("Hour")
ax5.set_ylabel("Total pages")
ax5.set_title("Pages read by time of day")

pages.groupby("hour")["duration"].mean().plot(ax=ax6, kind="bar")
ax6.set_xlabel("Hour")
ax6.set_ylabel("Seconds per page")
ax6.set_title("Average time per page by time of day")

plt.show()

"""
https://github.com/koreader/koreader/issues/6454#issuecomment-666569750

Exported the following collections:
- book: All read books on the device
- page_stat_data: Statistics about each page read, related to a book by ID
from kindle (1st ereader), and kobo (2nd ereader)

The data was manually modified to combine the two datasets and remove unimportant data (books that are not actually read, etc.)
Original data is still available, and the combined data is under the files called `-combined`

The original file is sorted by time first opened.
"""

import matplotlib.pyplot as plt
import pandas as pd

books = pd.read_csv("05-koreader-combined-books.csv")
pages = pd.read_csv("05-koreader-combined-pages.csv")
pages["date"] = pd.to_datetime(pages["start_time"], unit="s")

# Visualize minutes per page, for each book.
books["minutes_per_page"] = books["total_read_time"] / books["total_read_pages"] / 60
books = books.sort_values("minutes_per_page")

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)

books.plot(ax=ax1, kind="barh", x="title", y="minutes_per_page", ylabel="")
# plt.subplots_adjust(left=0.4)
# ax1.set_position([0.35, 0.1, 0.15, 0.8])
pos = ax1.get_position()
ax1.set_position([pos.x0 + 0.15, pos.y0, pos.width - 0.15, pos.height])
ax1.set_yticklabels(
    ax1.get_yticklabels(), rotation=15, ha="right", rotation_mode="anchor"
)
ax1.set_title("Average time spent per page by book")

# Visualize time spent on each page over time
# pages_fr = pages[pages["id_book"] == 7]
pages_by_book = pages.groupby("id_book")
pages_fr = pages_by_book.get_group(6).sort_values("page")  # thhgttg
pages_fr.plot(ax=ax2, kind="line", x="page", y="duration")
pages_fr["rolling_avg"] = pages_fr["duration"].rolling(window=20).mean()
pages_fr.plot(
    ax=ax2, kind="line", x="page", y="rolling_avg", color="orange", linewidth=2
)
ax2.set_title("Time spent per page: The hitchhiker's guide to the galaxy")

# Most productive days
pages["weekday"] = pages["date"].dt.day_name()
weekdays = pages["weekday"].value_counts(sort=False)
weekdays.plot(ax=ax3, kind="bar")
ax3.set_xticklabels(
    ax3.get_xticklabels(), rotation=45, ha="right", rotation_mode="anchor"
)
ax3.set_title("Pages read by day")

plt.show()

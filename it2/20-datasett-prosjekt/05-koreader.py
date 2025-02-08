"""
https://github.com/koreader/koreader/issues/6454#issuecomment-666569750

Exported the following collections:
- book: All read books on the device
- page_stat_data: Statistics about each page read, related to a book by ID
from kindle (1st ereader), and kobo (2nd ereader)

The data was manually modified to combine the two datasets and remove unimportant data (books that are not actually read, etc.)
Original data is still available, and the combined data is under the files called `-combined`
"""

import pandas as pd

books = pd.read_csv("05-koreader-combined-books.csv")
pages = pd.read_csv("05-koreader-combined-pages.csv")

print(books)
print(pages)

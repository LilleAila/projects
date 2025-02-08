"""
https://docs.ankiweb.net/stats.html#manual-analysis
"""

import pandas as pd

df = pd.read_csv("04-anki-reviews.csv")
df["date"] = pd.to_datetime(df["id"], unit="ms")

print(df)

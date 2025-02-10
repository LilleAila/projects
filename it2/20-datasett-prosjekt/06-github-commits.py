"""
https://docs.github.com/en/rest/search/search?apiVersion=2022-11-28

Denne filen ble lastet ned med:
wget 'https://api.github.com/search/commits?q=author:lilleaila'

Men gir kin noen få resulteter. Må gjøre noe med pagination, og sikkert
?q=author:lilleaila+committer-date:2023-01-01..2023-12-31
for å filtrere etter dato, siden maks 1000 resultater. Pagination med
&per_page=100&page=1
"""

import pandas as pd
import json

with open("06-github-commits.json") as file:
    data = json.load(file)

commits = [
    {
        "message": c["commit"]["message"],
        "date": pd.to_datetime(c["commit"]["author"]["date"]),
    }
    for c in data["items"]
]

df = pd.DataFrame(commits)

print(df)

import requests
from bs4 import BeautifulSoup
import csv

# They allow scraping: https://sprakradet.no/robots.txt
url = "https://sprakradet.no/godt-og-korrekt-sprak/praktisk-sprakbruk/nynorskhjelp/administrativ-ordliste-bokmal-nynorsk/"
response = requests.get(url)
html_content = response.text

soup = BeautifulSoup(html_content, "html.parser")

tables = soup.select(".wp-block-table table")

words = []
for table in tables:
    rows = table.find_all("tr")
    for row in rows:
        columns = row.find_all("td")
        words.append([column.get_text(strip=True) for column in columns])

with open("output.csv", "w", newline="", encoding="utf-8") as file:
    writer = csv.writer(file, delimiter=";")
    writer.writerow(["Bokmål", "Nynorsk"])
    writer.writerows(words)

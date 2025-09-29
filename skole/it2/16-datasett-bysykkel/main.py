import json
import csv
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime

# TODO. idk pandas
# df = pd.read_json("05.json")
# print(df)

with open("05.json") as file:
    data = json.load(file)

# with open("05.csv") as file:
#     reader = csv.DictReader(file)
#     data = list(reader)

frequencies = {}
for location in data:
    name = location["start_station_name"]
    frequencies[name] = frequencies.get(name, 0) + 1

locations = sorted(frequencies.items(), key=lambda l: -l[1])

# Ville formatert det skikkelig på faktisk prøve, men unødvendig når jeg bare vil ha enkle data
print(locations[:3])
print(locations[-3:])

days = [0] * 7
for route in data:
    day = datetime.fromisoformat(route["started_at"]).weekday()
    days[day] += 1

print(days)

# TODO diagram. idk how to matplotlib heller

fig, ax = plt.subplots()
plt.title("Sykkelturer med Oslo bysykkel per dag")
plt.xlabel("Dag")
plt.ylabel("Antall turer")
ax.bar(["Mandag", "Tirsdag", "Onsdag", "Torsdag", "Fredag", "Lørdag", "Søndag"], days)
plt.show()

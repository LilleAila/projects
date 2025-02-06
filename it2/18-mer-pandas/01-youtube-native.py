"""
Sett inn en flerlinjet kommentar her
"""

import csv

# Bør kommentere at kopierte og pastet i vim heller enn å laste ned filen
# , som kan ha fikset potensielle problemer med encoding
with open("01-youtube.csv") as file:
    reader = csv.DictReader(file)
    data = list(reader)

# Oppgave a
num_channels = {}
for channel in data:
    country = channel["Country"]
    # Dette bør kommenteres
    if country == "nan":
        continue
    num_channels[country] = num_channels.get(country, 0) + 1
# Dette bør kommenteres
countries = sorted(num_channels.items(), key=lambda l: -l[1])
top10 = countries[:10]

print("Land med flest kanaler:")
for country, channels in top10:
    print(f"{country:20}: {channels:3} channels.")


# Oppgave b
def avg(xs):
    return round(sum(xs) / len(xs), 1)


for country, num_channels in top10:
    # Kunne heller strukturert den tidligere koden bedre (forklare at gjøre alt i samme loop)
    # Kommentere at list comprehension, kanskje vise imperative alternativ
    channels = [c for c in data if c["Country"] == country]
    subscribers = avg([float(c["subscribers"]) for c in channels])
    views = avg([float(c["video views"]) for c in channels])

    print(f"{country:15} - {num_channels:3} - {subscribers:10.1f} - {views:13.1f}")

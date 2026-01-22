# Importere bibliotekene som kreves for analysen
import matplotlib.pyplot as plt
import statistics as st
import numpy as np
import matplotlib.ticker as ticker
from collections import Counter

# Lese inn data fra tekstfilen og gjøre dette om til arrays for x- og y-aksene.
ds = list(np.loadtxt("input.txt"))
xs = list(range(len(ds)))

# Regne ut diverse sentralmål
mean = st.mean(ds) # Gjennomsnitt
median = st.median(ds) # Median
deviation = st.stdev(ds) # Standardavvik

print(f"Gjennomsnitt: {mean:.2f}")

# Opprette subplots. Dette gjør at jeg kan plotte flere grafer i det samme bildet.
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 5))

## Plotte den første grafen
# Sette tekst
ax1.set_title("Søvn per dag")
ax1.set_xlabel("Dag siden start")
ax1.set_ylabel("Søvn (timer)")
# Plotte selve dataen og fremheve datapunktene
ax1.plot(xs, ds, marker="o", label="Søvn")
# Legge til linjer for sentralmålene jeg har valgt å regne ut
ax1.axhline(y=mean, label="Gjennomsnitt", linestyle="-", color="black")
ax1.axhline(y=mean + deviation, label=r"$+1\sigma$", linestyle="--", color="black")
ax1.axhline(y=mean - deviation, label=r"$-1\sigma$", linestyle="--", color="black")
ax1.axhline(y=median, label="Median", linestyle="-.", color="green")
# Vise kun heltall på x-aksen.
ax1.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
ax1.legend() # Vise oppsummering

## Plotte den neste grafen
counts = Counter(ds) # Regne ut antall
# Sette teksten
ax2.set_title("Søvn per dag (spørreundersøkelse)")
# Formatere navnet på resultatene
labels = [f"{int(i)} timer" for i in counts.keys()]
# Plotte sektordiagrammet
ax2.pie(counts.values(), labels=labels, autopct="%1.0f%%")

# Lagre og forhåndsvise resultatet
plt.savefig("plot.png")
plt.show()

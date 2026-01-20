import matplotlib.pyplot as plt
import statistics as st
import numpy as np

ds = np.loadtxt("input.txt")
xs = np.array(range(len(ds)))

mean = st.mean(ds)
median = st.median(ds)
mode = st.mode(ds)
variation = max(ds) - min(ds)
uncertainty = variation / 2
deviation = st.stdev(ds)

fig, (ax1, ax2) = plt.subplots(1, 2)

ax1.set_title("Nedbør over tid")
ax1.set_xlabel("Tid (t)")
ax1.set_ylabel("Nedbør (mm)")
ax1.plot(xs, ds, label="Nedbør")
ax1.axhline(y=mean, label="Gjennomsnitt", linestyle="-", color="black")
ax1.axhline(y=mean + deviation, label=r"$+1\sigma$", linestyle="--", color="black")
ax1.axhline(y=mean - deviation, label=r"$-1\sigma$", linestyle="--", color="black")
ax1.axhline(y=median, label="Median", linestyle="-.", color="green")
ax1.axhline(y=mode, label="Typetall", linestyle=":", color="green")
ax1.axhline(y=max(ds), label=r"+Usikkerhet", linestyle="-.", color="red")
ax1.axhline(y=min(ds), label=r"-Usikkerhet", linestyle="-.", color="red")
ax1.legend()

plt.show()

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_csv(
    "PrecipVestlandMonthly.csv",
    header=None,
    names=[
        "Year",
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December",
    ],
)
df[df == -999.99] = np.nan
# data = data.transpose()
df["Average"] = df[1:].mean(axis=1)

print(df)

fig, ax = plt.subplots()
df.plot(ax=ax, x="Year", y=["Average"], title="Nedbør over tid")
plt.xlabel("Årstall")
plt.ylabel("Nedbør")
plt.show()

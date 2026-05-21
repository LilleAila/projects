import pandas as pd

# NSR:Quay:53028 : Bergen busstasjon

# Before route change
df = pd.read_csv("query_result_export_2025-08-01_2025-08-05_SKY.csv")
df = df[df["lineRef"] == "SKY:Line:5"]

print(df["stopPointRef"].value_counts())
print("Times stopped at bergen busstasjon before:", len(df[df["stopPointRef"] == "NSR:Quay:53028"]))

# After route change
df = pd.read_csv("query_result_export_2025-10-01_2025-10-02_SKY.csv")
df = df[df["lineRef"] == "SKY:Line:5"]

print(df["stopPointRef"].value_counts())
print("Times stopped at bergen busstasjon after:", len(df[df["stopPointRef"] == "NSR:Quay:53028"]))

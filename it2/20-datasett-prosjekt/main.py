import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_csv("cstimer-3x3.csv", delimiter=";")
df["Date"] = pd.to_datetime(df["Date"])

print(df)

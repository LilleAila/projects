import csv
from collections import defaultdict
import timeit
import pandas as pd


def manual():
    with open("./diabetes.csv") as file:
        reader = csv.DictReader(file)
        df = list(reader)

    smoker = defaultdict(int)

    for r in df:
        if r["Diabetes_binary"] == "1.0":
            smoker[r["Smoker"]] += 1

    print(smoker)


def pandas():
    df = pd.read_csv("./diabetes.csv")
    print(df[df["Diabetes_binary"] == 1].groupby("Smoker").size())


if __name__ == "__main__":
    result = timeit.timeit(manual, number=5)
    print(result)
    result2 = timeit.timeit(pandas, number=5)
    print(result2)

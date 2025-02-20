from timeit import timeit
import pandas as pd
import csv


def read_native():
    data = []
    with open("pages.csv") as file:
        lines = file.readlines()
        header = lines[0].strip().split(",")
        for l in lines:
            cells = l.strip().split(",")
            data.append({header[i]: cells[i] for i in range(len(header))})
    return data


def read_csv():
    with open("pages.csv") as file:
        reader = csv.DictReader(file)
        return list(reader)


def read_pandas():
    df = pd.read_csv("pages.csv")
    return df


if __name__ == "__main__":
    native_time = timeit(read_native, number=1000)
    print(f"Native: {native_time}")
    csv_time = timeit(read_csv, number=1000)
    print(f"Csv: {csv_time}")
    pandas_time = timeit(read_pandas, number=1000)
    print(f"Pandas: {pandas_time}")

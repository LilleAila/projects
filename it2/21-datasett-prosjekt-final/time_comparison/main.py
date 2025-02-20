"""
Ulike implementasjoner for å lese data fra en CSV-fil inn i et format som er enkelt å jobbe med gjennom python.
"""

from datetime import datetime, timedelta
from collections import defaultdict
from timeit import timeit
import pandas as pd
import pytz
import csv


def read_native():
    data = []
    with open("pages.csv") as file:
        lines = file.readlines()
        header = lines[0].strip().split(",")
        for l in lines[1:]:
            cells = l.strip().split(",")
            data.append({header[i]: cells[i] for i in range(len(header))})
    return data


def read_native_types_pre():
    return [
        {
            "id_book": int(r["id_book"]),
            "page": int(r["page"]),
            "start_time": datetime.fromtimestamp(
                int(r["start_time"]), tz=pytz.UTC
            ).astimezone(pytz.timezone("Europe/Oslo")),
            "duration": timedelta(seconds=int(r["duration"])),
            "total_pages": int(r["total_pages"]),
        }
        for r in read_native()
    ]


def read_native_types_on_demand():
    """
    Simulere ulike handlinger som kan bli gjort dersom man konverterer types on demand.
    """
    for r in read_native():
        int(r["total_pages"])
        int(r["page"])
        int(r["page"])
        datetime.fromtimestamp(int(r["start_time"]), tz=pytz.UTC).astimezone(
            pytz.timezone("Europe/Oslo")
        )
        int(r["duration"])
        timedelta(seconds=int(r["duration"]))


def read_csv():
    with open("pages.csv") as file:
        reader = csv.DictReader(file)
        return list(reader)


def read_csv_types_pre():
    return [
        {
            "id_book": int(r["id_book"]),
            "page": int(r["page"]),
            "start_time": datetime.fromtimestamp(
                int(r["start_time"]), tz=pytz.UTC
            ).astimezone(pytz.timezone("Europe/Oslo")),
            "duration": timedelta(seconds=int(r["duration"])),
            "total_pages": int(r["total_pages"]),
        }
        for r in read_csv()
    ]


def read_csv_types_on_demand():
    """
    Simulere ulike handlinger som kan bli gjort dersom man konverterer types on demand.
    """
    for r in read_csv():
        int(r["total_pages"])
        int(r["page"])
        int(r["page"])
        datetime.fromtimestamp(int(r["start_time"]), tz=pytz.UTC).astimezone(
            pytz.timezone("Europe/Oslo")
        )
        int(r["duration"])
        timedelta(seconds=int(r["duration"]))


def read_pandas():
    df = pd.read_csv("pages.csv")
    return df


def read_pandas_types():
    df = read_pandas()
    df["start_time"] = (
        pd.to_datetime(df["start_time"], unit="s")
        .dt.tz_localize("UTC")
        .dt.tz_convert("Europe/Oslo")
    )
    df["duration"] = pd.to_timedelta(df["duration"], unit="s")  # type: ignore # lsp complains, idk why
    return df


def test_native():
    pages = [
        {
            **r,
            "hour": r["start_time"].hour,
        }
        for r in read_native_types_pre()
    ]
    hours = defaultdict(list)
    for p in pages:
        hours[p["hour"]].append(p["duration"].total_seconds())
    sorted([(h, sum(d) / len(d)) for h, d in hours.items()])

    hours = defaultdict(int)
    for p in pages:
        hours[p["hour"]] += 1
    sorted(hours.items())


def test_pandas():
    pages = read_pandas_types()
    pages["hour"] = pages["start_time"].dt.hour
    pages.groupby("hour")["duration"].mean()
    pages["hour"].value_counts().sort_index()


def test_reading():
    native_time = timeit(read_native, number=2500)
    print(f"Native: {native_time}")
    native_types_on_demand_time = timeit(read_native_types_pre, number=2500)
    print(f"Native (precomputed types): {native_types_on_demand_time}")
    native_types_on_demand_time = timeit(read_native_types_on_demand, number=2500)
    print(f"Native (on-demand types): {native_types_on_demand_time}")

    csv_time = timeit(read_csv, number=2500)
    print(f"Csv: {csv_time}")
    csv_types_on_demand_time = timeit(read_csv_types_pre, number=2500)
    print(f"Csv (precomputed types): {csv_types_on_demand_time}")
    csv_types_on_demand_time = timeit(read_csv_types_on_demand, number=2500)
    print(f"Csv (on-demand types): {csv_types_on_demand_time}")

    pandas_time = timeit(read_pandas, number=2500)
    print(f"Pandas: {pandas_time}")
    pandas_types_time = timeit(read_pandas_types, number=2500)
    print(f"Pandas (precomputed types): {pandas_types_time}")


def test_computations():
    native_time = timeit(test_native, number=2500)
    print(f"Native: {native_time}")

    pandas_time = timeit(test_pandas, number=2500)
    print(f"Pandas: {pandas_time}")


if __name__ == "__main__":
    test_reading()
    test_computations()

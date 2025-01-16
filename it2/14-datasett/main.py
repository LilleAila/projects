import csv
import json
import matplotlib.pyplot as plt

with open("1-test-read.txt") as file:
    innhold = file.read()
print(innhold)

with open("1-test-read.txt") as file:
    for line in file:
        print(line)

with open("2-test-write.txt", "w") as file:
    file.write("Hello, World!")

with open("3-test-append.txt", "a") as file:
    file.write("Hello, World!\n")

test1 = "a,b,c,d"
test2 = "a b c d"
test3 = "a\tb\tc\td"
print(test1.split(","))
print(test2.split())
print(test3.split("\t"))

test4 = "  abc   \n"
print(test4)
print(test4.lstrip())
print(test4.rstrip())
print(test4.strip())

with open("4-test.csv") as file:
    content = file.read().splitlines()
    for row in content[1:]:
        (year, population) = map(int, row.split(";"))
        print(year, population)

years = []
populations = []

# Optionally pass encoding if not utf-8
# ex. utf-8-sig, utf-16, latin1
with open("4-test.csv", encoding="utf-8") as file:
    # Optionally pass delimiter if not ,
    reader = csv.reader(file, delimiter=";")
    header = next(reader)
    print(header)
    for row in reader:
        (year, population) = map(int, row)
        years.append(year)
        populations.append(population)

# plt.plot(years, populations)
# plt.grid()
# plt.show()

with open("5-test.json") as file:
    data = json.load(file)

print(data)
print(json.dumps(data, indent=2))
print(data["dataset"]["dimension"]["Tid"]["category"]["label"])
print(data["dataset"]["value"])

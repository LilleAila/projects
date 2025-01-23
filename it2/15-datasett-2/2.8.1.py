import json

with open("p_2_8_1.json") as file:
    data = json.load(file)

# a)
print("Elise veier {} kg.".format([r for r in data if r["navn"] == "Elise"][0]["vekt"]))

# b)
for person in data:
    print(
        f"""
{person["navn"]}
  vekt\t: {person["vekt"]}
  høyde\t: {person["høyde"]}"""
    )

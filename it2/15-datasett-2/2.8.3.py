import json

with open("p_2_8_3.json") as file:
    data = json.load(file)

print("Bilmerker:")
for car in data:
    print(f"  {car}")


def print_car(car):
    # alt:
    # for name, value in car.items():
    #     print(f"{name:10}: {value}")
    print(
        f"""  modell: {car["modell"]}
      farge: {car["farge"]}
    """
    )


car = data["Ford"][1]
print("En utvalgt Ford modell:")
print_car(car)

for bilmerke, modeller in data.items():
    print(bilmerke)
    for modell in modeller:
        print_car(modell)


def biler_med_farge(farge: str) -> None:
    print(f"Modeller med fargen: {farge}")
    printed = False
    for bilmerke, modeller in data.items():
        modeller = list(filter(lambda m: m["farge"] == farge, modeller))
        if len(modeller) >= 1:
            print(bilmerke)
            [print_car(car) for car in modeller]
            printed = True
    if not printed:
        print("Ingen funnet")


biler_med_farge("Rød")
biler_med_farge("Lilla")

import json

data = []


while True:
    name = input("Navn: ")
    if len(name) == 0:
        break
    age = input("Alder: ")
    if len(age) == 0:
        break
    elif age.isnumeric():
        age = int(age)
    else:
        print("Alder er ikke et heltall!")
        continue
    data.append({"navn": name, "alder": age})

with open("p_2_8_2.json", "w") as file:
    # Equivalent.
    # file.write(json.dumps(data, indent=1))
    json.dump(data, file, indent=1)

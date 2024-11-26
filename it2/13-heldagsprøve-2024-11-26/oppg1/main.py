def BMI(weight: float, height: float) -> float:
    return weight / height**2


def BMI_category(bmi: float) -> str:
    # Velger å ta < 25 i stedet for <= 24.9 i 1.e)
    return "Overvektig" if bmi > 25 else "Normalvektig" if bmi > 18.5 else "Undervektig"


def int_input(prompt: str) -> int:
    # Be om input og validere
    while True:
        result = input(prompt)
        # Velger i 1.d) å også avslutte
        if len(result) == 0:
            print("Takk for nå.")
            exit(0)  # Avslutte programmet
        try:
            num = int(result)
            if num > 0:
                return num
            else:
                print("Tallet må være mer enn 0")
        except ValueError:
            print("Dette er ikke et heltall!")


def ask_BMI() -> None:
    height = int_input("Skriv inn høyden i cm: ")
    weight = int_input("Skriv inn vekten i kg: ")
    bmi = BMI(weight, height / 100)
    category = BMI_category(bmi)
    print(f"BMI: {bmi:.1f} ({category})")


if __name__ == "__main__":
    while True:
        print("Regn ut din BMI. Trykk enter for å avslutte")
        ask_BMI()
        print()

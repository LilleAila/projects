while True:
    try:
        n = int(input("Antall billetter: "))
    except ValueError:
        print("Du må skrive inn et heltall!")
        continue

    if not 1 <= n <= 10:
        print("Du kan kjøpe mellom 1 og 10 billetter.")
        continue

    p = n * 150
    print(f"Totalpris: {p} kr")
    break

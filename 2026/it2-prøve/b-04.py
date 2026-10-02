while True:
    try:
        alder = int(input("Hvor gammel er du? "))
        break
    except ValueError:
        pass

studentbevis = input("Har du studentbevis? (ja/nei, nei standard) ").lower() == "ja"

if alder < 12:
    print("Barn, 80kr")
elif alder >= 67:
    print("Honnør, 100kr")
elif studentbevis and 12 <= alder <= 30:
    print("Student, 110kr")
else:
    print("Andre, 150kr")

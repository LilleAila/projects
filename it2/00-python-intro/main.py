"""
Oppgave:
1. Valider input fra bruker
2. Lag et program som forteller en bruker hvilke kjøretøy de har lov til å kjøre med alderen sin
"""


def numInput(text):
    while True:  # Løkke som får for alltid
        try:
            # Return vil også stoppe løkken. Dersom den feiler med ValueError, vil den fortsette.
            return int(input(text))
        except ValueError:
            print("Du  må skrive inn et heltall!")


age = numInput("Skriv inn alderen din: ")
print(f"Du er {age} år gammel")

if age in range(0, 101):  # [0, 101>
    print("yay du er innenfor aldersgrensen")
else:
    print("nuh uh")
    exit(1)  # 1 er en feilkode, som betyr at programmet ikke klarte å kjøre riktig

# Lager en liste over lister med lovlige kjøretøy for hver alder
førerkort = [[] for _ in range(50)]  # Liste over tomme lister
førerkort[16].append("Moped")
førerkort[16].append("Lett motorsykkel")
førerkort[16].append("Traktor")
førerkort[18].append("Bil")
førerkort[18].append("Mellomtung motorsykkel")
# Kunne brukt en dictionary her, men valgte lister siden da er det enklere å jobbe med tall

# Passe på at jeg ikke prøver å ta en index som er utenfor listen
# Dette er en syntaks for å ta "alle elementer frem til indeks med [:n]"
valid = førerkort[: min(age + 1, len(førerkort))]  # Lister i python er 0-indeksert
# Ulike måter å concat ulike lister: https://stackoverflow.com/questions/716477/join-list-of-lists-in-python
# Jeg valgte det uten noen eksterne dependencies.
can_drive = [inner for outer in valid for inner in outer]
if not valid:
    print("Du er ikke gammel nok til å kjøre!")
else:
    print(f"Du er gammel nok til å kjøre {', '.join(can_drive)}!")

# . før users betyr at den skal importere relativt til denne filen i stedet for fra root av prosjektet
from .users import Users, UserDoesNotExistError
from getpass import getpass  # Bultin, hide chars when inputting password


class Login:
    def __init__(self):
        self._username = None
        self._users = Users()

    def _input(self, prompt):
        _input = ""
        while True:
            # Spørre bruker og fjerne ekstra mellomrom
            _input = input(f"{prompt} ").strip()
            if len(_input) > 0:
                # Spørre på nytt helt til brukeren faktisk skriver noe
                return _input
            print("Du må skrive inn noe!")

    # Samme som over men bruker getpass
    def _input_password(self, prompt):
        _input = ""
        while True:
            _input = getpass(f"{prompt} ").strip()
            if len(_input) > 0:
                return _input
            print("Du må skrive inn noe!")

    # ja/nei-spørsmål til brukeren
    def _yesno(self, prompt):
        return self._input(f"{prompt} (j/n)").lower() == "j"

    """
    Funksjoner skrevet rundt Users for å gi tilbakemelding til brukere.
    """

    def _list_users(self):
        users = self._users.get_all_users()
        print("Alle brukere:")
        print("\n".join([user["username"] for user in users if user["type"] == "user"]))

    def _remove_user(self, username):
        if self._yesno("Er du sikker på at du vil fjerne brukeren din?"):
            self._users.remove_user(username)
            print("Brukeren din ble fjernet!")

    def _login(self):
        print("Logger inn!")
        counter = 0
        # Spørre på nytt helt til man er logget inn eller har brukt for mange forsøk
        while True:
            username = self._input("Brukernavnet ditt:")
            password = self._input_password("Passordet ditt:")
            if self._users.check_password(username, password):
                user = self._users.get_user(username)
                # Dette er mest for at editoren ikke skal klage
                assert user is not None, "password checking would have failed"
                if user["2fa"] is not None:
                    if not self._users.verify_2fa(username):
                        print("Koden er feil!")
                        continue
                self._username = username
                self._success()
                break
            # Samme som å ha inni else pga. break
            print("Brukernavn eller passord er feil.")
            counter += 1
            if counter >= 3:
                print("Du har brukt for mange forsøk!")
                break

    def _signup(self):
        print("Lager en ny bruker!")
        # Spørre på nytt helt til brukeren er gyldig
        while True:
            username = self._input("Lag et brukernavn:")
            while True:
                # Sjekke at passordet er likt to ganger
                password = self._input_password("Lag et passord:")
                password2 = self._input_password("Skriv inn passordet på nytt:")
                if password == password2:
                    break
                else:
                    print("Passordene er ikke like!")
            user = self._users.add_user(username, password)
            if user is not None:
                print(f"Brukeren din {username} ble laget!")
                break
            else:
                print(f"Brukeren med brukernavn {username} eksisterer allerede!")

    """
    Løkke for å starte innloggingsprossessen på nytt når man har logget ut
    """

    def main(self):
        self._running = True
        while self._running:
            if self._yesno("Har du en bruker fra før?"):
                self._login()
            else:
                self._signup()

    """
    Gi alternativer til brukeren om hva som er mulig å gjøre og kjøre de ulike funksjonene derfra
    """

    # Konfigurere tofaktor-innlogging
    def _config_2fa(self):
        while True:
            print(
                """
Du kan:
- (s)ett opp 2fa
- (t)est 2fa
- (g)å tilbake
                """
            )
            action = self._input("Hva vil du gjøre?")
            print()
            match action:
                case "s":
                    code = self._users.get_2fa(self._username)
                    print(
                        f"Skriv inn den følgende koden i autentiserings-appen din: {code}"
                    )
                case "t":
                    if self._users.verify_2fa(self._username):
                        print("Koden er riktig!")
                    else:
                        print("Koden er feil!")
                case _:
                    break

    # Hoved-skjermen som vises etter man er logget inn
    def _success(self):
        print(f"Du har blitt logget inn som {self._username}!")
        while True:
            print(
                """
Du kan:
- (l)ogg ut
- (s)topp programmet
- (a)lle brukere
- (t)ofaktor autentisering
- (f)jern brukeren din
            """
            )
            action = self._input("Hva vil du gjøre?")
            print()
            match action:
                case "l":
                    # Stopp løkken og gå tilbake til main()
                    self._username = None
                    break
                case "s":
                    # Stopp løkken *og* fortelle hovedfunksjonen at programmet har stoppet
                    self._running = False
                    break
                case "a":
                    self._list_users()
                case "t":
                    self._config_2fa()
                case "f":
                    self._remove_user(self._username)
                    self._username = None
                    print()
                    break
                case _:
                    print("Det er ikke et gyldig valg!")
                    continue

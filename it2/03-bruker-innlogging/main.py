from getpass import getpass  # Innebygget i python
import bcrypt
from tinydb import TinyDB, Query
import pyotp

class UserDoesNotExistError(Exception):
    pass

class UserAlreadyExistsError(Exception):
    pass

class DuplicateUserError(Exception):
    pass

class Login:
    def __init__(self):
        self._username = None
        self._db = TinyDB("./db.json")

    def _input(self, prompt):
        _input = ""
        while True:
            _input = input(prompt).strip()  # Spørre bruker og fjerne ekstra mellomrom
            if len(_input) > 0:
                return _input
            print("Du må skrive inn noe!")

    # Samme som over men bruker getpass
    def _input_password(self, prompt):
        _input = ""
        while True:
            _input = getpass(prompt).strip()  # Spørre bruker og fjerne ekstra mellomrom
            if len(_input) > 0:
                return _input
            print("Du må skrive inn noe!")

    def _user_exists(self, username):
        users = self._db.search(Query().username == username)
        return len(users) > 0

    def _check_password(self, username, password):
        users = self._db.search(Query().username == username)
        if len(users) == 0:
            raise UserDoesNotExistError(f"The user {username} does not exist!")
        else:
            return bcrypt.checkpw(password.encode("utf-8"), users[0]["password"].encode("utf-8"))

    def _write_user(self, username, password):
        if self._user_exists(username):
            raise UserAlreadyExistsError(f"The user {username} already exists!")
        self._db.insert({
            "type": "user",
            "username": username,
            # tinydb trenger en vanlig string, ikke bytes, mens bcrypt er motsatt.
            "password": bcrypt.hashpw(password.encode("utf-8"), bcrypt.gensalt()).decode("utf-8"),
            "2fa": None
        })

    def _get_2fa(self, username):
        # Add or get the 2fa key for a user.
        user = self._get_user(username)
        if user["2fa"] is not None:
            return user["2fa"]
        else:
            key = pyotp.random_base32()
            self._db.update({
                "2fa": key
            }, Query().username == username)
            return key

    def _get_user(self, username):
        users = self._db.search(Query().username == username)
        if len(users) == 0:
            raise UserDoesNotExistError(f"The user {username} does not exist!")
        if len(users) > 1:
            raise DuplicateUserError(f"There are multiple users with the username {username}!")
        return users[0]

    def _list_users(self):
        users = self._db.all()
        print("Alle brukere:")
        print("\n".join([user["username"] for user in users if user["type"] == "user"]))

    def _verify_2fa(self, username):
        # Antar at 2fa allerede er satt opp.
        # Dette bør sjekkes før funksjonen calles, eller lages en egen error for det
        input_code = ""
        while True:
            input_code = input("Skriv inn koden fra autentiserings-appen din: ")
            if len(input_code) == 6 and input_code.isdigit():
                break
            print("Koden er ugyldig!")
        totp = pyotp.TOTP(self._get_2fa(username))
        return totp.verify(input_code)

    def _login(self):
        print("Logger inn!")
        counter = 0
        while True:
            username = self._input("Brukernavnet ditt: ")
            password = self._input_password("Passordet ditt: ")
            try:
                if self._check_password(username, password):
                    if self._get_user(username)["2fa"] is not None:
                        if not self._verify_2fa(username):
                            print("Koden er feil!")
                            continue
                    self._username = username
                    self._success()
                    break
            except UserDoesNotExistError:
                print("Brukernavn eller passord er feil.")
                counter += 1
                if counter >= 3:
                    print("Du har brukt for mange forsøk!")
                    break

    def _signup(self):
        print("Lager en ny bruker!")
        while True:
            username = self._input("Lag et brukernavn: ")
            while True:
                password = self._input_password("Lag et passord: ")
                password2 = self._input_password("Skriv inn passordet på nytt: ")
                if password == password2:
                    break
                else:
                    print("Passordene er ikke like!")
            try:
                self._write_user(username, password)
                print(f"Brukeren din {username} ble laget!")
                break
            except UserAlreadyExistsError:
                print(f"Brukeren med brukernavn {username} eksisterer allerede!")

    def main(self):
        self._running = True
        while self._running:
            if self._input("Har du en bruker fra før? (j/n) ").lower() == "j":
                self._login()
            else:
                self._signup()

    def _config_2fa(self):
        while True:
            print("""
Du kan:
- (s)ett opp 2fa
- (t)est 2fa
- (g)å tilbake
                """)
            action = self._input("Hva vil du gjøre? ")
            print()
            match action:
                case "s":
                    code = self._get_2fa(self._username)
                    print(f"Skriv inn den følgende koden i autentiserings-appen din: {code}")
                case "t":
                    if self._verify_2fa(self._username):
                        print("Koden er riktig!")
                    else:
                        print("Koden er feil!")
                case _:
                    break

    def _success(self):
        print(f"Du har blitt logget inn som {self._username}!")
        while True:
            print("""
Du kan:
- (l)ogg ut
- (s)topp programmet
- (a)lle brukere
- (t)ofaktor autentisering
            """)
            action = self._input("Hva vil du gjøre? ")
            print()
            match action:
                case "l":
                    # Stopp løkken og gå tilbake til main()
                    break
                case "s":
                    # Stopp løkken *og* fortelle hovedfunksjonen at programmet har stoppet
                    self._running = False
                    break
                case "a":
                    self._list_users()
                case "t":
                    self._config_2fa()
                case _:
                    print("Det er ikke et gyldig valg!")
                    continue


if __name__ == "__main__":
    try:
        login = Login()
        login.main()
    except (KeyboardInterrupt, EOFError):
        print()
        print(":(")

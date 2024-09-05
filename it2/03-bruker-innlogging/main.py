from getpass import getpass  # Innebygget i python
import bcrypt
from tinydb import TinyDB, Query

class UserDoesNotExistError(Exception):
    pass

class UserAlreadyExistsError(Exception):
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

    def user_exists(self, username):
        users = self._db.search(Query().username == username)
        return len(users) > 0

    def check_password(self, username, password):
        users = self._db.search(Query().username == username)
        if len(users) == 0:
            raise UserDoesNotExistError(f"The user {username} does not exist!")
        else:
            return bcrypt.checkpw(password.encode("utf-8"), users[0]["password"].encode("utf-8"))

    def write_user(self, username, password):
        if self.user_exists(username):
            raise UserAlreadyExistsError(f"The user {username} already exists!")
        self._db.insert({
            "type": "user",
            "username": username,
            # tinydb trenger en vanlig string, ikke bytes, mens bcrypt er motsatt.
            "password": bcrypt.hashpw(password.encode("utf-8"), bcrypt.gensalt()).decode("utf-8")
        })

    # Samme som over men bruker getpass
    def _input_password(self, prompt):
        _input = ""
        while True:
            _input = getpass(prompt).strip()  # Spørre bruker og fjerne ekstra mellomrom
            if len(_input) > 0:
                return _input
            print("Du må skrive inn noe!")

    def _login(self):
        print("Logger inn!")
        counter = 0
        while True:
            username = self._input("Brukernavnet ditt: ")
            password = self._input_password("Passordet ditt: ")
            try:
                if self.check_password(username, password):
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
                self.write_user(username, password)
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

    def _success(self):
        print(f"Du har blitt logget inn som {self._username}!")
        while True:
            print("""
Du kan:
- (l)ogg ut
- (s)topp programmet
            """)
            action = self._input("Hva vil du gjøre? ")
            match action:
                case "l":
                    # Stopp løkken og gå tilbake til main()
                    break
                case "s":
                    # Stopp løkken *og* fortelle hovedfunksjonen at programmet har stoppet
                    self._running = False
                    break
                case _:
                    print("Det er ikke et gyldig valg!")
                    continue


if __name__ == "__main__":
    login = Login()
    login.main()

import bcrypt  # Password hashing
from tinydb import TinyDB, Query  # DB
import pyotp  # 2fa


# Feilmeldinger
class UserDoesNotExistError(Exception):
    def __init__(self, username, message="User does not exist"):
        self.username = username
        self.message = f"{message}: {username}"
        super().__init__(self.message)


class DuplicateUserError(Exception):
    def __init__(self, username, message="Duplicate user"):
        self.username = username
        self.message = f"{message}: {username}"
        super().__init__(self.message)


class Users:
    def __init__(self, db_path):
        self._db = TinyDB(db_path)
        # https://tinydb.readthedocs.io/en/latest/usage.html#queries
        # brukes i alle database-operasjoner for å filtrere til riktig dokument
        self._user = Query()

    """
    Getters
    """

    def get_all_users(self):
        # Det skal egentlig kun være brukere i databasen, men det er bra å ha en sjekk for det
        return self._db.search(self._user.type == "user")

    def get_user(self, username):
        # få en liste over alle brukere med dette navnet
        users = self._db.search(self._user.username == username)
        # sjekke at det kun er en bruker, ellers sende en feilmelding
        if len(users) == 0:
            return None
        if len(users) > 1:
            # (dette skal egentlig aldri skje, siden koden skal sjekke det i self.add_user)
            raise DuplicateUserError(username)
        return users[0]

    def user_exists(self, username):
        return self.get_user(username) is not None

    def get_2fa(self, username):
        user = self.get_user(username)
        if user is None:
            raise UserDoesNotExistError(username)
        if user["2fa"] is not None:
            # returnere den eksisterende nøkkelen
            return user["2fa"]
        else:
            # generere en ny nøkkel og oppdatere databasen
            key = pyotp.random_base32()
            self._db.update({"2fa": key}, self._user.username == username)
            return key

    """
    Utility functions
    """

    def check_password(self, username, password):
        user = self.get_user(username)
        if user is None:  # Brukernavn er feil
            # raise UserDoesNotExistError(username)
            return False
        return bcrypt.checkpw(
            password.encode("utf-8"), user["password"].encode("utf-8")
        )

    def verify_2fa(self, username):
        # Antar at 2fa allerede er satt opp.
        # Dette bør sjekkes før funksjonen calles, eller lages en egen error for det
        input_code = ""
        while True:
            input_code = input("Skriv inn koden fra autentiserings-appen din: ")
            if len(input_code) == 6 and input_code.isdigit():
                break
            print("Koden er ugyldig!")
        totp = pyotp.TOTP(self.get_2fa(username))
        return totp.verify(input_code)

    """
    Setters
    """

    def add_user(self, username, password):
        if self.user_exists(username):
            return None
        id = self._db.insert(
            {
                "type": "user",
                "username": username,
                # tinydb trenger en vanlig string, ikke bytes, mens bcrypt er motsatt.
                "password": bcrypt.hashpw(
                    password.encode("utf-8"), bcrypt.gensalt()
                ).decode("utf-8"),
                "2fa": None,
            }
        )
        return self._db.get(doc_id=id)

    def remove_user(self, username):
        self._db.remove(self._user.username == username)

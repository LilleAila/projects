import random

SPECIAL_SYMBOLS = "#@&%$!"


# Klasse for å gjøre det enklere å jobbe med ordene
class BadWord:
    def _remove_special(self, word):
        """Gå gjennom symboler i en liste og fjerne dem"""
        for char in SPECIAL_SYMBOLS:
            word = word.replace(char, "")
        return word

    def trim(self, word):
        """Fjerne ekstra mellomrom, gjøre om til små bokstaver"""
        return self._remove_special(word.strip().lower())

    def __init__(self, word, replacement=None, real_words=[]):
        self.word = self.trim(word)
        self.replacement = self.trim(replacement)
        self._real_words = [word.strip() for word in real_words]

    def check(self, word):
        # Kunne overskrevet word, men den brukes nede for å sjekke direkte
        word_trimmed = self.trim(word)
        # Sjekke om ordet starter med dette ordet (altså vil både fis og fisen bli True)
        matches_word = word_trimmed.startswith(self.word)
        # Enten: ordet starter med noe av det som spesifikt er lov
        # Eller: ordet er helt likt (inkludert store/små bokstaver) av det som spesifikt er lov
        is_allowed = (
            any([word_trimmed.startswith(w) for w in self._real_words])
            or word.strip() in self._real_words
        )
        return matches_word and not is_allowed


def random_censor(word: str, bad_word: BadWord) -> str:
    # Splitte ordet på endelsen sin slik at man beholder endelsen når man sensurerer
    ending = word.split(bad_word.word)[1]
    return (
        "".join(
            [random.choice(SPECIAL_SYMBOLS) for _ in range(len(word) - len(ending))]
        )
        + ending
    )


def replacement_censor(word: str, bad_word: BadWord) -> str:
    # Samme som over, men erstatte ordet med en erstatning fra klassen i stedet for å bruke tilfeldige symboler
    ending = word.split(bad_word.word)[1]
    return bad_word.replacement + ending


# Her kan det legges til flere stygge ord når man vil.
BAD_WORDS = [
    BadWord("fis", "promp", ["FIS", "fiskal", "fisk"]),
    BadWord("idiot", "nisse"),
    BadWord("stygtord2", "nuh uh"),
    BadWord(
        "stygtord3",
        replacement="det var litt slemt sagt",
        real_words=["stygtord3(snilt)"],
    ),
    BadWord("fjomp", "tulling", ["fjompenisse"])
]


def censor_sentence(text, censor_method=random_censor):
    """Gå gjennom en setning og bytt ut stygge ord med en sensurert versjon"""
    words = text.split()  # Splitte etter ord
    # Dette blir tidskompleksitet O(n^2), fordi det er to løkker inni hverandre som sjekker ordet.
    # Det er det som er enklest for meg å skrive, men kunne bli gjort med O(n) dersom jeg hadde fokusert på fart heller enn å gjøre det enkelt for meg selv.
    # Dette er et valg jeg tok for å gjøre det enklere for meg selv heller enn at det er så raskt som mulig.
    new_sentence = []
    # Gå gjennom alle ordene
    for word in words:
        new_word = word
        # Sjekke hvert ord om det passer med hvert stygge ord
        for w in BAD_WORDS:
            if w.check(word):
                new_word = censor_method(w.trim(word), w)
                break
        new_sentence.append(new_word)  # Bygge en ny setning med de nye
    return " ".join(new_sentence)  # Sette inn mellomrom or returnere


TESTS = [
    "Den som fisen først er var, den er fisens rette far",
    "FIS globally governs skiing and snowboarding and oversees over 7000 events annually in Alpine, Cross-Country, Ski Jumping, Nordic Combined and many more.",
    "Fiskal blir benyttet om det som har med statskassen eller regnskap å gjøre, som for eksempel «fiskale avgifter» (statlige avgifter), eller «det fiskale året 2005» (regnskapsåret 2005). Fiskale skatter og avgifter har til formål å skaffe inntekter til staten.",
    "Oppskrifter med fisk og sjømat for alle, til middag, lunsj og fest. Lær om behandling av fisk og sjømat.",
    "Du er en idiot",
    "F##!@is",
    "denne setningen inneholder stygtord2, stygtord3 *og* stygtord3(snilt)",
]

# Slik at koden kun kjøres hvis jeg gjør `python3 __init__.py`, men ikke hvis noen vil importere det fantastiske biblioteket mitt med `import 01-sensurering`, eller `python3 -i __init__.py`
if __name__ == "__main__":
    for test in TESTS:
        print("Original setning:        ", test)
        print("Tilfeldig sensurert:     ", censor_sentence(test))
        print("Sensurert med erstatning:", censor_sentence(test, replacement_censor))
    print("\nNå kan du prøve noen setninger selv:\n")
    while True:
        try:
            user_sentence = input("Skriv inn en setning: ")
            print("Original setning:        ", user_sentence)
            print("Tilfeldig sensurert:     ", censor_sentence(user_sentence))
            print("Sensurert med erstatning:", censor_sentence(user_sentence, replacement_censor))
        except (KeyboardInterrupt, EOFError):
            print("\nneiiii :(")
            break

"""
Oppgave:
    - Grunnleggande oppgåve: Lag eit system som handterer innlogging. Ha eit fokus på korleis du kan sjekke kriterier på best måte (kva er "best"?). Brukaren må skrive inn brukarnavn og passord, og skal få tydelege feilmeldingar. NB: Særs forenkla når det gjeld lagring av brukarnavn og passord, kryptering mm. Denne oppgåva bør du absolutt diskutere og samanlikne med naboane, då det er mange moglege løysingar.
    - Utvida, og meir avansert oppgåve: Tenk sikkerheit.

Lisens: GNU General Public License v3.0

Skrevet av Olai Solsvik
"""

from src.login import Login
import os  # jobbe med paths

# Sjekke at programmet blir kjørt direkte. Hvis den blir importert som en module, skal den ikke kjøre koden.
if __name__ == "__main__":
    dir = os.path.dirname(__file__)
    db = os.path.join(dir, "db.json")  # /path/til/denne/filen/db.json
    login = Login()
    try:
        login.main()
    # Avslutte på en bedre måte når ^C eller ^D
    except (KeyboardInterrupt, EOFError):
        print()
        print(":(")

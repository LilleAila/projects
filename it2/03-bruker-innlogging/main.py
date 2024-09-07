from src.login import Login

# Sjekke at programmet blir kjørt direkte. Hvis den blir importert som en module, skal den ikke kjøre koden.
if __name__ == "__main__":
    try:
        login = Login()
        login.main()
    # Avslutte på en bedre måte når ^C eller ^D
    except (KeyboardInterrupt, EOFError):
        print()
        print(":(")

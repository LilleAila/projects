# Tidskompleksitet: O(n^2) - ifølge chatgpt
def sort(liste):
    """
    Recursive quicksort-algoritme
    Python er egentlig ikke laget for funksjonell programmering med rekursjo med rekursjonn, så denne koden vil ikke virke med veldig store lister
    Grensen er 1000 elementer i listen
    """
    if len(liste) <= 1:
        return liste
    xs = liste.pop(0)
    lower = [x for x in liste if x < xs]
    higher = [x for x in liste if x > xs]
    return sort(lower) + [xs] + sort(higher)


liste = [1, 6, 2, 5, 8, 3, 5, 9, 4, 0, 7]
print(sort(liste))

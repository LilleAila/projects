# Tidskompleksitet: O(n^2)
def quicksort(liste):
    """
    Recursive quicksort-algoritme
    Python er egentlig ikke laget for funksjonell programmering med rekursjo med rekursjonn, så denne koden vil ikke virke med veldig store lister
    """
    if len(liste) <= 1:
        return liste
    xs = liste.pop(0)
    lower = [x for x in liste if x < xs]
    higher = [x for x in liste if x > xs]
    return quicksort(lower) + [xs] + quicksort(higher)


liste = [1, 6, 2, 5, 8, 3, 5, 9, 4, 0, 7]
print(quicksort(liste))

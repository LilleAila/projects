if True:
    print("oi")
if not False:
    print("oi 2")
if False:
    print("nei")
if not False and len("abcd") == 4:
    print("hallo")
if (2 + 2 == 3 and True) or 3 >= 3:
    print("heihei")

tall1 = 2
tall2 = 4
if tall1 != tall2:
    if tall1 > tall2:
        print("tall1 er mer enn tall2")
    else:
        print("tall2 er størst")
else:
    print("de er like")

print("oida" if 9 + 10 == 21 else "neivel :(")

"""
| ==  | Lik              |
| !=  | Ulik             |
| <   | Mindre enn       |
| >   | Større enn       |
| <=  | Mindre eller lik |
| >=  | Større eller lik |
| and | Og               |
| Or  | Eller            |
| Not | Ikke             |
"""

from math import sqrt

x = sqrt(2.0)
y = x * x
if abs(y - 2.0) < 0.00001:
      print("floats er liksom ikke nøyaktig så man må gjøre sånn det")

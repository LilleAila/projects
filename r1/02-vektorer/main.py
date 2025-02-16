"""
Everything works, but might crash or give incorrect result with zero-vectors.
"""

from math import sqrt, cos

type Vector = tuple[float, float]
type Scalar = float


def length(u: Vector) -> Scalar:
    x, y = u
    return sqrt(x ** 2 + y ** 2)

def skalarprodukt(u: Vector, v: Vector) -> Scalar:
    (x1, y1), (x2, y2) = u, v
    return x1 * x2 + y1 * y2

def skalarprodukt2(u: Vector, v: Vector, alpha: Scalar) -> Scalar:
    return length(u) * length(v) * cos(alpha)

def orthogonal(u: Vector, v: Vector) -> bool:
    return skalarprodukt(u, v) == 0

def parallel(u: Vector, v: Vector) -> bool:
    return abs(skalarprodukt(u, v) / skalarprodukt2(u, v, 0)) == 1

def parallel2(u: Vector, v: Vector) -> bool:
    (x1, y1), (x2, y2) = u, v
    if (x1 == 0 and y1 == 0) or (x2 == 0 and y2 == 0):
        return False
    elif (x1 == 0 and x2 == 0) or (y1 == 0 and y2 == 0):
        return True
    try:
        return x2 / x1 == y2 / y1
    except ZeroDivisionError:
        return x1 / x2 == y1 / y2

u = (4, 1)
v =(-4, 16)

print(skalarprodukt(u, v))
print(skalarprodukt2(u, v, 90))
print(orthogonal(u, v))
print(parallel(u, v))
print(parallel2(u, v))

u = (4, 0)
v =(-8, 0)

print(skalarprodukt(u, v))
print(skalarprodukt2(u, v, 0))
print(orthogonal(u, v))
print(parallel(u, v))
print(parallel2(u, v))

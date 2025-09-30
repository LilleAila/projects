import sys
import functools

sys.set_int_max_str_digits(100000)

@functools.lru_cache()
def f(n):
    if n in {0, 1}:
        return 1
    return f(n - 1) + f(n - 2)

for i in range(10000):
    print(f(i))

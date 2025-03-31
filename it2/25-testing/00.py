# Debugger
import ipdb

# n - next line
# s - step (smaller increments)
# c - next preakpoint
# p <var> - print the value of the variable <var>
# q - quit


def add(a, b):
    return a + b


a = 123
b = 456

assert add(1, 2) == 3

ipdb.set_trace()  # Set breakpoint
assert add(1, 3) == 3
assert add(1, 2) == 3

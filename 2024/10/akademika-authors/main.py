#!/usr/bin/env python3
import os

names = os.popen("wl-paste").read().strip()

names = names.split()
full_names = []

buf = []
for name in names:
    if "," in name:
        if len(buf) > 0:
            full_names.append(buf)
            buf = []
        buf.append(name[:-1])
    else:
        buf.append(name)
full_names.append(buf)

full_names = [i[1:] + [i[0]] for i in full_names]

output = " & ".join([" ".join(i) for i in full_names])

os.system(f"wl-copy '{output}'")

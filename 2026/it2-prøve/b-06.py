temperaturer = [4.5, -2.0, 1.5, -6.5, 0.0, 3.0, -1.5, 7.0, 5.5, -3.5]

print(f"""
Antall målinger: {len(temperaturer)}
Første måling: {temperaturer[0]}
Siste måling: {temperaturer[-1]}
""".strip())

mean = sum(temperaturer) / len(temperaturer)
print(f"Gjennomsnitt: {mean:.1f}")

n_neg = 0
for t in temperaturer:
    n_neg += t < 0
print(f"Antall negativ: {n_neg}")

pos = []
for t in temperaturer:
    if t > 0:
        pos.append(t)
pos.sort(reverse=True)
print(pos)

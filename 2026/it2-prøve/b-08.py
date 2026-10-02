from typing import Tuple


def konverter_tid(sekunder: int) -> Tuple[int, int, int]:
    minutter = sekunder // 60
    sekunder %= 60

    timer = minutter // 60
    minutter %= 60

    return timer, minutter, sekunder

timer, minutter, sekunder = konverter_tid(4000)
print(f"{timer} t, {minutter} min, {sekunder} s")

def er_sterkt_passord(passord: str) -> bool:
    return (
        len(passord) >= 8
        and any(i.isdigit() for i in passord)
        and any(i.isupper() for i in passord)
    )

print(f"""
"hemmelig": {er_sterkt_passord("hemmelig")}
"Hemmelig": {er_sterkt_passord("Hemmelig")}
"Hemmelig1": {er_sterkt_passord("Hemmelig1")}
""".strip())

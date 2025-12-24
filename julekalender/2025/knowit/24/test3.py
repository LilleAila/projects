def shift_bits(data: bytes, shift: int) -> bytes:
    """Left-shift a byte stream by `shift` bits"""
    out = bytearray()
    carry = 0
    for b in data:
        new = ((b << shift) & 0xff) | carry
        carry = (b >> (8 - shift)) & ((1 << shift) - 1)
        out.append(new)
    return bytes(out)

with open("key_xor.bin", "rb") as f:
    data = f.read()

for shift in range(8):
    shifted = shift_bits(data, shift)
    printable = shifted[:64].decode("ascii", errors="ignore")
    print(f"\n--- SHIFT {shift} ---")
    print(printable)

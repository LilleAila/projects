from PIL import Image

def extract_bits(img_path, channel="r", bit=4):
    img = Image.open(img_path).convert("RGBA")
    pixels = img.load()

    channel_index = {"r": 0, "g": 1, "b": 2, "a": 3}[channel]

    bits = []

    for y in range(img.height):
        for x in range(img.width):
            value = pixels[x, y][channel_index]
            bits.append((value >> bit) & 1)

    return bits


def bits_to_bytes(bits):
    out = bytearray()
    for i in range(0, len(bits), 8):
        byte = bits[i:i+8]
        if len(byte) < 8:
            break
        out.append(int("".join(str(b) for b in byte), 2))
    return out


# --- extract from both images ---
bits_main = extract_bits("juletreet.png", channel="r", bit=4)
bits_hint = extract_bits("nissens-hint.png", channel="r", bit=4)

# ensure same length
length = min(len(bits_main), len(bits_hint))
bits_main = bits_main[:length]
bits_hint = bits_hint[:length]

# --- XOR bits ---
xor_bits = [b1 ^ b2 for b1, b2 in zip(bits_main, bits_hint)]

# --- convert to bytes ---
data = bits_to_bytes(xor_bits)

# --- print preview ---
print(data[:300])
print("\nAs text (errors ignored):\n")
print(data[:300].decode("ascii", errors="ignore"))

with open("xor.bin", "wb") as f:
    f.write(data)

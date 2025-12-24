from pathlib import Path

def bytes_to_bits(data, bitplane):
    """
    Convert a byte sequence to a list of bits from the given bitplane.
    bitplane = 0 (LSB) .. 7 (MSB)
    """
    bits = []
    for b in data:
        bits.append((b >> bitplane) & 1)
    return bits

def bits_to_bytes_plane(bits, plane_weight=0, reverse_bits=False):
    """
    Convert a list of bits into bytes, shifting each byte to the correct plane weight.
    reverse_bits: reverse each group of 8 bits
    plane_weight: shift each byte left by this many bits to align planes
    """
    out = bytearray()
    for i in range(0, len(bits), 8):
        byte_bits = bits[i:i+8]
        if len(byte_bits) < 8:
            break
        if reverse_bits:
            byte_bits = byte_bits[::-1]
        val = 0
        for bit in byte_bits:
            val = (val << 1) | bit
        val <<= plane_weight  # align the plane
        out.append(val)
    return out

def read_raw_file(path):
    with open(path, "rb") as f:
        return f.read()

def write_bytes(path, data):
    with open(path, "wb") as f:
        f.write(data)

# --- CONFIG ---
key1_file = "key1.bin"        # from b1,r,lsb,xy
key2_file = "key2.bin"        # from b3,g,msb,xy
output_file = "key_xor_aligned.bin"

bitplane_key1 = 0  # zsteg extraction plane for key1 (LSB plane)
bitplane_key2 = 7  # zsteg extraction plane for key2 (MSB plane)
reverse_bits = True  # reverse bits per byte if necessary

# --- load raw bytes ---
data1 = read_raw_file(key1_file)
data2 = read_raw_file(key2_file)

# truncate to same length
length = min(len(data1), len(data2))
data1 = data1[:length]
data2 = data2[:length]


data1 = read_raw_file(key1_file)
data2 = read_raw_file(key2_file)

length = min(len(data1), len(data2))
data1 = data1[:length]
data2 = data2[:length]

xor_bytes = bytearray(a ^ b for a, b in zip(data1, data2))
write_bytes(output_file, xor_bytes)

# # --- extract bits from specified bitplanes ---
# bits1 = bytes_to_bits(data1, bitplane_key1)
# bits2 = bytes_to_bits(data2, bitplane_key2)
#
# # --- convert bits back to bytes with proper plane alignment ---
# # key1 is LSB → plane_weight=0
# # key2 is MSB → plane_weight=7
# bytes1_aligned = bits_to_bytes_plane(bits1, plane_weight=0, reverse_bits=reverse_bits)
# bytes2_aligned = bits_to_bytes_plane(bits2, plane_weight=0, reverse_bits=reverse_bits)  # plane_weight=0, already shifted
#
# # --- XOR the aligned bytes ---
# xor_bytes = bytearray(a ^ b for a, b in zip(bytes1_aligned, bytes2_aligned))
#
# # --- save to file ---
# write_bytes(output_file, xor_bytes)
# print(f"Written aligned XOR result to {output_file}")
# print("Try: gpg --list-packets", output_file)
# print("Or check ASCII fragments with: strings", output_file)

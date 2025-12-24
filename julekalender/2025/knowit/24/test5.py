from PIL import Image
import zlib
import sys

def solve_santa_stego(image_path):
    print(f"Opening {image_path}...")
    try:
        img = Image.open(image_path)
        pixels = img.load()
        width, height = img.size
    except Exception as e:
        print(f"Error opening image: {e}")
        return

    binary_string = ""
    
    # 1. Extract LSB from Red Channel
    for y in range(height):
        for x in range(width):
            r = img.getpixel((x, y))[0]
            binary_string += str(r & 1)

    # 2. Convert bits to Bytes
    data_bytes = bytearray()
    for i in range(0, len(binary_string), 8):
        byte = binary_string[i:i+8]
        if len(byte) == 8:
            data_bytes.append(int(byte, 2))

    print(f"Extracted {len(data_bytes)} bytes.")

    # 3. Attempt Zlib Decompression
    try:
        # We try to decompress the whole block
        decompressed_data = zlib.decompress(data_bytes)
        print("\n--- SUCCESS! DECOMPRESSED DATA ---")
        print(decompressed_data.decode('utf-8', errors='ignore'))
        return
    except zlib.error:
        print("\nDirect decompression failed. Scanning for Zlib header (0x78)...")

    # 4. Brute-force scan for the Zlib header
    # Sometimes there is junk data at the start. We look for 'x' (0x78) 
    # and try to decompress from that point.
    found = False
    for i in range(len(data_bytes)):
        if data_bytes[i] == 0x78: # 0x78 is the start of Zlib
            try:
                # Try decompressing from this point
                payload = data_bytes[i:]
                decompressed_data = zlib.decompress(payload)
                print(f"\n--- SUCCESS! Found Zlib stream at offset {i} ---")
                print(decompressed_data.decode('utf-8', errors='ignore'))
                found = True
                break
            except:
                continue
    
    if not found:
        print("Could not decompress data. It might not be Zlib or the bit extraction order is reversed.")

# Run it
solve_santa_stego('juletreet.png')

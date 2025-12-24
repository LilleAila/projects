from PIL import Image
import sys

def reverse_bits(byte_val):
    # Helper to reverse bits in a byte (0b10110000 -> 0b00001101)
    return int('{:08b}'.format(byte_val)[::-1], 2)

def solve_santa_raw(image_path):
    print(f"Opening {image_path}...")
    try:
        img = Image.open(image_path)
        width, height = img.size
    except Exception as e:
        print(f"Error: {e}")
        return

    print("Extracting bits: Column-Major (Left->Right), Bottom->Top...")
    
    binary_string = ""
    
    # Scan: X from 0 to Width, Y from Height-1 to 0
    for x in range(width):
        for y in range(height - 1, -1, -1):
            r = img.getpixel((x, y))[0]
            binary_string += str(r & 1)

    print(f"Extracted {len(binary_string)} bits.")
    
    # Convert to bytes
    data_bytes = bytearray()
    for i in range(0, len(binary_string), 8):
        byte_str = binary_string[i:i+8]
        if len(byte_str) == 8:
            data_bytes.append(int(byte_str, 2))

    # --- ANALYSIS ---
    print("\n--- ATTEMPT 1: Raw Output (First 200 chars) ---")
    # Clean up non-printable chars for display
    raw_text = data_bytes[:200].decode('latin-1', errors='replace')
    print(f"preview: {raw_text}")
    if "FLAG" in raw_text or "knowit" in raw_text.lower():
        print(">>> FOUND POSSIBLE FLAG IN RAW OUTPUT <<<")

    print("\n--- ATTEMPT 2: Bit Reversal (First 200 chars) ---")
    # Try reversing the bit order of every byte
    reversed_bytes = bytearray([reverse_bits(b) for b in data_bytes[:200]])
    rev_text = reversed_bytes.decode('latin-1', errors='replace')
    print(f"preview: {rev_text}")
    if "FLAG" in rev_text or "knowit" in rev_text.lower():
        print(">>> FOUND POSSIBLE FLAG IN BIT-REVERSED OUTPUT <<<")

    print("\n--- ATTEMPT 3: Inverted Bits (First 200 chars) ---")
    # Try flipping 0s to 1s (XOR with 0xFF)
    inverted_bytes = bytearray([b ^ 0xFF for b in data_bytes[:200]])
    inv_text = inverted_bytes.decode('latin-1', errors='replace')
    print(f"preview: {inv_text}")
    
    # Save the full raw data to a file just in case it's a file header we don't recognize
    with open('extracted_bottom_up.bin', 'wb') as f:
        f.write(data_bytes)
    print("\nFull extracted data saved to 'extracted_bottom_up.bin'.")

solve_santa_raw('juletreet.png')

from PIL import Image

# The 9 "Red Ball" Candidates (Sorted Bottom-to-Top)
candidates = [
    (69, 906),
    (274, 900),
    (671, 812),
    (821, 717),
    (486, 586),
    (675, 478),
    (215, 458),
    (395, 339),
    (562, 292)
]

def solve_santa_final(image_path):
    print(f"Opening {image_path}...")
    try:
        img = Image.open(image_path)
    except Exception as e:
        print(f"Error: {e}")
        return

    print("\nScanning the 9 Candidates (Bottom-Up)...")
    print(f"{'Idx':<3} | {'Coord':<10} | {'R':<3} {'G':<3} {'B':<3} | {'Chr(B)':<6} {'Chr(G)':<6}")
    print("-" * 60)

    blue_chars = []
    green_chars = []
    
    for i, (x, y) in enumerate(candidates):
        try:
            r, g, b = img.getpixel((x, y))[:3]
            
            # Convert to ASCII if possible
            cb = chr(b) if 32 <= b <= 126 else '?'
            cg = chr(g) if 32 <= g <= 126 else '?'
            
            blue_chars.append(cb)
            green_chars.append(cg)
            
            print(f"{i+1:<3} | ({x:<3}, {y:<3}) | {r:<3} {g:<3} {b:<3} | {cb:<6} {cg:<6}")
        except Exception as e:
            print(f"{i+1:<3} | ({x:<3}, {y:<3}) | ERROR")

    print("-" * 60)
    print("Possibility A (Blue Channel - All 9):  " + "".join(blue_chars))
    print("Possibility B (Green Channel - All 9): " + "".join(green_chars))

    print("\n--- TRYING SUBSETS OF 8 ---")
    # We have 9 pixels, we need 8. Let's try removing one at a time to see if a word appears.
    for i in range(9):
        subset = blue_chars[:i] + blue_chars[i+1:]
        word = "".join(subset)
        if word.isalpha():
             print(f"Removing item {i+1}: {word} (Valid Letters!)")
        else:
             print(f"Removing item {i+1}: {word}")

solve_santa_final('juletreet.png')

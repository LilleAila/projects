from PIL import Image

def solve():
    print("Loading image...")
    try:
        img = Image.open("juletreet.png").convert("RGB")
        pixels = img.load()
        width, height = img.size
    except Exception as e:
        print(f"Error: {e}")
        return

    # 1. FIND BAUBLES (Standard Rudolph Red)
    def is_red(r, g, b):
        return (r > 100) and (r > g + 30) and (r > b + 30)

    visited = set()
    baubles = []

    print("Scanning for baubles...")
    for y in range(height):
        for x in range(width):
            if (x, y) in visited:
                continue
            
            r, g, b = pixels[x, y]
            if is_red(r, g, b):
                # Flood fill to find blob
                q = [(x, y)]
                visited.add((x, y))
                blob_pixels = []
                while q:
                    cx, cy = q.pop()
                    blob_pixels.append((cx, cy))
                    for dx, dy in [(-1,0), (1,0), (0,-1), (0,1)]:
                        nx, ny = cx+dx, cy+dy
                        if 0<=nx<width and 0<=ny<height and (nx, ny) not in visited:
                            nr, ng, nb = pixels[nx, ny]
                            if is_red(nr, ng, nb):
                                visited.add((nx, ny))
                                q.append((nx, ny))
                
                # If valid bauble
                if len(blob_pixels) > 10:
                    # CALCULATE PRECISE CENTER
                    # We want the geometric center to ensure the neighbors are also red
                    avg_x = sum(p[0] for p in blob_pixels) // len(blob_pixels)
                    avg_y = sum(p[1] for p in blob_pixels) // len(blob_pixels)
                    baubles.append((avg_x, avg_y))

    print(f"Found {len(baubles)} valid baubles.")

    # 2. SORT BAUBLES (Global Order)
    # Scroll: "Bunn-til-topp" (Y descending), then "Venstre-høyre" (X ascending)
    baubles.sort(key=lambda p: (-p[1], p[0]))

    # 3. DECODE NEIGHBORS (The "8 Pixels" around the center)
    print("-" * 40)
    print("DECODING...")

    # We will try all 3 channels
    for channel_name, c_idx in [("Red", 0), ("Green", 1), ("Blue", 2)]:
        result = ""
        
        for (cx, cy) in baubles:
            # Define the 8 neighbors "Bottom-to-Top, Left-to-Right"
            # Bottom Row (y+1)
            neighbors = [
                (cx-1, cy+1), (cx, cy+1), (cx+1, cy+1),
                (cx-1, cy),               (cx+1, cy),   # Middle Row (skip center)
                (cx-1, cy-1), (cx, cy-1), (cx+1, cy-1)  # Top Row (y-1)
            ]
            
            bits = ""
            for nx, ny in neighbors:
                if 0 <= nx < width and 0 <= ny < height:
                    val = pixels[nx, ny][c_idx]
                    bits += str(val & 1)
                else:
                    bits += "0" # Out of bounds default
            
            # Try Standard ASCII (MSB first)
            try:
                char_code = int(bits, 2)
                result += chr(char_code)
            except:
                result += "?"
        
        print(f"[{channel_name}]: {result}")
        
    print("-" * 40)

if __name__ == "__main__":
    solve()

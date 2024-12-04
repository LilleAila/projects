import matplotlib.pyplot as plt
import numpy as np
from PIL import Image

img = Image.open("input.png")
data = np.array(img)

# XOR the color planes, AND with 00000001 to get only last bit
extracted = (data[..., 0] ^ data[..., 1] ^ data[..., 2]) & 0x01

plt.imshow(extracted)
plt.show()

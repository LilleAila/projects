import cv2

carrier = cv2.imread("carrier.png")
secret = cv2.imread("secret.png", cv2.IMREAD_GRAYSCALE)

assert carrier is not None and secret is not None

_, binary_secret = cv2.threshold(secret, 127, 1, cv2.THRESH_BINARY)
carrier[:, :, 2] &= 0b11111110
carrier[:, :, 2] |= binary_secret

cv2.imwrite("output.png", carrier)

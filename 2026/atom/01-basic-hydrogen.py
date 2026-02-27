import numpy as np
import matplotlib.pyplot as plt
from scipy.special import sph_harm_y, genlaguerre, factorial

n, l, m = 4, 2, 0 # Quantum numbers
a0 = 1.0 # Bohr radius (only for visualization)
threshold = 0.05

grid_resolution = 50
grid_size = 30

x = np.linspace(-grid_size*a0, grid_size*a0, grid_resolution)
y = np.linspace(-grid_size*a0, grid_size*a0, grid_resolution)
z = np.linspace(-grid_size*a0, grid_size*a0, grid_resolution)
X, Y, Z = np.meshgrid(x, y, z)

# We have that psi(r, theta, psi) = R(r) * Y(theta, psi)
# where R(r) depends on the distance to the nucleus from the electron
# and Y(theta, psi) depends on the direction of the electron
# Thus, we can simplify by splitting to the two componends
# and using spherical coordinates rather than cartesian

R = np.sqrt(X**2 + Y**2 + Z**2)
Theta = np.arccos(np.clip(Z/R, -1, 1)) # polar angle
Phi = np.arctan2(Y, X) # azimuthal angle

# The radial part. I don"t understand all of this lol
rho = 2 * R / (n * a0)
L = genlaguerre(n-l-1, 2*l+1)(rho)
norm_r = np.sqrt((2/(n*a0))**3 * factorial(n-l-1)/(2*n*factorial(n+l)))
Rnl = norm_r * np.exp(-rho/2) * rho**l * L

# The angular part completely uses an implementation directly from scipy
Ylm = sph_harm_y(l, m, Theta, Phi)

# As described earlier, we multiply the radial and angular functions to get psi,
# the wave function. Then, the probability density is given by |psi|^2
psi = Rnl * Ylm
prob_density = np.abs(psi)**2

# Only plot the points within a certain threshold
mask = prob_density > threshold * prob_density.max()


# Plot the points with probability density visualized with color
fig = plt.figure(figsize=(8,8))
ax = fig.add_subplot(111, projection="3d")
ax.scatter(X[mask], Y[mask], Z[mask], c=prob_density[mask], cmap="viridis", alpha=0.4, s=20)
ax.set_xlabel("x"); ax.set_ylabel("y"); ax.set_zlabel("z")
ax.set_title(f"Hydrogen Orbital n={n}, l={l}, m={m}")
plt.show()

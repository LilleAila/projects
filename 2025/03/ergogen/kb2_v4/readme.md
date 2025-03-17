Simple steno keyboard with 6x2 on left and right, and 2x1 thumb clusters.

## TODO

- [x] Better position the reset button on the PCB
- [x] Route PCB
- [x] Make case with proper cutouts for microcontroller, etc.
- [x] Find a way to mount the PCB to the case
- [x] Add more spacing between the case edge and the PCB
- [ ] Test case with actual screws and inserts (with mock PCB)
- [x] Move the standoffs such that there are two right next to each other so that the case can be split down the middle for printing

## BOM

Best to buy from [VOEC-registered](https://www.skatteetaten.no/person/avgifter/kjop-fra-utlandet/nettbutikker-og-e-markedsplasser-som-er-registrert-i-voec-registeret/) stores, to avoid paying extra for import.

- Both JLCPCB and 42keebs are registered

Total price for PCB + Parts + shipping is around 60€
Key switches and keycaps under 50€ if 3d printed

### PCB Fabrication

Using [JCLPCB](https://jlcpcb.com), which has very competitive pricing.
Using all default settings, with gerber files from kicad

### Microcontroller

[Frood RP2040](https://42keebs.eu/shop/parts/controllers/frood-rp2040-pro-micro-controller/)

RP2040-based pro-micro compatible.

> [!NOTE]
> Using pro micro footprint in ergogen. Works the same, except the five bottom pins are not present. Has no effect for this project.

- Black
- With standard header pins
- With `Medium Profile Sockets + Pins (30) [Frood]`

[Socketed microcontroller](https://42keebs.eu/build-guides/socketing-the-frood-nice-nano-controllers/)

### Diodes

[1N4148 Diodes](https://42keebs.eu/shop/parts/components/1n4148-diodes-through-hole-smd/?attribute_type=SMD)

- 3x10 (keyboard has 28 keys)
- SMD

### Sockets

[Kailh Choc V1 Hot-swap Sockets](https://42keebs.eu/shop/parts/kailh-choc-hot-swap-sockets/)

- 3x10 (same as diodes)

### Key switches

[Choc pink pro](https://www.maxgaming.no/no/switchar/choc-low-profile-pink-pro)

20 gram actuation (lowest available)
Bought in Norway, per switch
Expensive :(

### Reset switch

[Reset switch](https://42keebs.eu/shop/parts/components/reset-switch/)

> [!NOTE]
> Might remove in the future, in favor of the on-board switch?
> Redundancy is not bad though

- SMD 4-pin Flat
- Silver

### Keycaps

3D-printed custom-made choc V1 keycaps

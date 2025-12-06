Simple steno keyboard with 6x2 on left and right, and 2x1 thumb clusters.

## TODO

- [x] Better position the reset button on the PCB
- [x] Route PCB
- [x] Make case with proper cutouts for microcontroller, etc.
- [x] Find a way to mount the PCB to the case
- [x] Add more spacing between the case edge and the PCB
- [x] Test case with actual screws and inserts (with mock PCB)
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

- Black
- With standard header pins
- With `Medium Profile Sockets + Pins (30) [Frood]`

[Microcontroller socket guide](https://42keebs.eu/build-guides/socketing-the-frood-nice-nano-controllers/) ([alt guide](https://docs.splitkb.com/product-guides/aurora-series/build-guide/microcontrollers))

### Diodes

[1N4148 Diodes](https://42keebs.eu/shop/parts/components/1n4148-diodes-through-hole-smd/?attribute_type=SMD)

- 3x10 (keyboard has 28 keys)
- SMD

[Diode soldering guide](https://docs.splitkb.com/product-guides/aurora-series/build-guide/diodes#smd-diodes)

Also see [soldering joints](https://docs.splitkb.com/resources/soldering/healthy-joints#reworking-a-joint) and [iron temperature](https://docs.splitkb.com/resources/soldering/dialling-in-your-iron)

### Sockets

[Kailh Choc V1 Hot-swap Sockets](https://42keebs.eu/shop/parts/kailh-choc-hot-swap-sockets/)

- 3x10 (same as diodes)

[Socket soldering guide](https://docs.splitkb.com/product-guides/aurora-series/build-guide/switch-sockets)

### Key switches

Any choc v1 compatible switches will work.

[Choc pink pro](https://www.maxgaming.no/no/switchar/choc-low-profile-pink-pro) - 20 gram actuation (lowest available)

### Reset switch

[Reset switch](https://42keebs.eu/shop/parts/components/reset-switch/)

- SMD 4-pin Flat
- Silver

### Keycaps

3D-printed custom-made choc V1 keycaps

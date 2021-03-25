# PiMesh INFO

## Keyboard

The Pincode is entered via a keypad (sometimes) the suggested
pincodes are typically 4 or 6 digits.

There are 3 prototype keyboards

- one keyboard 3x3 [1-9] on the TCA8418E-EVM (evaluation board)
- one BIG old style (telefon) keypad 4x3  [0-9#\*]
- one MEDIUM style (telefon) keypad 4x3  [0-9#\*]

Pinlayout on EVM board

    C9 C8 C7 C6 C5 C4 C3 C2 C1 C0
    G  V  R7 R6 R5 R4 R3 R2 R1 R0

The BIG keyboard 8-pin layout

    G  C1 C0 R3 C2 R2 R1 R0

For the EVM do not connect the ground (G) pin
(may be used for PULLUP for eached Ri and  Cj)

The SMALL keyboard 7-pin layout

    R2 R1 C0 R0 C2 R3 C1

## LED & PIN-ENTRY

The pin entry process is guided only with LEDS,

Example

- Pin code is needed, RED-BLINK, GREEN-OFF
- Key is pressed FLASH GREEN for each PRESS/RELEASE
- Pin code is accepted GREEN-ON, RED-OFF, device enabled
- When device is enabled a sequence of with # and * will
disable the device again.
- If key presses are delayed too long the current code
will be cleared and the process restarted (no back-off penalty needed?)

Pin code could be accepted either when the correct digits
have been pressed regardless how many key have been pressed.
Or the # key is used to enter the pincode. If keyboard is
missing a key for enter (#) then the first option may be
needed.
Enter a bad pin code will set RED-ON and run a exponetial back-off
delay until a new pin code may be tried.
Pin codes should be digested and matched against a hash.
This way the clear text pin code does not have to be stored in the 
key server.

## LED & BATTERY 

We could use the keyboard (using TCA8418) control 
4 LEDs for the battery level and.
if the SOC (state of charge) is present this could be used
to show the status of the battery level.

## LED & APPLICAION

Also on one LED could be used to show when the application is
running.This is helpful when problem arise in logging in
to the web UI etc. If the application LED is not ON, then there
should not be expected that loggin in would be succesful.

May be some kind of watch dog could try to turn the led OFF.

## Raspberry pi Z pin header 

First part (used part) of the pin header

    3.3v Power  1   2  5v Power
    SDA         3   4  5v Power
    SCL         5   6  GND
    GPIO4(CLK)  7   8  GPIO 14 (Tx)
    GND         9  10  GPIO 15 (Rx)
    GPIO 17    11  12  GPIO 18 (PWM/PCM CLK)
    GPIO 27    13  14  GND
    -          15  16  -
    3.3v Power 17  18  -

## The TCA Lab card colors (temporary)

    1  RED    (3.3v)  TCA
    3  YELLOW (I2C SDA)
    5  GRAY   (I2C SCL)
    7  -                  8 - Tx  (GPS module)
    9  GND               10 - RX  (GPS module)
    11 BROWN  (INT)
    13 WHITE  (RESET)
    15
    17 RED    (3.3v) GPS-module

## The MixMesh mini keyboard (1.3)

	4 - SCL
	6 - SDA
	8 - RESET
	2 - INT
	7 - COL8
	9 - COL9
	5 - PWM
	1 - VCC
	10 - GND
	
## MixMesh keyboard connection to raspberry pi Z

	Keyboard       Raspberry PI Z (pin header)
	--------
	1  - VCC       1    3.3v
	2  - INT       11   (GPIO 17)
	3  - 	       -
	4  - SCL       5    (GPIO 3, I2C SCL)
	5  - PWM       12   (GPIO 18, PWM/PCM CLK)
	6  - SDA       3    (GPIO 2, I2C SDA)
	7  - COL8	   -
	8  - RESET     13   (GPIO 27)
	9  - COL9      -
	10 - GND       9    (GND)

## MixMesh keyboard dimensions

Circuit board thickness 0.95 mm
width: 27.97 mm
height: 46.01 mm

Button height: 1.35mm (pressed) 1.5mm (release)
Platsic cover: 1.4mm?
Height at tca8418: 1.8mm


Component sizes:

	S1,..., S12  radius 3,550      (buttons)
	C1,C2
	R1, ..., R41   w=1.520, h=0.850  (resistors)
	D1, ..., D7    w=0.850, h=1.520  (LED)
	D8             w=3.200, h=1.200  (2-LED)
	IC1            w=3.500, h=3.500  (TCA8418)

	Connector: w=
	  pin1 x=11.500, y=15.170
	  pin2 x=11.500, y=13.900
	  pin9 x=16.580, y=15.170
	  pin9 x=16.580, y=13.900 

	

# PiMesh INFO

## Keyboard

The Pincode is entered via a keypad (sometimes) the suggested
pincodes are typically 4 or 6 digits.

There are 3 prototype keybaords

- one keyboard 3x3 [1-9] on the TCA8418E-EVM (evaluation board)
- one BIG old style (telefon) keypad 4x3  [0-9#\*]
- one MEDIUM style (telefon) keypad 4x3  [0-9#\*]

Pinlayout on EVM baord

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

3x3 Power   1   2  5v Power
SDA         3   4  5v Power
SCL         5   6  GND
GPIO4(CLK)  7   8  GPIO 14 (Tx)
GND         9  10  GPIO 15 (Rx)
GPIO 17    11  12  GPIO 18 (PCM CLK)
GPIO 27    13  14  GND
-          15  16  -
3x3 Power  17  18  -

The TCA Lab card colors (temporary)

1  RED    (3.3v)  TCA
3  YELLOW (I2C SDA)
5  GRAY   (I2C SCL)
7  -
9  GND             10 - RX  (GPS module)
11 BROWN  (INT)
13 WHITE  (RESET)
15
17 RED    (3.3v) GPS-module

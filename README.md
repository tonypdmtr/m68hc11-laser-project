# m68hc11-laser-project
This project is about an assembly language application that uses a laser.

The system toggles between four modes to display characters input from a keyboard.

* Mode 1 puts characters on an LCD (Liquid Crystal Display) but also the LCD always displays characters upon an interrupt.
* Mode 2 puts characters on the LCD and through the laser display.
* Mode 3 outputs a test signal through the laser (a box with an "X" through it).
* Mode 4 displays a spiral animation through the laser.

(Assembles with latest [ASM11](http:www.aspisys.com/asm11.htm) assembler)

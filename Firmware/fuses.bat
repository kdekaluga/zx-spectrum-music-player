@echo off

rem Fuses:
rem   Clock source = 16 MHz from internal PLL
c:\programming\avrdude\avrdude.exe -c usbasp -p t85 -B 100kHz -U efuse:w:0xFF:m -U hfuse:w:0xDF:m -U lfuse:w:0xE1:m
rem fuses: E:FF, H:D9, L:FF

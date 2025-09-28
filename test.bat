cls
@echo off
rem c:\programming\avrdude\avrdude.exe -c usbasp -p m328p -v -B 10MHz
rem C:\Users\SD\.platformio\packages\tool-avrdude\avrdude.exe -c usbasp -p t85 -v -B 100kHz
C:\Users\SD\.platformio\packages\tool-avrdude\avrdude.exe -c usbasp -p t85 -v -B 10MHz
rem fuses: E:FF, H:D9, L:FF

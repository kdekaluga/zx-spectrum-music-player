import sys

if len(sys.argv) < 2:
	print("Provide an STC binary file name!")
	exit(1)

fileName = sys.argv[1]

with open(fileName, mode='rb') as file:
	binData = file.read()

print(
	"#pragma once\r\n"
	"\r\n"
	F"// Original name: {fileName}\r\n"
	F"// Original size: {len(binData)} bytes\r\n"
	"static const uint8_t PROGMEM melody_data[] =\r\n"
	"{\r\n"
	"    ",
	end="")

byteNum = -1;
for byte in binData:
	byteNum += 1;
	if byteNum == 16:
		byteNum = 0;
		print("\r\n    ", end="")

	print(F"0x{byte:0{2}X}, ", end="");

print("\r\n};")

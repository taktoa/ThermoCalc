#!/bin/bash
rm -f test.png
./bin/ThermoCalc.bin +RTS -K80000000 -RTS -o test.png -w 4096
mogrify -background "#FFFFFF" -flatten test.png
feh test.png
rm -f test.png

#!/bin/bash
./ThermoCalc.bin +RTS -K80000000 -RTS -o test.png -w 600
feh test.png
rm test.png

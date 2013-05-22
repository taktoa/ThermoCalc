all: ThermoCalc.hs
	ghc -O3 --make ThermoCalc.hs -o ThermoCalc.bin && make clean && strip ThermoCalc

clean:
	rm -f *.hi *.o

clean-all:
	rm -f ThermoCalc *.hi *.o *.dat
	    

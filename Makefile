OUTPUT_EXEC = "bin/ThermoCalc.bin"

all: src/ThermoCalc.hs
	cd src && ghc -O2 -rtsopts -fllvm --make ThermoCalc.hs -o ../$(OUTPUT_EXEC) && cd .. && strip $(OUTPUT_EXEC)

clean:
	rm -f src/*.hi src/*.o

clean-all:
	rm -f $(OUTPUT_EXEC) src/*.hi src/*.o
	    

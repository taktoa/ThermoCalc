OUTPUT_EXEC = "bin/ThermoCalc.bin"

all: src/ThermoCalc.hs
	cd src && ghc -O3 -rtsopts --make ThermoCalc.hs -o ../$(OUTPUT_EXEC) && cd .. && make clean && strip $(OUTPUT_EXEC)

clean:
	rm -f src/*.hi src/*.o

clean-all:
	rm -f $(OUTPUT_EXEC) src/*.hi src/*.o
	    

OUTPUT_EXEC = "ThermoCalc.bin"

all: ThermoCalc.hs
	ghc -O3 --make ThermoCalc.hs -o $(OUTPUT_EXEC) && make clean && strip $(OUTPUT_EXEC)

clean:
	rm -f *.hi *.o

clean-all:
	rm -f $(OUTPUT_EXEC) *.hi *.o *.dat
	    

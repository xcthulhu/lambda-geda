PROGS = parse-test parse-print-test geda-sexpress flatten-hierarchy vhdl-to-sym
MODULES = Geda/*.hs

all: $(PROGS) 

%: %.hs $(MODULES)
	ghc --make $< -o $@

clean:
	rm -f *.o *.hi *~ Geda/*.o Geda/*.hi Geda/*~ Language/VHDL/*.o Language/VHDL/*.hi Language/VHDL/*~  $(PROGS) 
	rm -rf tests/SXI/flatten

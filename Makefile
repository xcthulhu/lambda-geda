PROGS = parse-test parse-print-test geda-sexpress flatten-hierarchy
MODULES = Geda/*.hs

all: $(PROGS) 

%: %.hs $(MODULES)
	ghc --make $< -o $@

clean:
	rm -f *.o *.hi *~ Geda/*.o Geda/*.hi Geda/*~ parsec-2.1.0.1/*.o parsec-2.1.0.1/*.hi $(PROGS) 
	rm -rf tests/SXI/flatten

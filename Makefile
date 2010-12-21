PROGS = parse-test parse-print-test geda-sexpress flatten-hierarchy
MODULES = Geda/*.hs

all: $(PROGS) 

%: %.hs $(MODULES)
	ghc --make $< -o $@

clean:
	rm -f *.o *.hi *~ Geda/*.o Geda/*.hi Geda/*~ $(PROGS) 
	rm -rf tests/SXI/flatten

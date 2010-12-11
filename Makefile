PROGS = parse-test parse-print-test geda-sexpress
MODULES = Gaf.hs Gaf/*.hs

all: $(PROGS) 

%: %.hs $(MODULES)
	ghc --make $< -o $@

clean:
	rm -f *.o *.hi *~ Gaf/*.o Gaf/*.hi Gaf/*~ $(PROGS) 

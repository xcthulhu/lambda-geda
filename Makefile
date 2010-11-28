PROGS = test
MODULES = Gaf.hs Gaf/*.hs

all: $(PROGS) 

%: %.hs $(MODULES)
	ghc --make $< -o $@

clean:
	rm *.o *.hi Gaf/*.o Gaf/*.hi $(PROGS)

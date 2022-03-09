
PROG  = gui_literatuurGTK2Hs2
SOURCES = gui_literatuurGTK2Hs2.hs

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc

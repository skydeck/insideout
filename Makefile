insideout: insideout.mll
	ocamllex $<
	ocamlopt -o $@ insideout.ml

ifndef PREFIX
  PREFIX = $(HOME)
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
endif

.PHONY: install
install:
	cp insideout $(BINDIR)

.PHONY: clean
clean:
	rm -f *.o *.cm* *~ insideout.ml insideout

insideout: insideout.mll
	ocamllex $<
	ocamlopt -o $@ insideout.ml

.PHONY: demo
demo: example
	./example > out.html

example: example.ml example_main.ml
	ocamlopt -o example example.ml example_main.ml

example.ml: insideout example.html
	./insideout example.html -o example.ml

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
	rm -f example.ml out.html example

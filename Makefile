help:
	@echo "Available commands:"
	@echo " make help    # shows this help"
	@echo " make test    # tests solutions"
	@echo " make clean   # cleans working directory"

simpletest.cmo: simpletest.ml
	ocamlc -c simpletest.ml

.PHONY: help test clean

test: simpletest.cmo
	@MAKEFILE_PRIVATE=1 ./private/run_test.sh

clean:
	rm -f *.cmi *.cmo *.o

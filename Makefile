.PHONY : dev_run
dev_run :
	dune build bin/simple_term.exe --profile release && ./_build/default/bin/simple_term.exe

.PHONY : run
run :
	dune build bin/simple_term.exe && ./_build/default/bin/simple_term.exe
	dune build src/rameau.exe && ./_build/default/src/rameau.exe

.PHONY : clean
clean :
	dune clean

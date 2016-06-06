.PHONY: run
run: euler
	./$<

.PHONY: run.native
run.native: euler.native
	./$<

# This won't work if multiple ml files need to be compiled, since their order
# when invoking ocamlc/ocamlopt matters.
euler: *.ml
	ocamlc -safe-string -o $@ str.cma $^

euler.native: *.ml
	ocamlopt -safe-string -o $@ str.cmxa $^

.PHONY: clean
clean:
	rm -fv *.cm* euler euler.native *.o

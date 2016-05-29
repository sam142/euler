.PHONY: run
run: euler
	./$<

.PHONY: run.native
run.native: euler.native
	./$<

euler: *.ml
	ocamlc -safe-string -o $@ $^

euler.native: *.ml
	ocamlopt -safe-string -o $@ $^

.PHONY: clean
clean:
	rm -fv *.cm* euler euler.native *.o

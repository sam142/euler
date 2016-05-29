.PHONY: run
run: euler
	./$<

euler: *.ml
	ocamlc -safe-string -o $@ $^

.PHONY: clean
clean:
	rm -fv *.cm* euler

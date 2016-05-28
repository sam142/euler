.PHONY: run
run: euler
	./$<

euler: *.ml
	ocamlc -o $@ $^

.PHONY: clean
clean:
	rm -fv *.cm* euler

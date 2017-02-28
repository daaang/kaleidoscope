toy.byte:
	ocamlbuild toy.byte

clean:
	rm -rf toy.byte _build

test: toy.byte
	true

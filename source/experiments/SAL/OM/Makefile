all: proof_graph.png

%.png: %.dot
	dot -Tpng $< -o$@

%.pdf: %.dot
	dot -Tpdf $< -o$@

.PHONY: all

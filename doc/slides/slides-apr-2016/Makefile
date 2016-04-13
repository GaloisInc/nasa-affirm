all: improvements-to-om1.pdf
.PHONY: all

improvements-to-om1.pdf: improvements-to-om1.md state-machine.pdf
	pandoc -f markdown -t beamer \
		-o $@ $<

state-machine.pdf: state-machine.tex
	pdflatex $<


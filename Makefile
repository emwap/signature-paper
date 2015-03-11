all: paper.pdf

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

%.tex: %.md Makefile
	pandoc $< -o $@ -s --tab-stop=2 --number-sections


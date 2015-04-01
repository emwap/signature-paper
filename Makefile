all: paper.pdf

paper.pdf: paper.md paper.bib Makefile
	pandoc -s -o paper.pdf paper.md --filter pandoc-citeproc


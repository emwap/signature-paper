.PHONY: all

all: paper.pdf talk.pdf

paper.pdf: paper.md paper.bib Makefile
	pandoc -s -o paper.pdf paper.md --filter pandoc-citeproc

talk.pdf: talk.md paper.bib Makefile
	pandoc -s -o talk.pdf talk.md --bibliography paper.bib -t beamer

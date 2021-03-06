.PHONY: all

all: box talk.pdf talk.html

clean:
	rm -f paper.pdf talk.pdf talk.tex talk.html

box: paper.pdf
	cp paper.pdf ~/Dropbox/Artiklar/signature-paper.pdf

OPTS += --smart
OPTS += --bibliography paper.bib
OPTS += --self-contained

PAPER_OPTS += ${OPTS}
PAPER_OPTS += --listings
PAPER_OPTS += --filter pandoc-citeproc
PAPER_OPTS += --filter filters/fix-biblio.hs
PAPER_OPTS += --filter filters/ghci.hs

paper.tex: paper.md paper.bib Makefile filters/fix-biblio.hs filters/ghci.hs listings_haskell.tex
	stack exec -- pandoc -o $@ paper.md ${PAPER_OPTS}

paper.pdf: paper.md paper.bib Makefile filters/fix-biblio.hs filters/ghci.hs listings_haskell.tex
	stack exec -- pandoc -o $@ paper.md ${PAPER_OPTS}

paper.json: paper.md paper.bib Makefile filters/fix-biblio.hs filters/ghci.hs listings_haskell.tex
	stack exec -- pandoc -o $@ paper.md ${PAPER_OPTS}

TEX_OPTS += ${OPTS}
TEX_OPTS += -t beamer
TEX_OPTS += --incremental
TEX_OPTS += --template beamer.latex

talk.pdf: talk.md paper.bib beamer.latex Makefile
	stack exec -- pandoc -o talk.pdf talk.md ${TEX_OPTS}

talk.tex: talk.md paper.bib Makefile
	stack exec -- pandoc -o talk.tex talk.md ${TEX_OPTS}

HTML_OPTS += ${OPTS}
HTML_OPTS += --template revealjs.html
HTML_OPTS += -t revealjs
HTML_OPTS += -V theme=sky
HTML_OPTS += -V transition=none
HTML_OPTS += --css=talk.css
# HTML_OPTS += -A init.html
# HTML_OPTS += --highlight-style=zenburn
# HTML_OPTS += --highlight-style=monochrome
HTML_OPTS += --no-highlight
HTML_OPTS += -V hlss=tomorrow

talk.html: talk.md talk.css paper.bib revealjs.html Makefile
	pandoc -o talk.html talk.md ${HTML_OPTS}

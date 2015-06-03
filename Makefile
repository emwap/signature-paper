.PHONY: all

all: paper.pdf talk.pdf talk.html

clean:
	rm -f paper.pdf talk.pdf talk.tex talk.html

paper.pdf: paper.md paper.bib Makefile
	pandoc -s -o paper.pdf paper.md --filter pandoc-citeproc

OPTS += --smart
OPTS += --bibliography paper.bib
OPTS += --self-contained

TEX_OPTS += ${OPTS}
TEX_OPTS += -t beamer
TEX_OPTS += --incremental
TEX_OPTS += --template beamer.latex

talk.pdf: talk.md paper.bib beamer.latex Makefile
	pandoc -o talk.pdf talk.md ${TEX_OPTS}

talk.tex: talk.md paper.bib Makefile
	pandoc -o talk.tex talk.md ${TEX_OPTS}

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

##
# Cuerda
#
# @file
# @version 0.1

all: test1.pdf test2.pdf

%.pdf: %.tex
	pdflatex --shell-escape  --interaction=nonstopmode $<

test1.tex: mydiagrams.sty cuerda.sty *.hs
	runhaskell Cuerda.hs

test2.tex: mydiagrams.sty cuerda.sty *.hs
	runhaskell Cuerda.hs

example.pdf: mydiagrams.sty example.tex cuerda.sty
	pdflatex --shell-escape example.tex

clean:
	rm -rf test.tex test.pdf *.hi *.o *.aux *.log .texfrag* .auctex-auto*

tarball:
	rm -f cuerdahs.tar.gz
	tar -czvf cuerdahs.tar.gz *.hs *.tex *.org makefile
	tar -czvf 00-index.tar cuerda.cabal


.INTERMEDIATE: test.tex
.PHONY: clean

# end

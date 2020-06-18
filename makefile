##
# Corda
#
# @file
# @version 0.1

all: test1.pdf

%.pdf: %.tex
	pdflatex --shell-escape  --interaction=nonstopmode $<

test1.tex: mydiagrams.sty corda.sty *.hs
	runhaskell Corda.hs

example.pdf: mydiagrams.sty example.tex corda.sty
	pdflatex --shell-escape example.tex

clean:
	rm -rf test.tex test.pdf *.hi *.o *.aux *.log .texfrag* .auctex-auto*

tarball:
	rm -f cordahs.tar.gz
	tar -czvf cordahs.tar.gz *.hs *.tex *.org makefile
	tar -czvf 00-index.tar corda.cabal


.INTERMEDIATE: test.tex
.PHONY: clean

# end

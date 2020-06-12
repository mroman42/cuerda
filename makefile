##
# Chorda
#
# @file
# @version 0.1

example.pdf: mydiagrams.sty example.tex corda.sty
	pdflatex --shell-escape example.tex

mydiagrams.sty: *.hs
	runhaskell Example.hs > $@

clean:
	rm -rf test.tex test.pdf *.hi *.o *.aux *.log .texfrag* .auctex-auto*

tarball:
	rm -f cordahs.tar.gz
	tar -czvf cordahs.tar.gz *.hs *.tex *.org makefile

.INTERMEDIATE: test.tex
.PHONY: clean

# end

##
# Chorda
#
# @file
# @version 0.1

test.pdf: test.tex
	pdflatex $<

test.tex: *.hs latexHeader.tex latexFooter.tex
	runhaskell Example.hs > $@

clean:
	rm -rf test.tex test.pdf *.hi *.o *.aux *.log .texfrag* .auctex-auto*

tarball:
	rm -f chordahs.tar.gz
	tar -czvf chordahs.tar.gz *.hs *.tex *.org makefile

.INTERMEDIATE: test.tex
.PHONY: clean

# end

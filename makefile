##
# Chorda
#
# @file
# @version 0.1

example.pdf: mydiagrams.sty example.tex chorda.sty
	pdflatex example.tex

mydiagrams.sty: *.hs
	runhaskell Example.hs > $@

clean:
	rm -rf test.tex test.pdf *.hi *.o *.aux *.log .texfrag* .auctex-auto*

tarball:
	rm -f chordahs.tar.gz
	tar -czvf chordahs.tar.gz *.hs *.tex *.org makefile

.INTERMEDIATE: test.tex
.PHONY: clean

# end

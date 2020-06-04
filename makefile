##
# Chorda
#
# @file
# @version 0.1

test.pdf: test.tex
	pdflatex $<

test.tex: Chorda.hs
	runhaskell Chorda.hs > $@

clean:
	rm -f test.tex test.pdf

.INTERMEDIATE: test.tex
.PHONY: clean

# end

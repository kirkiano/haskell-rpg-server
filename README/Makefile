
PDFLATEX=pdflatex -jobname=README --output-directory=../out

.PHONY: build clean


clean:
	find . -iname '*.aux' -print0 | xargs -0 rm
	find . -iname '*.log' -print0 | xargs -0 rm
	find . -iname '*.toc' -print0 | xargs -0 rm
	find . -iname '*.out' -print0 | xargs -0 rm

build:
	cd src && $(PDFLATEX) main.tex
	cd src && $(PDFLATEX) main.tex
	cd src && $(PDFLATEX) main.tex

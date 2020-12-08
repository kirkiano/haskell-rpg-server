
LATEX=lualatex -jobname=README --output-directory=../out

.PHONY: build clean


build: clean
	cd src && $(LATEX) main.tex
	cd src && $(LATEX) main.tex
	cd src && $(LATEX) main.tex

clean:
	find . -iname '*.aux' -print0 | xargs -0 rm
	find . -iname '*.log' -print0 | xargs -0 rm
	find . -iname '*.toc' -print0 | xargs -0 rm
	find . -iname '*.out' -print0 | xargs -0 rm

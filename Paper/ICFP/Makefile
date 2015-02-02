all: paper.pdf

paper.tex: paper.lhs Section2.lhs Section3.lhs Section4.lhs
	if [ -f "paper.tex" ]; then chmod +w paper.tex; fi
	lhs2TeX -o paper.tex paper.lhs
	chmod -w paper.tex

paper.aux: paper.tex table.tex formalism.tex
	pdflatex paper

paper.bbl: paper.aux paper.bib
	bibtex paper

paper.pdf: paper.aux paper.bbl
	pdflatex paper
	pdflatex paper

.PHONY:clean
clean:
	rm -f *.out *.aux *.log *.bbl *.blg *.ptb *.brf *~ \
	paper.tex

clean-all: clean
	rm -f paper.pdf

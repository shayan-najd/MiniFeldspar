all: icfp-paper.pdf

icfp-paper.tex: icfp-paper.lhs Section2.lhs Section3.lhs Section4.lhs
	if [ -f "icfp-paper.tex" ]; then chmod +w icfp-paper.tex; fi
	lhs2TeX -o icfp-paper.tex icfp-paper.lhs
	chmod -w icfp-paper.tex

icfp-paper.aux: icfp-paper.tex table.tex formalism.tex
	pdflatex icfp-paper

icfp-paper.bbl: icfp-paper.aux icfp-paper.bib
	bibtex icfp-paper

icfp-paper.pdf: icfp-paper.aux icfp-paper.bbl
	pdflatex icfp-paper
	pdflatex icfp-paper

paper.tex: paper.lhs Section2.lhs Section3.lhs Section4.lhs
	if [ -f "paper.tex" ]; then chmod +w paper.tex; fi
	lhs2TeX -o paper.tex paper.lhs
	chmod -w paper.tex

paper.aux: paper.tex table.tex formalism.tex
	pdflatex paper > /dev/null

paper.bbl: paper.aux paper.bib
	bibtex paper > /dev/null

paper.pdf: paper.aux paper.bbl
	pdflatex paper > /dev/null
	pdflatex paper

.PHONY:clean
clean:
	rm -f *.out *.aux *.log *.bbl *.blg *.ptb *.brf *~ \
	paper.tex

clean-all: clean
	rm -f paper.pdf

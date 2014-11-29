cv.pdf: _cv.tex
	pdflatex _cv
	pdflatex _cv
	mv _cv.pdf cv.pdf

simulation-design.html: simulation-design.qmd
	quarto render

publish:
	quarto publish gh-pages

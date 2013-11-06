haddock:
	cabal haddock --executables --hyperlink-source
	xdg-open dist/doc/html/sloch/sloch/index.html
	xdg-open dist/doc/html/sloch/lang-gen/index.html

hlint:
	@mkdir -p dist/hlint
	-hlint src -rdist/hlint/report.html 1>/dev/null
	xdg-open dist/hlint/report.html

#!/bin/bash

# Haddock
cabal haddock --executables --hyperlink-source

# Hlint
mkdir -p ./dist/hlint
hlint src -rdist/hlint/report.html 1>/dev/null

# SourceGraph
SourceGraph ./sloch.cabal

xdg-open ./site.html

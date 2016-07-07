VERSION:=$(shell grep Version: DESCRIPTION|sed 's/Version: //')
NAME:=$(shell grep Package: DESCRIPTION|sed 's/Package: //')
PACKAGEFILE:=../$(NAME)_$(VERSION).tar.gz

all: $(PACKAGEFILE) README.md

.PHONY: all install

install:
	R -e 'devtools::install_github("sherrillmix/violinPointR")'
	R -e 'devtools::install_github("eclarke/violin_point")'

localInstall:
	R -e 'devtools::install()'

man: R/*.R 
	R -e 'devtools::document()'
	touch man


inst/doc: vignettes/*.Rnw R/*.R
	R -e 'devtools::build_vignettes()'
	touch inst/doc

README.md: README.Rmd R/*.R
	make localInstall
	R -e 'knitr::opts_chunk$$set(fig.path="README_files/figure-html/");knitr::knit("README.Rmd")'
	sed '/^---$$/,/^---$$/d' README.md --in-place
	
$(PACKAGEFILE): man R/*.R DESCRIPTION inst/doc
	R -e 'devtools::check();devtools::build()'

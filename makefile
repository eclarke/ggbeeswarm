VERSION:=$(shell grep Version: DESCRIPTION|sed 's/Version: //')
NAME:=$(shell grep Package: DESCRIPTION|sed 's/Package: //')
PACKAGEFILE:=../$(NAME)_$(VERSION).tar.gz

all: $(PACKAGEFILE)

.PHONY: all install

install:
	R -e 'devtools::install_github("eclarke/violin_point")'

man: R/*.R R/*.r
	R -e 'devtools::document()'
	touch man


#inst/doc: vignettes/*.Rnw
	#R -e 'devtools::build_vignettes()'
	#touch inst/doc

	
#inst/doc
$(PACKAGEFILE): man R/*.R DESCRIPTION 
	R -e 'devtools::check();devtools::build()'

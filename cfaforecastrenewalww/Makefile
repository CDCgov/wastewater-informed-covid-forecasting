PKG_VERSION=$(shell Rscript --vanilla -e 'l<-readLines("DESCRIPTION");cat(gsub(".+:\\s+", "", l[grepl("^Version", l)]))')

docs:
	Rscript --vanilla -e 'roxygen2::roxygenize()'

build:
	cd .. && \
		R CMD build cfaforecastrenewalww

check:
	cd .. && \
		R CMD check cfaforecastrenewalww_$(PKG_VERSION).tar.gz

install: build
	cd .. && \
		R CMD INSTALL --preclean cfaforecastrenewalww_$(PKG_VERSION).tar.gz

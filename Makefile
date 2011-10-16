package := OOS
version := 0.2
zipfile := $(package)_$(version).tar.gz

RFLAGS := --vanilla --slave
Rscript := Rscript

Rfiles := $(patsubst OOS/man/%.Rd,OOS/R/%.R,$(wildcard OOS/man/*.Rd))

.PHONY: all build

all: build Install OOS/inst/doc/implementation.pdf
build: $(zipfile)
$(zipfile): Check 
	R CMD build $(package)

Install: $(zipfile)
	R CMD INSTALL $(package)
	touch $@

$(Rfiles) OOS/NAMESPACE: OOS/noweb/implementation.rnw
	notangle -R$(@F) $< > $@
OOS/inst/doc/implementation.pdf: OOS/inst/doc/implementation.tex
%.pdf: %.tex
	cd $(dir $<) && pdflatex -p -q -b $(<F)
OOS/inst/doc/implementation.tex: OOS/noweb/implementation.rnw
	noweave -latex -x -delay $< > $@

# I like this next rule.  The 'check' file depends on every file that's
# under version control or unknown in the $(package) subdirectory.
Check: $(Rfiles) OOS/NAMESPACE $(addprefix $(package)/,$(shell bzr ls $(package)/ -R --unknown -V --kind=file))
	R CMD check $(package)
	touch $@
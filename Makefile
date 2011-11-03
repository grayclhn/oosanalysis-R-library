package := oosanalysis
version := 0.2
zipfile := $(package)_$(version).tar.gz

RFLAGS   := --vanilla --slave
Rscript  := Rscript
latexmk  := /usr/local/texlive/2011/bin/x86_64-linux/latexmk
LATEXMKFLAGS := -pdf -silent
noweave := noweave
notangle:= notangle

Rfiles := $(patsubst $(package)/man/%.Rd,$(package)/R/%.R,$(filter-out $(package)/man/nobs-methods.Rd,$(wildcard $(package)/man/*.Rd)))

.PHONY: all build pdf

all: check build install pdf
pdf: $(package)/inst/doc/implementation.pdf
build: $(zipfile)
$(zipfile): check 
	R CMD build $(package)

install: $(zipfile)
	sudo R CMD INSTALL $(package)
	touch $@

$(Rfiles) $(package)/NAMESPACE: $(package)/noweb/implementation.rnw
	mkdir -p $(package)/R
	$(notangle) -R$(@F) $< | cpif $@
%.pdf: %.tex
	cd $(dir $<) && $(latexmk) $(LATEXMKFLAGS) $(<F)
$(package)/inst/doc/implementation.tex: $(package)/noweb/implementation.rnw
	mkdir -p $(package)/inst/doc
	$(noweave) -latex -index -delay $< | sed /^#/d | cpif $@

# I like this next rule.  The 'check' file depends on every file that's
# under version control or unknown in the $(package) subdirectory.
check: pdf $(Rfiles) $(package)/NAMESPACE $(addprefix $(package)/,$(shell bzr ls $(package)/ -R --unknown -V --kind=file))
	R CMD check $(package)
	touch $@
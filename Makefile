package := OOS
version := 0.01
zipfile := $(package)_$(version).tar.gz

.PHONY: all build

all: build Install
build: $(zipfile)
$(zipfile): Check 
	R CMD build $(package)

Install: $(zipfile)
	R CMD INSTALL $(package)
	touch $@

OOS/data/mccArray.Rda: R/mccArray.R OOS/R/make.mccArray.R
	R CMD BATCH --vanilla --slave $< 

# I like this next rule.  The 'check' file depends on every file that's
# under version control or unknown in the $(package) subdirectory.
Check: OOS/data/mccArray.Rda $(shell bzr ls $(package)/ -R --unknown -V --kind=file)
	R CMD check $(package)
	touch $@
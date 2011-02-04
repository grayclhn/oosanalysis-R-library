package := OOS
version := 0.01
zipfile := $(package)_$(version).tar.gz

.PHONY: all build

all: build Install Online
build: $(zipfile)
	R CMD build $(package)

Install: $(zipfile)
	R CMD INSTALL $(package)
	touch $@

# Upload to my webpage
Online: $(zipfile)
	scp $? gcalhoun@econ22.econ.iastate.edu:public_html/software
	touch $@

# I like this next rule.  The 'check' file depends on every file that's
# under version control or unknown in the $(package) subdirectory.
Check: $(addprefix $(package)/,$(shell bzr ls $(package)/ -R --unknown -V --kind=file))
	R CMD check $(package)
	touch $@
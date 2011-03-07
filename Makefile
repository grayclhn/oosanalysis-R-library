package := OOS
version := 0.01
zipfile := $(package)_$(version).tar.gz

data := OOS/data/mccArray.RData

RFLAGS := --vanilla --slave
Rscript := Rscript

.PHONY: all build

all: build Install
build: $(zipfile)
$(zipfile): Check 
	R CMD build $(package)

Install: $(zipfile)
	R CMD INSTALL $(package)
	touch $@

$(data): R/mccArray.R OOS/R/make.mccArray.R OOS/R/rmcc.R
	mkdir -p $(dir $@)
	echo 'datafile <- "$@"' | cat - $< | $(Rscript) - 

# I like this next rule.  The 'check' file depends on every file that's
# under version control or unknown in the $(package) subdirectory.
Check: $(data) $(addprefix $(package)/,$(shell bzr ls $(package)/ -R --unknown -V --kind=file))
	R CMD check $(package)
	touch $@
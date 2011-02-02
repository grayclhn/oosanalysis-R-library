package := OOS
version := 0.01

.PHONY: all

all: $(package)_$(version).tar.gz
	R CMD INSTALL $(package)

# I like this next rule.  The .tar file depends on every file that's
# under version control or unknown in the $(package) subdirectory.
$(package)_$(version).tar.gz: $(addprefix $(package)/,$(shell bzr ls $(package)/ -R --unknown -V --kind=file))
	R CMD check $(package)
	R CMD build $(package)
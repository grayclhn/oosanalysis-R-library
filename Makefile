package := OOS
version := 0.01
zipfile := $(package)_$(version).tar.gz

data := OOS/data/mccArray.RData

RFLAGS := --vanilla --slave
Rscript := Rscript

dbframe := dbframe_0.1.1.tar.gz

remote := econ10.econ.iastate.edu
rrdir := Desktop\oos-Devel
lrdir := Desktop/oos-Devel

scp := scp
scpFLAGS := -oUser=gcalhoun@iastate.edu
ssh := ssh
sshFLAGS := -oUser=gcalhoun@iastate.edu

sshpreamble := cd $(rrdir) && set PATH=c:\\MinGW\\bin;c:\\MinGW\\msys\\1.0\\bin;c:\\Program Files\\R\\R-2.13.0\\bin\\x64;%PATH%

.PHONY: all build

all: build Install
build: $(zipfile)
$(zipfile): Check 
	R CMD build $(package)

Install: $(zipfile)
	R CMD INSTALL $(package)
	touch $@

## installs a package I need to use to create this package.
rsetup.done: $(dbframe)
	$(ssh) $(sshFLAGS) $(remote) "if not exist $(rrdir) mkdir $(rrdir)"
	$(scp) $(scpFLAGS) $< $(remote):$(lrdir)/dbframe.tar.gz
	$(ssh) $(sshFLAGS) $(remote) "$(sshpreamble) && R CMD INSTALL dbframe.tar.gz"
	touch $@

mccracken3-quantiles.done: mccracken2-index.done
mccracken2-index.done: mccracken1-genrv.done
mccracken1-genrv.done: rsetup.done
mccracken1-genrv.done mccracken2-index.done mccracken3-quantiles.done: %.done: %.R
	$(scp) $(scpFLAGS) $< $(remote):$(lrdir)/$<
	$(ssh) $(sshFLAGS) $(remote) "$(sshpreamble) && RScript --vanilla --slave $<"
	touch $@

## local versions
mccracken3-quantiles.local: mccracken2-index.local
mccracken2-index.local: mccracken1-genrv.local
mccracken1-genrv.local mccracken2-index.local mccracken3-quantiles.local: %.local: %.R
	$(Rscript) $(RFLAGS) $<
	touch $@

# create the .Rdata file from the sqlite simulation file
$(data): mccracken3-quantiles.done
	$(ssh) $(sshFLAGS) $(remote) "$(sshpreamble) && RScript --vanilla --slave -e \"library(dbframe); qdata <- dbframe('qmc1', 'mccracken.db'); mccArray <- select(qdata); save(mccArray, file = 'mccArray.RData')"         
	$(scp) $(scpFLAGS) $(remote):$(lrdir)/mccArray.RData $@

# $(data): R/mccArray.R OOS/R/make.mccArray.R OOS/R/rmcc.R
# 	mkdir -p $(dir $@)
#	echo 'datafile <- "$@"' | cat - $< | $(Rscript) - 

# I like this next rule.  The 'check' file depends on every file that's
# under version control or unknown in the $(package) subdirectory.
Check: $(data) $(addprefix $(package)/,$(shell bzr ls $(package)/ -R --unknown -V --kind=file))
	R CMD check $(package)
	touch $@
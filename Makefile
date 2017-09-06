PACKAGE_NAME := $(grep -Po "Package:\s*\K.*" DESCRIPTION)

R_DIR = $(shell pwd)
R_SOURCES := $(shell ls $(R_DIR)/R/*.R )
N = $(shell cat $(R_DIR)/DESCRIPTION |grep ^Package | cut --fields=2 --delimiter=: |sed s/\ //g)
V := $(shell cat $(R_DIR)/DESCRIPTION |grep ^Version  | cut --fields=2 --delimiter=: |sed s/\ //g)
R_TGZ := $(N)_$(V).tar.gz
R_PDF := $(N).pdf
vpath %.R  $(R_DIR)


.PHONY:  R tarball
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all : check $(R_PDF)

pdf: $(R_PDF)

clean:
	rm -fr  *.tar.gz *.out *.pdf  *.log  $(R_dir)/man $(R_dir)/NAMESPACE README.md
	
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tarball : $(R_TGZ)

R : tarball
	@echo "installing Package"
	R CMD INSTALL --no-multiarch --with-keep.source  $(R_DIR)
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$(R_PDF) : $(R_SOURCES)
	R CMD Rd2pdf --force --batch --no-preview -o $(R_PDF) .
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$(R_TGZ) : $(R_SOURCES)
	@echo "Roxygenising:"
	@echo $(R_SOURCES)
	@echo "library(devtools); devtools::document() ; print(\"OK\")" | R --vanilla
	@echo "Building Package $(R_TGZ):"
	@R CMD build $(R_DIR)
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
README.md: README.Rmd
	@echo "library(knitr); knit('README.Rmd')" | R --vanilla
#I~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check: R
	R -e "devtools::check()"


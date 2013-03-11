dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)

%.paper.nw : %.nw
	cat paper-wrapper.nw > $@
	echo "\\input{$$(basename -s .nw $<)} \\end{document}" >> $@

CLEANFILES=ctrnn.c.x 
EXTRA_DIST = ctrnn.paper.pdf

NOWEB_FILES = ctrnn.nw ctrnn.paper.nw
NOWEB_PRODUCTS = ctrnn.c ctrnn.h ctrnn.scm
BUILT_SOURCES = 
BUILT_SOURCES +=ctrnn.c.x
#ctrnn-paper.tex ctrnn.tex
# ctrnn.c.x
ctrnn.c.x : ctrnn.c
ctrnn.o : ctrnn.c.x

if LATEXMK
all-local : ctrnn.paper.pdf
clean-local : clean-latexmk
CLEANFILES+=ctrnn.paper.pdf
endif

lib_LTLIBRARIES = libguile-ctrnn.la

AM_CFLAGS = $(GUILE_CFLAGS) $(GSL_CFLAGS) 
libguile_ctrnn_la_LIBADD  = $(GSL_LIBS) $(GUILE_LIBS)
libguile_ctrnn_la_LDFLAGS = 
libguile_ctrnn_la_SOURCES = ctrnn.c
include_HEADERS = ctrnn.h

guilectrnnmoduledir = `pkg-config guile-2.0 --variable=pkgdatadir`/site/ctrnn
guilectrnnmodule_DATA = ctrnn.scm

EXTRA_DIST += $(guilectrnnmodule_DATA)

MAINTAINERCLEANFILES = Makefile.in config.h.in 

include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

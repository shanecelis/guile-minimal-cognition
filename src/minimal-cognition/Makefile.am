
CLEANFILES = ctrnn.c.x 
EXTRA_DIST = ctrnn.paper.pdf

NOWEB_FILES = ctrnn.nw ctrnn.paper.nw
NOWEB_PRODUCTS = ctrnn.c ctrnn.h ctrnn.scm ctrnn.test.scm
BUILT_SOURCES = ctrnn.c.x

if LATEXMK
all-local : ctrnn.paper.pdf
CLEANFILES += ctrnn.paper.pdf
endif
clean-local : clean-latexmk


lib_LTLIBRARIES = libguile-ctrnn.la

AM_CFLAGS = $(GUILE_CFLAGS) $(GSL_CFLAGS) 
libguile_ctrnn_la_LIBADD  = $(GSL_LIBS) $(GUILE_LIBS)
libguile_ctrnn_la_LDFLAGS = 
libguile_ctrnn_la_SOURCES = ctrnn.c
include_HEADERS = ctrnn.h

MAINTAINERCLEANFILES = Makefile.in config.h.in 

guilemoduledir = $(GUILE_SITE_LIB)/$(PACKAGE_TARNAME)
dist_guilemodule_DATA = ctrnn.scm

#TESTS_ENVIRONMENT = $(GUILE) -L $(top_builddir)/src -L $(top_srcdir)/support/scheme -x $(libdir)
TESTS_ENVIRONMENT = DYLD_LIBRARY_PATH=$(srcdir) $(GUILE) -L $(top_builddir)/src -L $(top_srcdir)/support/scheme 
TESTS = ctrnn.test.scm


include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

ctrnn.c.x : ctrnn.c

dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)

%.paper.nw : %.nw
	(cat paper-wrapper.nw;	echo "\\input{$$(basename -s .nw $<)} \\end{document}") | cpif $@

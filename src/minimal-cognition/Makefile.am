
EXTRA_DIST = ctrnn.paper.pdf vision.paper.pdf

NOWEB_FILES = ctrnn.nw ctrnn.paper.nw ctrnn.appendix.nw fode.nw vision.nw
NOWEB_PRODUCTS = ctrnn.h ctrnn.c ctrnn.scm ctrnn.test.scm ctrnn.test.c \
	               fode.h fode.c fode.scm fode.test.scm vision.scm vision.test.scm vision.c
BUILT_SOURCES = ctrnn.c.x fode.c.x
CLEANFILES = $(BUILT_SOURCES)

if LATEXMK
all-local : ctrnn.paper.pdf vision.paper.pdf
CLEANFILES += ctrnn.paper.pdf vision.paper.pdf

ctrnn.paper.pdf: ctrnn.paper.tex ctrnn.tex
vision.paper.pdf: vision.paper.tex vision.tex
endif
clean-local : clean-latexmk

NOTANGLE_C_FLAGS = -c -L -markup "$(top_srcdir)/bin/namespace-markup"
lib_LTLIBRARIES = libguile-ctrnn.la libguile-fode.la

AM_CFLAGS = $(GUILE_CFLAGS) $(GSL_CFLAGS) 
libguile_ctrnn_la_LIBADD  = $(GSL_LIBS) $(GUILE_LIBS)
libguile_ctrnn_la_SOURCES = ctrnn.c vision.c
include_HEADERS = ctrnn.h 

AM_CFLAGS = $(GUILE_CFLAGS) $(GSL_CFLAGS) 
libguile_fode_la_LIBADD  = $(GSL_LIBS) $(GUILE_LIBS)
libguile_fode_la_SOURCES = fode.c
include_HEADERS = fode.h 

MAINTAINERCLEANFILES = Makefile.in config.h.in 

guilemoduledir = $(GUILE_SITE_LIB)/$(PACKAGE_TARNAME)
dist_guilemodule_DATA = ctrnn.scm

check_PROGRAMS = ctrnn.test
ctrnn_test_LDADD   = libguile-ctrnn.la
ctrnn_test_SOURCES = ctrnn.test.c

#TESTS_ENVIRONMENT = $(GUILE) -L $(top_builddir)/src -L $(top_srcdir)/support/scheme -x $(libdir)
TESTS_ENVIRONMENT = DYLD_LIBRARY_PATH=$(srcdir) $(GUILE) -L $(top_builddir)/src -L $(top_srcdir)/support/scheme -L $(top_srcdir)/.. -L $(top_srcdir)/../noweb-emacsy 
TESTS = ctrnn.test.scm fode.test.scm vision.test.scm

# TESTS_ENVIRONMENT = 
# TESTS += ctrnn.test

# src_NOWEB = ctrnn.c 
# #ctrnn.scm ctrnn.h
# ctrnn_c_SOURCES = ctrnn.nw ctrnn.appendix.nw

include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am
#include $(top_srcdir)/support/automake/noweb2.am

ctrnn.c.x : ctrnn.c
fode.c.x : fode.c

ctrnn.c : ctrnn.nw ctrnn.appendix.nw $(BOILERPLATE)
ctrnn.test.scm : ctrnn.nw ctrnn.appendix.nw $(BOILERPLATE)
ctrnn.test.c : ctrnn.nw ctrnn.appendix.nw $(BOILERPLATE)

vision.c : vision.nw ctrnn.appendix.nw $(BOILERPLATE)


dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)



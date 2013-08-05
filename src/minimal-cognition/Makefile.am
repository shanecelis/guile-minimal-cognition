include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

# Stop make from deleting intermediate files. 
# http://darrendev.blogspot.com/2008/06/stopping-make-delete-intermediate-files.html
.SECONDARY:

NOWEB_DOCS = ctrnn-paper.pdf vision-paper.pdf 
EXTRA_DIST = $(NOWEB_DOCS)

NOWEB_FILES = ctrnn.nw ctrnn-paper.nw ctrnn-appendix.nw fode.nw vision.nw \
							unified-procedure.nw
NOWEB_PRODUCTS = ctrnn.h ctrnn.c ctrnn.scm ctrnn-test.scm ctrnn-test.c \
	               fode.h fode.c fode.scm fode-test.scm vision.scm \
                 vision-test.scm vision.c vision.h vision-test.c \
								 unified-procedure.scm unified-procedure.c unified-procedure.h \
							   unified-procedure-test.c unified-procedure-test.scm

BUILT_SOURCES = ctrnn.c.x fode.c.x vision.c.x unified-procedure.c.x
CLEANFILES = $(BUILT_SOURCES)

doc: $(NOWEB_DOCS)

if PDFLATEX
all-local : 
CLEANFILES += $(NOWEB_DOCS)

ctrnn.tex: ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)
#ctrnn-appendix.tex: ctrnn-appendix.nw ctrnn.nw $(BOILERPLATE)

ctrnn-paper.pdf: ctrnn-paper.tex ctrnn.tex
vision-paper.pdf: vision-paper.tex vision.tex

# ctrnn-cat.nw: ctrnn.nw ctrnn-appendix.nw
# 	cat $^ | cpif $@

endif

MARKUP_FLAGS = -markup "$(top_builddir)/bin/namespace-markup.pl -n ctrnn-appendix=ctrnn -n ctrnn-cat=ctrnn --"
#MARKUP_FLAGS = -markup "$(top_srcdir)/bin/namespace-markup.pl --"
WARN_NOTANGLE_FLAGS = -W 
NOWEAVE_FLAGS = -n -delay $(MARKUP_FLAGS)
NOTANGLE_C_FLAGS = -c -L $(MARKUP_FLAGS)
NOTANGLE_H_FLAGS = -L $(MARKUP_FLAGS)
lib_LTLIBRARIES = libguile-ctrnn.la libguile-fode.la libguile-vision.la libguile-unified-procedure.la

AM_CFLAGS = $(GUILE_CFLAGS) $(GSL_CFLAGS) 
libguile_ctrnn_la_LIBADD  = $(GSL_LIBS) $(GUILE_LIBS)
libguile_ctrnn_la_SOURCES = ctrnn.c
include_HEADERS = ctrnn.h 

libguile_vision_la_LIBADD  = $(GSL_LIBS) $(GUILE_LIBS)
libguile_vision_la_SOURCES = vision.c
include_HEADERS += vision.h

libguile_unified_procedure_la_LIBADD  = $(GUILE_LIBS)
libguile_unified_procedure_la_SOURCES = unified-procedure.c
include_HEADERS += unified-procedure.h

libguile_fode_la_LIBADD  = $(GSL_LIBS) $(GUILE_LIBS)
libguile_fode_la_SOURCES = fode.c
include_HEADERS += fode.h 

MAINTAINERCLEANFILES = Makefile.in config.h.in 

guilemoduledir = $(prefix)/share/guile/site/minimal-cognition
dist_guilemodule_DATA = ctrnn.scm fode.scm vision.scm

check_PROGRAMS = ctrnn-ctest vision-ctest unified-procedure-ctest
ctrnn_ctest_LDADD   = libguile-ctrnn.la libguile-unified-procedure.la
ctrnn_ctest_SOURCES = ctrnn-test.c

vision_ctest_LDADD   = libguile-ctrnn.la libguile-vision.la libguile-unified-procedure.la
vision_ctest_SOURCES = vision-test.c

unified_procedure_ctest_LDADD   = libguile-unified-procedure.la
unified_procedure_ctest_SOURCES = unified-procedure-test.c

#TESTS_ENVIRONMENT = $(GUILE) -L $(top_builddir)/src -L $(top_srcdir)/support/scheme -x $(libdir)
#TESTS_ENVIRONMENT = LOG_COMPILER
TESTS = ctrnn-test.scm fode-test.scm vision-test.scm ctrnn-ctest vision-ctest c-vision-test unified-procedure-ctest unified-procedure-test.scm
TEST_EXTENSIONS = .scm 
SCM_LOG_COMPILER = $(top_builddir)/bin/env guile

# TESTS_ENVIRONMENT = 
# TESTS =  ctrnn-test vision-test 

# src_NOWEB = ctrnn.c 
# #ctrnn.scm ctrnn.h
# ctrnn_c_SOURCES = ctrnn.nw ctrnn-appendix.nw

# check-log: 
# 	$(MAKE) check || cat test-suite.log

# recheck-log: 
# 	$(MAKE) recheck || cat test-suite.log

#	@if test "$(V)" == "1" && test -z "$(TEST_LOGS)"; then echo "FAILED TEST LOGS"; files=`grep -l 'fail$$' $(TEST_LOGS)`; test -z "$$files" && cat $$files;	fi


ctrnn.c.x : ctrnn.c
vision.c.x : vision.c
fode.c.x : fode.c

unified-procedure.c.x : unified-procedure.c

ctrnn.c : ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)
ctrnn-test.scm : ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)
ctrnn-test.c : ctrnn.nw ctrnn-appendix.nw $(BOILERPLATE)

vision.c : vision.nw ctrnn-appendix.nw $(BOILERPLATE)
vision.h : vision.nw ctrnn-appendix.nw $(BOILERPLATE)
vision-test.c : vision.nw ctrnn-appendix.nw $(BOILERPLATE)
vision-test.scm : vision.nw ctrnn-appendix.nw $(BOILERPLATE)

fode-test.scm : fode.nw  $(BOILERPLATE)


dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)



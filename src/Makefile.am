BUILT_SOURCES =
include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

CLEANFILES=ctrnn.c.x

if LATEXMK
CLEANFILES+=ctrnn.paper.pdf
endif

dist-hook:
	mv $(distdir)/*.pdf $(top_distdir)

%.paper.nw : %.nw
	cat paper-wrapper.nw > $@
	echo "\\input{$$(basename -s .nw $<)} \\end{document}" >> $@


NOWEB_FILES = ctrnn.nw ctrnn.paper.nw
EXTRA_DIST = ctrnn.paper.pdf

if NOWEB
EXTRA_DIST += $(NOWEB_FILES:.nw=.tex)
BUILT_SOURCES += ctrnn.c 
BUILT_SOURCES += $(NOWEB_FILES:.nw=.tex)
CLEANFILES += $(NOWEB_FILES:.nw=.tex) ctrnn.c 
# Maybe I should call latexmk -C before removing the .tex files.
endif

#ctrnn-paper.tex ctrnn.tex
# ctrnn.c.x
# ctrnn.c.x : ctrnn.c
# ctrnn.o : ctrnn.c.x

if LATEXMK
all-local : ctrnn.paper.pdf
endif

lib_LTLIBRARIES = libctrnn.la

AM_CFLAGS  = 
libctrnn_la_LIBADD  = 
libctrnn_la_LDFLAGS = 
libctrnn_la_SOURCES = ctrnn.c

MAINTAINERCLEANFILES = Makefile.in config.h.in 

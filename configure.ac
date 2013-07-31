#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([minimal-cognition], [0.01], [bug-report@address])
AC_CONFIG_AUX_DIR([support/build-aux])
# Lies.
#AM_INIT_GUILE_MODULE

# We're going to use GNUMake and we're going to like it.
AM_INIT_AUTOMAKE([-Wall foreign -Wno-portability parallel-tests])
AM_EXTRA_RECURSIVE_TARGETS([preview check-log recheck-log])
AM_SILENT_RULES([yes])
AC_CONFIG_MACRO_DIR([support/m4])
# Checks for programs.
AC_PROG_CC_C99
AC_PROG_CC_C_O
AC_PROG_LIBTOOL
AX_CHECK_NOWEB

GUILE_PROGS

AC_PATH_PROG([PERL], [perl])
AC_SUBST([PERL])

AC_PATH_PROG([BASH], [bash])
AC_SUBST([BASH])

AC_PATH_PROG([guile_snarf], guile-snarf)
AC_SUBST(guile_snarf)


# Checks for libraries.
PKG_CHECK_MODULES([GUILE], [guile-2.0])
PKG_CHECK_MODULES([GSL], [gsl])

AC_SUBST(GUILE_SITE_LIB)

AC_SUBST(DISTDIR, ["${PACKAGE_TARNAME}-${PACKAGE_VERSION}"])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdio.h])

AC_CHECK_HEADERS([tap/basic.h], [], [])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([puts])

# 
AC_SUBST(imagesdir, ['${top_srcdir}/support/images'])
AC_SUBST(automakedir, ['${top_srcdir}/support/automake'])

# Output files.
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([ Makefile
                  src/Makefile 
                  src/minimal-cognition/Makefile 
                  tests/Makefile 
                  tests/works-without-noweb.sh
                  tests/print-install.sh
                  ], [chmod +x bin/runtests])

AC_CONFIG_FILES([ src/minimal-cognition/c-vision-test ],
                [ chmod +x src/minimal-cognition/c-vision-test ])

AC_CONFIG_FILES([ bin/env],
                [ chmod +x bin/env])

AC_CONFIG_FILES([ bin/namespace-markup.pl],
                [ chmod +x bin/namespace-markup.pl])

AC_CONFIG_FILES([ bin/warn-notangle],
                [ chmod +x bin/warn-notangle])

AC_OUTPUT

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([guile-xcb], [0.1], [mark@markwitmer.com])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options])

PKG_PROG_PKG_CONFIG

AC_CONFIG_FILES([
        Makefile
])

GUILE_PKG([2.2])
GUILE_SITE_DIR
GUILE_PROGS
AC_MSG_CHECKING(Guile lib directory)
GUILE_LIB_DIR=`$PKG_CONFIG --print-errors --variable=libdir guile-$GUILE_EFFECTIVE_VERSION`
AC_SUBST(GUILE_LIB_DIR)
AC_MSG_RESULT($GUILE_LIB_DIR)
AC_OUTPUT

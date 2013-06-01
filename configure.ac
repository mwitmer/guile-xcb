#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([guile-xcb], [0.1], [mark@markwitmer.com])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options])

AC_CONFIG_FILES([
        Makefile
])

PKG_CHECK_MODULES([GUILE], [guile-2.2])
GUILE_SITE_DIR

AC_OUTPUT

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([guile-xcb], [1.2], [mark@markwitmer.com])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options])

PKG_PROG_PKG_CONFIG

GUILE_PKG([2.0 2.2])
GUILE_SITE_DIR
GUILE_PROGS

GUILE_SITE_CCACHE=`$GUILE -c '(display (%site-ccache-dir))'`

AC_ARG_WITH([guile-site-dir],
        [AS_HELP_STRING([--with-guile-site-dir],
            [use the specified installation path for Guile modules]
        )],
        [guilesitedir="$withval"],
        [guilesitedir="$GUILE_SITE"]
)

AC_ARG_WITH([guile-site-ccache-dir],
       [AS_HELP_STRING([--with-guile-site-ccache-dir],
           [use the specified installation path for Guile compiled modules]
       )],
       [guilesiteccachedir="$withval"],
       [guilesiteccachedir="$GUILE_SITE_CCACHE"]
)

AC_CONFIG_FILES([
        Makefile
])

AC_SUBST([guilesitedir])
AC_SUBST([guilesiteccachedir])
AC_MSG_RESULT($guilesitedir)
AC_MSG_RESULT($guilesiteccachedir)


AC_OUTPUT

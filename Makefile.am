## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2013 Mark Witmer
##
##   This file is part of guile-xcb.
##
##   guile-xcb is free software; you can redistribute it and/or modify
##   it under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   guile-xcb is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with guile-xcb; see the file COPYING.LESSER.  If
##   not, write to the Free Software Foundation, Inc., 51 Franklin
##   Street, Fifth Floor, Boston, MA 02110-1301 USA

ACLOCAL_AMFLAGS = -I ./m4

%.go : %.xml language/xml-xcb/compile-scheme.scm
	GUILE_FLAGS="-C ${CURDIR}" @GUILE_TOOLS@ compile $< --from=xml-xcb --output=$@

SCM_SITE_CCACHE_DIR = $(GUILE_LIB_DIR)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
CLEANFILES = xcb/xml/xproto.go xcb/xml/ext/*.go

godir = $(SCM_SITE_CCACHE_DIR)
nobase_go_DATA =                              \
  xcb/xml/xproto.go                           \
  xcb/xml/ext/bigreq.go                       \
  xcb/xml/ext/xc_misc.go                      \
  xcb/xml/ext/render.go                       \
  xcb/xml/ext/randr.go

scmdir = $(GUILE_SITE)
nobase_scm_DATA =                             \
  language/xml-xcb/support.scm                \
  language/xml-xcb/spec.scm                   \
  language/xml-xcb/compile-scheme.scm         \
  xcb/xml/records.scm                         \
  xcb/xml/enum.scm                            \
  xcb/xml/type.scm                            \
  xcb/xml/union.scm                           \
  xcb/xml/ext-support.scm                     \
  xcb/xml/struct.scm                          \
  xcb/xml/doc.scm                             \
  xcb/xml/connection.scm                      \
  xcb/xml/core.scm                            \
  xcb/xml/auth.scm                            \
  xcb/xml.scm                                 

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

AUTOMAKE_OPTIONS = -Wno-portability
ACLOCAL_AMFLAGS = -I ./m4

%.go : %.xml language/xml-xcb/compile-scheme.go language/xml-xcb/spec.go xcb/xml/records.go
	GUILE_FLAGS="-C ${CURDIR}" @GUILE_TOOLS@ compile $< --from=xml-xcb --output=$@

%.go : %.scm
	GUILE_FLAGS="-C ${CURDIR}" @GUILE_TOOLS@ compile $< --output=$@

SCM_SITE_CCACHE_DIR = $(GUILE_LIB_DIR)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
CLEANFILES = xcb/xml/ext/*.go xcb/xml/*.go language/xml-xcb/*.go

info_TEXINFOS = guile-xcb.texi

godir = $(SCM_SITE_CCACHE_DIR)
nobase_go_DATA =                     \
  flow/event-loop.go                 \
  xcb/xml/records.go                 \
  xcb/xml/enum.go                    \
  xcb/xml/type.go                    \
  language/xml-xcb/support.go        \
  xcb/xml/union.go                   \
  xcb/xml/struct.go                  \
  xcb/xml/doc.go                     \
  xcb/xml/connection.go              \
  language/xml-xcb/compile-scheme.go \
  language/xml-xcb/spec.go           \
  xcb/event-loop.go                  \
  xcb/xml/xproto.go                  \
  xcb/xml/ext-support.go             \
  xcb/xml/ext/bigreq.go              \
  xcb/xml/ext/ge.go                  \
  xcb/xml/ext/xc_misc.go             \
  xcb/xml/core.go                    \
  xcb/xml/auth.go                    \
  xcb/xml.go                         \
  xcb/xml/ext/render.go              \
  xcb/xml/ext/randr.go               \
  xcb/xml/ext/dri2.go                \
  xcb/xml/ext/shape.go               \
  xcb/xml/ext/xfixes.go              \
  xcb/xml/ext/composite.go           \
  xcb/xml/ext/damage.go              \
  xcb/xml/ext/dpms.go                \
  xcb/xml/ext/glx.go                 \
  xcb/xml/ext/record.go              \
  xcb/xml/ext/res.go                 \
  xcb/xml/ext/screensaver.go         \
  xcb/xml/ext/shm.go                 \
  xcb/xml/ext/sync.go                \
  xcb/xml/ext/xevie.go               \
  xcb/xml/ext/xf86dri.go             \
  xcb/xml/ext/xf86vidmode.go         \
  xcb/xml/ext/xinerama.go            \
  xcb/xml/ext/xinput.go              \
  xcb/xml/ext/xkb.go                 \
  xcb/xml/ext/xselinux.go            \
  xcb/xml/ext/xtest.go               \
  xcb/xml/ext/xv.go                  \
  xcb/xml/ext/xvmc.go


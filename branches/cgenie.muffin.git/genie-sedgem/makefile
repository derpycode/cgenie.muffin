#
# $Id: makefile 3320 2007-06-14 14:22:56Z andrew-price $
#

export

LIBNAME = libsedgem
LIBDIR  = lib
SRCDIR  = src/fortran

.PHONY: all $(LIBNAME).$(LIB_EXT) clean tags

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT):
	\cd $(SRCDIR) ; $(MAKE) all
	\cd $(LIBDIR) ; $(MAKE) all

clean:
	\cd $(SRCDIR) ; $(MAKE) clean
	\cd $(LIBDIR) ; $(MAKE) clean

tags:
	\cd $(SRCDIR) ; $(MAKE) tags

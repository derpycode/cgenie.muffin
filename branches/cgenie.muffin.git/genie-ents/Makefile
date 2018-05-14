#
# $Id: makefile,v 1.0 2007/02/01 18:20:38 cvs-sg Exp $
#
# include ../genie-main/makefile.arc
export

LIBNAME = libents
LIBDIR  = lib
SRCDIR  = src/fortran

.PHONY : all $(LIBNAME).$(LIB_EXT) clean tags


$(LIBNAME).$(LIB_EXT):
	\cd $(SRCDIR) ; $(MAKE) all
	\cd $(LIBDIR) ; $(MAKE) all

clean:
	\cd $(LIBDIR) ; $(MAKE) clean
	\cd $(SRCDIR) ; $(MAKE) clean

tags:
	\cd $(SRCDIR) ; $(MAKE) tags

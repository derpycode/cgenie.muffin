#
# $Id$
#

export

LIBNAME = libgoldsteinseaice
FOR_DIR = src/fortran
LIB_DIR = lib

.PHONY : all $(LIBNAME).$(LIB_EXT) clean tags

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT) :
	\cd $(FOR_DIR) ; $(MAKE) all
	\cd $(LIB_DIR) ; $(MAKE) all
clean :
	\cd $(FOR_DIR) ; $(MAKE) clean
	\cd $(LIB_DIR) ; $(MAKE) clean

tags:
	\cd $(FOR_DIR) ; $(MAKE) tags

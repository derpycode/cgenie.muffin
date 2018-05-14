#
# $Id: Makefile 3273 2007-04-30 14:14:31Z cvs-gw $
#

FOR_DIR = ../src/fortran

.PHONY: all clean

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT): $(FOR_DIR)/*.$(OBJ_EXT)
	$(LIB_CMD)$@ $^

clean:
	rm -f $(LIBNAME).$(LIB_EXT)

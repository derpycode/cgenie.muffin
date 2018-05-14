#
# $Id$
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(FLAGR8)

LIBNAME = libutil1
###OBJECTS = gwtlt.$(OBJ_EXT) hessen.$(OBJ_EXT) qreig.$(OBJ_EXT) \
###          qrt.$(OBJ_EXT) ranf.$(OBJ_EXT) lnsig.$(OBJ_EXT) \
###          cblank.$(OBJ_EXT) awi.$(OBJ_EXT) gwtcnr.$(OBJ_EXT) \
###          gwtbox.$(OBJ_EXT) lnsig1.$(OBJ_EXT) gwtlt1.$(OBJ_EXT) \
###          xtstwn.$(OBJ_EXT) calndr.$(OBJ_EXT)
OBJECTS = lnsig.$(OBJ_EXT) lnsig1.$(OBJ_EXT)

OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES)
	$(LIB_CMD)$(LIBNAME).$(LIB_EXT) $(OBJECTS)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

###gwtlt.$(OBJ_EXT)		: gwtlt.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) gwtlt.f

###hessen.$(OBJ_EXT)		: hessen.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) hessen.f

###qreig.$(OBJ_EXT)		: qreig.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) qreig.f

###qrt.$(OBJ_EXT)		: qrt.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) qrt.f

###ranf.$(OBJ_EXT)		: ranf.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) ranf.f

###awi.$(OBJ_EXT)		: awi.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) awi.f

###gwtcnr.$(OBJ_EXT)		: gwtcnr.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) gwtcnr.f

###gwtbox.$(OBJ_EXT)		: gwtbox.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) gwtbox.f

lnsig1.$(OBJ_EXT)		: lnsig1.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) lnsig1.f

lnsig.$(OBJ_EXT)		: lnsig.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) lnsig.f

###cblank.$(OBJ_EXT)		: cblank.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) cblank.f

###gwtlt1.$(OBJ_EXT)		: gwtlt1.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) gwtlt1.f

###xtstwn.$(OBJ_EXT)		: xtstwn.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) xtstwn.f

###calndr.$(OBJ_EXT)		: calndr.f $(OTHER_FILES)
###	$(F77) $(COMPILEONLY) $(LOCALFLAGS) calndr.f


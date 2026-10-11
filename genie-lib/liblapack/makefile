
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(FLAGR8)

LIBNAME = liblapack

# Source and target names
SRCS = dgemm.f  dgesv.f  dgetrf.f  dgetrf2.f  dgetrs.f  dlamch.f  dlaswp.f  dscal.f  dtrsm.f  idamax.f  ieeeck.f  ilaenv.f  iparmq.f  lsame.f  xerbla.f
OBJECTS = $(SRCS:.f=.o)

OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES)
	$(LIB_CMD)$(LIBNAME).$(LIB_EXT) $(OBJECTS)
	
.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

# all: $(LIB)

# $(LIB): $(OBJS)
	# ar rcs $@ $^

%.o: %.f $(OTHER_FILES)
	$(F77) -c -O3 $<

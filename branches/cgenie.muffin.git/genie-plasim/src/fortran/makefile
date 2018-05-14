#
# $Id: makefile 6279 2011-03-29 12:08:32Z gethin-williams $
#

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS)
LOCALFLAGS_F77=$(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS)
LOCALFLAGS_F90=$(FPPFLAGS) $(FFLAGS) $(F90FLAGS) $(FLAGS) $(LOC_INC_FLAGS)

%.o: %.mod

.SUFFIXES: .$(OBJ_EXT) .f .f90


.F.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS_F77) $(DOPTS) \
	$(COMPILEONLY) $*.F

.f.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS_F77) \
	$(GENIEPRECISIONOPTS) $(COMPILEONLY) $*.f

.f:
	$(F77) $(LOCALFLAGS_F77) \
	$(GENIEPRECISIONOPTS) $(OBJ_FLAG) $<

.f90.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS_F90) \
	$(GENIEPRECISIONOPTS) $(COMPILEONLY) $*.f90

.f90:
	$(F77) $(LOCALFLAGS_F90) \
	$(GENIEPRECISIONOPTS) $(OBJ_FLAG) $<

OBJS =	calmod.$(OBJ_EXT) cpl_stub.$(OBJ_EXT) fftmod.$(OBJ_EXT) \
 	fluxmod.$(OBJ_EXT) gaussmod.$(OBJ_EXT) \
	icemod.$(OBJ_EXT) landmod.$(OBJ_EXT) legmod.$(OBJ_EXT) \
	miscmod.$(OBJ_EXT) mpimod_stub.$(OBJ_EXT) oceanmod.$(OBJ_EXT) \
	outmod.$(OBJ_EXT) plasim.$(OBJ_EXT) \
	plasimmod.$(OBJ_EXT) radmod.$(OBJ_EXT) rainmod.$(OBJ_EXT) \
	geniemod.$(OBJ_EXT) restartmod.$(OBJ_EXT) seamod.$(OBJ_EXT) \
	simba.$(OBJ_EXT) surfmod.$(OBJ_EXT) tpcore.$(OBJ_EXT) \
	tracermod.$(OBJ_EXT) trc_routines.$(OBJ_EXT)

FBJS =	calmod.f90 cpl_stub.f90 fftmod.f90 fluxmod.f90 gaussmod.f90 \
	icemod.f90 landmod.f90 legmod.f90 miscmod.f90 \
	mpimod_stub.f90 oceanmod.f90 outmod.f90 plasim.f90 \
	plasimmod.f90 radmod.f90 rainmod.f90 geniemod.f90 restartmod.f90 \
	seamod.f90 simba.f90 surfmod.f90 tpcore.f90 tracermod.f90 \
	trc_routines.f90

#FLAGS = -r8 -O
FLAGS = $(FLAGR8) $(GOLDOPTIM)
DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos \
        $(GENIEPRECISIONOPTS) $(LIBNC1PRECISIONOPTS) $(LIBUTIL1PRECISIONOPTS)
#DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos \
#	$(GENIEPRECISIONOPTS) $(LIBNC1PRECISIONOPTS) $(LIBUTIL1PRECISIONOPTS)
#DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos $(DEFINE)clock \
#	$(GENIEPRECISIONOPTS) $(LIBNC1PRECISIONOPTS) $(LIBUTIL1PRECISIONOPTS)
LIBNAME = libembm
LOCAL_BIN_DIR = ../../bin

CMN =  makefile ../../../genie-main/makefile.arc

all : $(OBJS) $(CMN)

.PHONY: all clean tags
clean:
	\rm -f *.$(OBJ_EXT) *__genmod.f90 *__genmod.mod *.o *.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

# dependencies
fftmod.$(OBJ_EXT):	 fftmod.f90
gaussmod.$(OBJ_EXT):	 gaussmod.f90
geniemod.$(OBJ_EXT):	 geniemod.f90
restartmod.$(OBJ_EXT):	 restartmod.f90
tpcore.$(OBJ_EXT):	 tpcore.f90
trc_routines.$(OBJ_EXT): trc_routines.f90

plasimmod.$(OBJ_EXT):	 plasimmod.f90 geniemod.$(OBJ_EXT)

plasim.$(OBJ_EXT):	 plasim.f90 plasimmod.$(OBJ_EXT)
mpimod_stub.$(OBJ_EXT):  mpimod_stub.f90 plasimmod.$(OBJ_EXT)
rainmod.$(OBJ_EXT):	 rainmod.f90 plasimmod.$(OBJ_EXT)
landmod.$(OBJ_EXT):	 landmod.f90 plasimmod.$(OBJ_EXT)
calmod.$(OBJ_EXT):	 calmod.f90 plasimmod.$(OBJ_EXT)
legmod.$(OBJ_EXT):	 legmod.f90 plasimmod.$(OBJ_EXT)
outmod.$(OBJ_EXT):	 outmod.f90 plasimmod.$(OBJ_EXT)
miscmod.$(OBJ_EXT):	 miscmod.f90 plasimmod.$(OBJ_EXT)
fluxmod.$(OBJ_EXT):	 fluxmod.f90 plasimmod.$(OBJ_EXT)
radmod.$(OBJ_EXT):	 radmod.f90 plasimmod.$(OBJ_EXT)
surfmod.$(OBJ_EXT):	 surfmod.f90 plasimmod.$(OBJ_EXT)
seamod.$(OBJ_EXT):	 seamod.f90 plasimmod.$(OBJ_EXT)
icemod.$(OBJ_EXT):	 icemod.f90 plasimmod.$(OBJ_EXT)
oceanmod.$(OBJ_EXT):	 oceanmod.f90 plasimmod.$(OBJ_EXT)
tracermod.$(OBJ_EXT):	 tracermod.f90 plasimmod.$(OBJ_EXT)

simba.$(OBJ_EXT):	 simba.f90 landmod.$(OBJ_EXT)
cpl_stub.$(OBJ_EXT):	 cpl_stub.f90 plasimmod.$(OBJ_EXT) oceanmod.$(OBJ_EXT) icemod.$(OBJ_EXT) seamod.$(OBJ_EXT)


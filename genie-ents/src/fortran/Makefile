#
# $Id: makefile,v 1.0 2007/01/04 10:25:15 cvs-sg Exp $
#

# include  ../../../genie-main/makefile.arc

# GOLDSTEINNLONSOPTS=$(DEFINE)GOLDSTEINNLONS=36
# GOLDSTEINNLATSOPTS=$(DEFINE)GOLDSTEINNLATS=36
# GOLDSTEINNLEVSOPTS=$(DEFINE)GOLDSTEINNLEVS=8

.SUFFIXES: .$(OBJ_EXT) .f

.F.$(OBJ_EXT):
	$(F77) $(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(DOPTS) \
	$(GOLDSTEINNLONSOPTS) $(GOLDSTEINNLATSOPTS) $(GOLDSTEINNLEVSOPTS) \
	$(COMPILEONLY) $*.F

.f.$(OBJ_EXT):
	$(F77) $(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) \
	$(GOLDSTEINNLONSOPTS) $(GOLDSTEINNLATSOPTS) $(GOLDSTEINNLEVSOPTS) \
	$(COMPILEONLY) $*.f

.f:
	$(F77) $(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) \
	$(GOLDSTEINNLONSOPTS) $(GOLDSTEINNLATSOPTS) $(GOLDSTEINNLEVSOPTS) \
	$(OBJ_FLAG) $<

OBJS =	in_ents_ascii.$(OBJ_EXT) out_ents.$(OBJ_EXT) \
       setup_ents.$(OBJ_EXT) initialise_ents.$(OBJ_EXT) \
	screen_diags.$(OBJ_EXT) carbon.$(OBJ_EXT) carbt_diags.$(OBJ_EXT) \
	sealevel.$(OBJ_EXT) physt_diags.$(OBJ_EXT) annav_diags.$(OBJ_EXT) \
	ents.$(OBJ_EXT) entsdiagosc.$(OBJ_EXT) ConvertFunc.$(OBJ_EXT) \
	netcdf_db_ents.$(OBJ_EXT) netcdf_ents.$(OBJ_EXT) \
	netcdf_ts_ents.$(OBJ_EXT) in_ents_netcdf.$(OBJ_EXT)

FBJS = in_ents_ascii.F out_ents.F setup_ents.F \
       initialise_ents.F screen_diags.F carbon.F \
       carbt_diags.F sealevel.F physt_diags.F \
       annav_diags.F ents.F entsdiagosc.F \
       ConvertFunc.F netcdf_db_ents.F netcdf_ents.F \
	netcdf_ts_ents.F in_ents_netcdf.F

FLAGS = $(FLAGR8) $(GOLDOPTIM)
# DOPTS = $(DEFINE)dosc
DOPTS = 
LIBNAME = libents
LOCAL_BIN_DIR = ../../bin

CMN = genie_ents.cmn var_ents.cmn Makefile ../../../genie-main/makefile.arc

# AY (05/05/04) : mv below replaced by cp to prevent needless recompilation

all : $(OBJS) $(CMN)

.PHONY: all clean tags
clean:
	\rm -f *.$(OBJ_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

# dependencies

### VNL
ConvertFunc.$(OBJ_EXT): ConvertFunc.F $(CMN)
netcdf_db_ents.$(OBJ_EXT): netcdf_db_ents.F $(CMN)
netcdf_ents.$(OBJ_EXT): netcdf_ents.F $(CMN) 
netcdf_ts_ents.$(OBJ_EXT): netcdf_ts_ents.F $(CMN)
in_ents_netcdf.$(OBJ_EXT): in_ents_netcdf.F $(CMN)
in_ents_ascii.$(OBJ_EXT): in_ents_ascii.F $(CMN)
out_ents.$(OBJ_EXT): out_ents.F $(CMN)
setup_ents.$(OBJ_EXT): setup_ents.F $(CMN)
initialise_ents.$(OBJ_EXT): initialise_ents.F $(CMN)
screen_diags.$(OBJ_EXT): screen_diags.F
carbon.$(OBJ_EXT): carbon.F
carbt_diags.$(OBJ_EXT): carbt_diags.F
sealevel.$(OBJ_EXT): sealevel.F
physt_diags.$(OBJ_EXT): physt_diags.F
annav_diags.$(OBJ_EXT): annav_diags.F 
ents.$(OBJ_EXT): ents.F $(CMN)
entsdiagosc.$(OBJ_EXT): entsdiagosc.F

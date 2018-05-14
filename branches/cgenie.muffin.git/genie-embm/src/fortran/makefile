#
# $Id$
#

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS) $(FLAGR8)

.SUFFIXES: .$(OBJ_EXT) .f

.F.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) $(DOPTS) \
	$(COMPILEONLY) $*.F

.f.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) \
	$(COMPILEONLY) $*.f

.f:
	$(F77) $(LOCALFLAGS) \
	$(OBJ_FLAG) $<

OBJS = 	diag3.$(OBJ_EXT) diaga.$(OBJ_EXT) diagend_embm.$(OBJ_EXT) \
	diagosc_embm.$(OBJ_EXT) embm.$(OBJ_EXT) end_embm.$(OBJ_EXT) \
	initialise_embm.$(OBJ_EXT) inm_embm.$(OBJ_EXT) inm_netcdf_embm.$(OBJ_EXT) \
	netcdf_embm.$(OBJ_EXT) outm_embm.$(OBJ_EXT) outm_netcdf_embm.$(OBJ_EXT) \
	outm_surf.$(OBJ_EXT) ocean_alb.$(OBJ_EXT) radfor.$(OBJ_EXT) \
	readroff.$(OBJ_EXT) surflux.$(OBJ_EXT) \
	tstepa.$(OBJ_EXT) tstipa.$(OBJ_EXT) err_embm.$(OBJ_EXT) \
	rmsnorm_embm_T.$(OBJ_EXT) \
	rmsnorm_embm_q.$(OBJ_EXT) \
        field_interp.$(OBJ_EXT) \
        diag_embm_T_annual_av.$(OBJ_EXT) \
        diag_embm_q_annual_av.$(OBJ_EXT) \
        diag_embm_r_annual_av.$(OBJ_EXT)

FBJS = 	diag3.f diaga.f diagend_embm.F diagosc_embm.F embm.F end_embm.F \
	initialise_embm.F inm_embm.f inm_netcdf_embm.F netcdf_embm.F outm_embm.f \
	outm_netcdf_embm.F outm_surf.f ocean_alb.F radfor.F readroff.f \
	surflux.F \
	tstepa.f tstipa.f \
        err_embm.F \
	rmsnorm_embm_T.F \
	rmsnorm_embm_q.F \
        field_interp.F \
        diag_embm_T_annual_av.F \
        diag_embm_q_annual_av.F \
        diag_embm_r_annual_av.F

#FLAGS = -r8 -O
FLAGS = $(FLAGR8) $(GOLDOPTIM)
DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos
#DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos
#DOPTS = $(DEFINE)dimpa $(DEFINE)ebatmos $(DEFINE)clock
LIBNAME = libembm
LOCAL_BIN_DIR = ../../bin

CMN = embm.cmn netcdf_embm.cmn netcdf_grid.cmn makefile ../../../genie-main/makefile.arc

all : $(OBJS) $(CMN)

.PHONY: all clean tags
clean:
	\rm -f *.$(OBJ_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

# dependencies
diag3.$(OBJ_EXT): diag3.f $(CMN)
diaga.$(OBJ_EXT): diaga.f $(CMN)
diagend_embm.$(OBJ_EXT): diagend_embm.F $(CMN)
diagosc_embm.$(OBJ_EXT): diagosc_embm.F $(CMN)
end_embm.$(OBJ_EXT): end_embm.F $(CMN)
embm.$(OBJ_EXT): embm.F $(CMN)
err_embm.$(OBJ_EXT): err_embm.F $(CMN)
rmsnorm_embm_T.$(OBJ_EXT): rmsnorm_embm_T.F $(CMN)
rmsnorm_embm_q.$(OBJ_EXT): rmsnorm_embm_q.F $(CMN)
diag_embm_T_annual_av.$(OBJ_EXT): diag_embm_T_annual_av.F $(CMN)
diag_embm_q_annual_av.$(OBJ_EXT): diag_embm_q_annual_av.F $(CMN)
diag_embm_r_annual_av.$(OBJ_EXT): diag_embm_r_annual_av.F $(CMN)
initialise_embm.$(OBJ_EXT): initialise_embm.F $(CMN)
inm_embm.$(OBJ_EXT): inm_embm.f $(CMN)
inm_netcdf_embm.$(OBJ_EXT): inm_netcdf_embm.F $(CMN) netcdf.inc
netcdf_embm.$(OBJ_EXT): netcdf_embm.F $(CMN)
outm_embm.$(OBJ_EXT): outm_embm.f $(CMN)
outm_netcdf_embm.$(OBJ_EXT): outm_netcdf_embm.F $(CMN) netcdf.inc
outm_surf.$(OBJ_EXT): outm_surf.f $(CMN)
radfor.$(OBJ_EXT): radfor.F $(CMN)
readroff.$(OBJ_EXT): readroff.f $(CMN)
ocean_alb.$(OBJ_EXT): ocean_alb.F $(CMN)
surflux.$(OBJ_EXT): surflux.F $(CMN)
tstepa.$(OBJ_EXT): tstepa.f $(CMN)
tstipa.$(OBJ_EXT): tstipa.f $(CMN)
#plotgraph.$(OBJ_EXT): plotgraph.f $(CMN)
field_interp$(OBJ_EXT): field_interp.F $(CMN)

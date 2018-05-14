
ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS= $(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS) $(FLAGR8)

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

OBJS = diagend_seaice.$(OBJ_EXT) diagosc_sic.$(OBJ_EXT) diagsic.$(OBJ_EXT) \
		diag4.$(OBJ_EXT) end_seaice.$(OBJ_EXT) gold_seaice.$(OBJ_EXT) \
		initialise_seaice.$(OBJ_EXT) inm_netcdf_sic.$(OBJ_EXT) inm_seaice.$(OBJ_EXT) \
		netcdf_sic.$(OBJ_EXT) outm_netcdf_sic.$(OBJ_EXT) outm_seaice.$(OBJ_EXT) \
		tstepsic.$(OBJ_EXT) tstipsic.$(OBJ_EXT) \
		diag_goldsteinseaice_ice_annual_av.$(OBJ_EXT) \
		surflux_goldstein_seaice.$(OBJ_EXT)

FBJS = diagend_seaice.f diagosc_sic.F diagsic.f diag4.f end_seaice.F \
		gold_seaice.F initialise_seaice.F inm_netcdf_sic.F inm_seaice.f netcdf_sic.F \
		outm_netcdf_sic.F outm_seaice.f tstepsic.f tstipsic.f \
		diag_goldsteinseaice_ice_annual_av.f \
		surflux_goldstein_seaice.F

#FLAGS = -r8 -O -static
FLAGS = $(FLAGR8) $(GOLDOPTIM)
DOPTS = $(DEFINE)ebatmos

CMN = seaice.cmn netcdf_sic.cmn netcdf_grid.cmn makefile ../../../genie-main/makefile.arc

all : $(OBJS) $(CMN)

.PHONY : all clean tags
clean:
	\rm -f *.$(OBJ_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

# dependencies
diagend_seaice.$(OBJ_EXT): diagend_seaice.f $(CMN)
diagosc_sic.$(OBJ_EXT): diagosc_sic.F $(CMN)
diagsic.$(OBJ_EXT): diagsic.f $(CMN)
diag4.$(OBJ_EXT): diag4.f $(CMN)
end_seaice.$(OBJ_EXT): end_seaice.F $(CMN)
gold_seaice.$(OBJ_EXT): gold_seaice.F $(CMN)
initialise_seaice.$(OBJ_EXT): initialise_seaice.F $(CMN)
inm_netcdf_sic.$(OBJ_EXT): inm_netcdf_sic.F $(CMN) netcdf.inc
inm_seaice.$(OBJ_EXT): inm_seaice.f $(CMN)
netcdf_sic.$(OBJ_EXT): netcdf_sic.F $(CMN)
outm_netcdf_sic.$(OBJ_EXT): outm_netcdf_sic.F $(CMN) netcdf.inc
outm_seaice.$(OBJ_EXT): outm_seaice.f $(CMN)
tstepsic.$(OBJ_EXT): tstepsic.f $(CMN)
tstipsic.$(OBJ_EXT): tstipsic.f $(CMN)
#plotgraph.$(OBJ_EXT): plotgraph.f $(CMN)
diag_goldsteinseaice_ice_annual_av.$(OBJ_EXT): diag_goldsteinseaice_ice_annual_av.F $(CMN)

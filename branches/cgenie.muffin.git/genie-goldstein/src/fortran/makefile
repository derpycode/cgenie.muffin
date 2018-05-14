#
# $Id$
#
# AY (23/06/04) : surf_ocn_sic.F added to subroutine list

.SUFFIXES: .$(OBJ_EXT) .f

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FPPFLAGS) $(FFLAGS) $(F77FLAGS) $(FLAGS) $(LOC_INC_FLAGS) $(FLAGR8)


.F.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) $(DOPTS) \
	$(COMPILEONLY) $*.F

.f.$(OBJ_EXT):
	$(F77) $(LOCALFLAGS) \
	$(COMPILEONLY) $*.f

.f:
	$(F77) $(LOCALFLAGS) \
	$(OBJ_FLAG) $<

OBJS = co.$(OBJ_EXT) diag.$(OBJ_EXT) diag2.$(OBJ_EXT) diagend.$(OBJ_EXT) \
		diagopsi.$(OBJ_EXT) diagosc.$(OBJ_EXT) drgset.$(OBJ_EXT) \
                ediff.$(OBJ_EXT) \
		end_goldstein.$(OBJ_EXT) eos.$(OBJ_EXT) \
                gold_ocnsic_avg.$(OBJ_EXT) \
		goldstein.$(OBJ_EXT) initialise_goldstein.$(OBJ_EXT) inm.$(OBJ_EXT) \
		inm_netcdf.$(OBJ_EXT) invert.$(OBJ_EXT) island.$(OBJ_EXT) \
                jbar.$(OBJ_EXT) krausturner.$(OBJ_EXT) \
                matinv_gold.$(OBJ_EXT) netcdf.$(OBJ_EXT) \
		outm.$(OBJ_EXT) outm_netcdf.$(OBJ_EXT) outm_surf_ocn_sic.$(OBJ_EXT) \
		surf_ocn_sic.$(OBJ_EXT) tstepo.$(OBJ_EXT) tstipo.$(OBJ_EXT) \
                tstepo_flux.$(OBJ_EXT) \
		ubarsolv.$(OBJ_EXT) velc.$(OBJ_EXT) wind.$(OBJ_EXT) \
                goldstein_delayfluxes.$(OBJ_EXT) \
                get_hosing.$(OBJ_EXT) err_gold.$(OBJ_EXT) \
                rmsnorm_goldstein_S.$(OBJ_EXT) \
                rmsnorm_goldstein_T.$(OBJ_EXT) \
                diag_goldstein_T_annual_av.$(OBJ_EXT) \
                diag_goldstein_Tsurf_annual_av.$(OBJ_EXT) \
                diag_goldstein_S_annual_av.$(OBJ_EXT) \
                diag_goldstein_Ssurf_annual_av.$(OBJ_EXT) \
                diag_goldstein_circulation_annual_av.$(OBJ_EXT) \
                gwt.$(OBJ_EXT)

FLAGS = $(FLAGR8) $(GOLDOPTIM) $(BOUNDS_FLAGS)
DOPTS = $(DEFINE)ebatmos
#DOPTS = $(DEFINE)disles $(DEFINE)diso $(DEFINE)ebatmos
#DOPTS = $(DEFINE)diso $(DEFINE)ebatmos $(DEFINE)clock

CMN = ocean.cmn netcdf.cmn netcdf_grid.cmn makefile ../../../genie-main/makefile.arc

all : $(OBJS) $(CMN)

.PHONY: clean tags all
clean:
	\rm -f *.$(OBJ_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

# dependencies
# BEWARE! THESE DEPENDECIES ARE OVER-PRESCRIBED AND 
#   SOME MAY BE INCOMPLETE.
# THIS SHOULD BE SORTED ASAP.
co.$(OBJ_EXT): co.F $(CMN)
diag.$(OBJ_EXT): diag.f $(CMN)
diag2.$(OBJ_EXT): diag2.f $(CMN)
diagend.$(OBJ_EXT): diagend.F $(CMN)
diagopsi.$(OBJ_EXT): diagopsi.f $(CMN)
diagosc.$(OBJ_EXT): diagosc.F $(CMN)
drgset.$(OBJ_EXT): drgset.f $(CMN)
end_goldstein.$(OBJ_EXT): end_goldstein.F $(CMN)
err_gold.$(OBJ_EXT): err_gold.F $(CMN)
rmsnorm_goldstein_T.$(OBJ_EXT): rmsnorm_goldstein_T.F $(CMN)
rmsnorm_goldstein_S.$(OBJ_EXT): rmsnorm_goldstein_S.F $(CMN)
diag_goldstein_T_annual_av.$(OBJ_EXT): diag_goldstein_T_annual_av.F $(CMN)
diag_goldstein_Tsurf_annual_av.$(OBJ_EXT): diag_goldstein_Tsurf_annual_av.F $(CMN)
diag_goldstein_S_annual_av.$(OBJ_EXT): diag_goldstein_S_annual_av.F $(CMN)
diag_goldstein_Ssurf_annual_av.$(OBJ_EXT): diag_goldstein_Ssurf_annual_av.F $(CMN)
diag_goldstein_circulation_annual_av.$(OBJ_EXT): diag_goldstein_circulation_annual_av.F $(CMN)
get_hosing.$(OBJ_EXT): get_hosing.F $(CMN)
gold_ocnsic_avg.$(OBJ_EXT): gold_ocnsic_avg.F $(CMN)
goldstein.$(OBJ_EXT): goldstein.F $(CMN)
initialise_goldstein.$(OBJ_EXT): initialise_goldstein.F $(CMN)
inm.$(OBJ_EXT): inm.f $(CMN)
inm_netcdf.$(OBJ_EXT): inm_netcdf.F $(CMN) netcdf.inc
invert.$(OBJ_EXT): invert.f $(CMN)
island.$(OBJ_EXT): island.f $(CMN)
jbar.$(OBJ_EXT): jbar.f $(CMN)
matinv_gold.$(OBJ_EXT): matinv_gold.f $(CMN)
netcdf.$(OBJ_EXT): netcdf.F $(CMN)
outm.$(OBJ_EXT): outm.f $(CMN)
outm_netcdf.$(OBJ_EXT): outm_netcdf.F $(CMN) netcdf.inc
outm_surf_ocn_sic.$(OBJ_EXT): outm_surf_ocn_sic.f $(CMN)
surf_ocn_sic.$(OBJ_EXT): surf_ocn_sic.F $(CMN)
tstepo.$(OBJ_EXT): tstepo.F $(CMN)
tstipo.$(OBJ_EXT): tstipo.F $(CMN)
tstepo_flux.$(OBJ_EXT): tstepo_flux.F $(CMN)
ubarsolv.$(OBJ_EXT): ubarsolv.f $(CMN)
velc.$(OBJ_EXT): velc.f $(CMN)
wind.$(OBJ_EXT): wind.f $(CMN)
gwt.$(OBJ_EXT): gwt.f $(CMN)
#plotgraph.$(OBJ_EXT): plotgraph.f $(CMN)

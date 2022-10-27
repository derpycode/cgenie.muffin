# *******************************************************************
# *** muffin.CB.GIteiiva.BASESFeTDTL.1p2tau.SPIN ********************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# (climate responding to changing pCO2)
ea_36=y
#
# *** BIOLOGICAL NEW PRODUCTION *************************************
#
# biological scheme ID string
bg_par_bio_prodopt="bio_PFe"
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=0.10E-06
# [Fe] M-M half-sat value (mol kg-1)
bg_par_bio_c0_Fe=0.10E-09
# biological uptake time-scale
bg_par_bio_tau=60.0
#
# *** ORGANIC MATTER EXPORT RATIOS **********************************
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# *** INORGANIC MATTER EXPORT RATIOS ********************************
#
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.0485
# exponent for modifier of CaCO3:POC export ratio
bg_par_bio_red_POC_CaCO3_pP=0.7440
#
# *** REMINERALIZATION **********************************************
#
# DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=0.5
# *** recommended ***************************************************
# set 'instantaneous' water column remineralziation
bg_par_bio_remin_sinkingrate_physical=9.9E9
bg_par_bio_remin_sinkingrate_reaction=125.0
# *** common settings ***********************************************
# initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=0.0557
# depth of remineralization or particulate organic matter
bg_par_bio_remin_POC_eL1=589.9451
# remineralization length #2 for POC
bg_par_bio_remin_POC_eL2=1000000.0
# initial fractional abundance of CaCO3 component #2
bg_par_bio_remin_CaCO3_frac2=0.45
# depth of remineralization or CaCO3
bg_par_bio_remin_CaCO3_eL1=1.8905e+003
# remineralization length #2 for CaCO3
bg_par_bio_remin_CaCO3_eL2=1000000.0
#
# *** IRON CYCLING **************************************************
#
# iron tracer scheme
# NOTE: the base-config requires TFe and TL tracers
bg_opt_geochem_Fe='hybrid'
# exponent for aeolian Fe solubility [use 1.0 for uniform solubility]
bg_par_det_Fe_sol_exp=0.500
# aeolian Fe solubility [Albani tuning]
bg_par_det_Fe_sol=0.002441
# modifier of the scavenging rate of dissolved Fe [Albani tuning]
bg_par_scav_Fe_sf_POC=0.2250
# no scavenged regeneration
bg_par_scav_fremin=0.0
# return POFe
bg_ctrl_bio_NO_fsedFe=.false.
# Variable Fe:C
bg_ctrl_bio_red_fixedFetoC=.false.
# adjust pK'(FeL)
bg_par_K_FeL_pP=11.0
#(max) C/Fe organic matter ratio
bg_par_bio_red_POFe_POC=250000.0
#[FeT] dependent Fe:C ratio [Ridgwell, 2001] -- power
bg_par_bio_FetoC_pP=-0.4225
#[FeT] dependent Fe:C ratio [Ridgwell, 2001] -- scaling
bg_par_bio_FetoC_K=103684.0
#[FeT] dependent Fe:C ratio [Ridgwell, 2001] -- constant
bg_par_bio_FetoC_C=0.0
#
# *** DATA SAVING ***************************************************
#
# BASIC + biology + tracer + proxy diagnostics
bg_par_data_save_level=7
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
#
# *** FORCINGS ******************************************************
#
# specify forcings
bg_par_forcing_name="GIteiiva.RpCO2_Rp13CO2.Fsal_SUR.Albani.21ka"
bg_par_atm_force_scale_val_3=193.0E-06
bg_par_atm_force_scale_val_4=-6.46
# 14C production -- mean of 19-23 ka from Hain et al.
ac_par_atm_F14C = 576.5483771
# Orbital parameters
ea_par_orbit_osce=0.018994 # eccentricity
ea_par_orbit_oscsob=0.389911 # sine of obliquity
ea_par_orbit_oscgam=114.42 # longitude of perihelion (degrees)
#
# *** MISC **********************************************************
#
# preformed!
bg_ctrl_bio_preformed=.true.
# add ventillation tracers
bg_ctrl_force_ocn_age1=.true.
# add brine rejection SH limit
bg_par_misc_brinerejection_jmax=19
# wind-stress
go_13=1.3
ea_11=1.3
# *** recommended ***************************************************
# maximum time-scale to geochemical reaction completion (days)
bg_par_bio_geochem_tau=45.0
# extend solubility and geochem constant T range (leave S range as default)
gm_par_geochem_Tmin  = -2.0
gm_par_geochem_Tmax  = 45.0
gm_par_carbchem_Tmin = -2.0
gm_par_carbchem_Tmax = 45.0
#
# *** TUNING ********************************************************
#
# parameter: bg_par_misc_brinerejection_frac -- default value (1) modified by factor: 0.2
bg_par_misc_brinerejection_frac=0.2
# parameter: bg_par_ocn_force_scale_val_2 -- default value (1) modified by factor: 0.05
bg_par_ocn_force_scale_val_2=0.05
#
# *** INITIAL CONDITIONS ********************************************
#
# modern DIC (2.244E-03) + 1%
bg_ocn_init_3=0.002275818239634
# modern 13C DIC
bg_ocn_init_4=0.4
# modern ALK (2.363E-03) + 1%
bg_ocn_init_12=0.002396505570523
# modern PO4 (2.159E-06) + 1%
bg_ocn_init_8=2.189613003283920e-06
# modern SAL + 0.33 PSU
go_saln0=35.23
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

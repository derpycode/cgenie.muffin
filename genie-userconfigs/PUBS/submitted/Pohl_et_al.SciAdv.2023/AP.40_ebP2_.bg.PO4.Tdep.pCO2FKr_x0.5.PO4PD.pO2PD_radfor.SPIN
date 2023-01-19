# *******************************************************************
# *** USERCONFIG.PALEO.BIOGEM.PO4.SPIN ******************************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=n
ea_radfor_scl_co2 = 1.381294964028777
#
# solar constant (Gough, 1981)
ma_genie_solar_constant=1363.4
#
# *** BIOLOGICAL NEW PRODUCTION *************************************
#
# *** recommended ***************************************************
# biological scheme ID string
bg_par_bio_prodopt="bio_P"
# biological uptake time-scale
bg_par_bio_tau=63.3827
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=0.10E-06
# *** published paleo configuration *********************************
###bg_par_bio_prodopt="1N1T_PO4MM"
###bg_par_bio_k0_PO4=8.9876e-006
###bg_par_bio_c0_PO4=8.9369e-007
# *** Crichton et al. [2020] calibration ****************************
# NOTE: light limitation term is different in biological scheme 1N1T_PO4MM_Tdep compared to bio_P
###bg_par_bio_prodopt='1N1T_PO4MM_Tdep'
###bg_par_bio_mu1=10
###bg_par_bio_c0_PO4=8.9369e-007
#
# *** ORGANIC MATTER EXPORT RATIOS **********************************
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# *** INORGANIC MATTER EXPORT RATIOS ********************************
#
# fixed CaCO3:POC
bg_opt_bio_CaCO3toPOCrainratio='FIXED'
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.2
# *** alternative for > 200 Ma **************************************
###bg_par_bio_red_POC_CaCO3=0.0
#
# *** REMINERALIZATION **********************************************
#
# *** recommended ***************************************************
# set 'instantaneous' water column remineralziation
bg_par_bio_remin_sinkingrate_physical=9.9E9
bg_par_bio_remin_sinkingrate_reaction=125.0
# *** common settings ***********************************************
# DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=0.5
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
# *** Crichton et al. [2020] temeprature-dependent remin ************
bg_ctrl_bio_remin_POC_fixed=.false.
bg_par_bio_remin_POC_K1=9.0E11
bg_par_bio_remin_POC_Ea1=54000.0
bg_par_bio_remin_POC_K2=1.0E14
bg_par_bio_remin_POC_Ea2=80000.0
bg_par_bio_remin_POC_frac2=0.008
#
# *** DATA SAVING ***************************************************
#
# BASIC + biology + tracer + proxy diagnostics
bg_par_data_save_level=16
# *** recommended ***************************************************
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_EVERY001000.dat'
#
# *** FORCINGS ******************************************************
#
# specify forcings -- restoring of atmopsheric pCO2 and d13C
bg_par_forcing_name="pyyyyz.RpO2"
bg_par_atm_force_scale_val_6=0.2095 # 0.2095 = 1 PAL O2
#
# mean ocean PO4 -- 2.159E-06 is the default, present-day value
bg_ocn_init_8=2.159E-06
# 
# *** MISC **********************************************************
#
# age tracer, automatic method
bg_ctrl_force_ocn_age=.true.
# disable carbonate chemistry updating for time-slices
bg_ctrl_data_save_slice_carb_update=.false.
# number of pH solution iterations
gm_par_carbchem_pH_iterationmax=5000
# re-seed pH?
gm_ctrl_carbchem_pHseed_retry=.true.
#
# *** recommended ***************************************************
# maximum time-scale to geochemical reaction completion (days)
bg_par_bio_geochem_tau=90.0
# extend solubility and geochem constant T range (leave S range as default)
gm_par_geochem_Tmin  = -2.0
gm_par_geochem_Tmax  = 45.0
gm_par_carbchem_Tmin = -2.0
gm_par_carbchem_Tmax = 45.0
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

# *******************************************************************
# * USERCONFIG for JG.Scot390M.02X_1.25PO4_0.7O2.0_24_0.SPIN *
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=y
#
# *** BIOLOGICAL NEW PRODUCTION *************************************
#
# *** published paleo configuration *********************************
bg_par_bio_prodopt="1N1T_PO4MM"
bg_par_bio_k0_PO4=8.9876e-006
bg_par_bio_c0_PO4=8.9369e-007
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
#bg_par_bio_red_POC_CaCO3=0.200
# *** alternative for > 200 Ma **************************************
bg_par_bio_red_POC_CaCO3=0.0
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
#
# *** DATA SAVING ***************************************************
#
# BASIC + biology + tracer + proxy diagnostics
bg_par_data_save_level=16
bg_par_infile_slice_name='save_timeslice_EVERY001000.dat'
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# *** FORCINGS ******************************************************
#
# specify forcings -- restoring of atmospheric pCO2 and d13C
bg_par_forcing_name="JG.DEVO_0.7O2_02X"
bg_par_atm_force_scale_val_3=280.0E-06
bg_par_atm_force_scale_val_4=-6.5
bg_par_atm_force_scale_val_6=0.2095
#
# *** MISC **********************************************************
#
# mean ocean PO4
#bg_ocn_init_8=2.159E-06 # default PD value included in the base config
bg_ocn_init_8=2.69875E-06
#
# age tracer, automatic method
bg_ctrl_force_ocn_age=.true.
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
# Orbital parameters -- modern set => adjust as necessary
ea_opt_orbit_old=.false. # New model of radiation forcing: old = false.
ea_par_orbit_osce=0 # eccentricity
ea_par_orbit_oscobl=0.41887902047863906 # obliquity in rads
ea_par_orbit_osclonperi=0.0 # longitude of perihelion in rads
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

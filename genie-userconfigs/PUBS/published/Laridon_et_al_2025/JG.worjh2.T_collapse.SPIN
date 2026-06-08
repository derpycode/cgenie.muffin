# *******************************************************************
# *** USERCONFIG for JG.worjh2.T_collapse.SPIN **********************
# *******************************************************************
#
# --- CLIMATE ---------------------------------------------------------
#
# set CO2 climate feedback
ea_36=y
#
# --- BIOLOGICAL NEW PRODUCTION ---------------------------------------
#
# maximum rate of conversion of dissolved PO4 into organic matter by phytoplankton (mol kg-1 yr-1)
bg_par_bio_k0_PO4=8.9876e-006
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=8.9369e-007
#
# --- ORGANIC MATTER EXPORT RATIOS ------------------------------------
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS ----------------------------------
#
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.0485
# exponent for modifier of CaCO3:POC export ratio
bg_par_bio_red_POC_CaCO3_pP=0.7440
#
# --- REMINERALIZATION ------------------------------------------------
#
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
# --- DATA SAVING -----------------------------------------------------
#
# ! BASIC + biology + geochem diagnostics
bg_par_data_save_level=16
bg_par_infile_slice_name="save_timeslice_EVERY25.dat"
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS --------------------------------------------------------
#
# specify forcings
bg_par_forcing_name="JG.T_collapse"
#bg_par_atm_force_scale_val_3=278.0E-06 # Complete values in forcing file!
bg_par_atm_force_scale_val_4=-6.5
#
# --- MISC ------------------------------------------------------------
#
# age tracer, automatic method
bg_ctrl_force_ocn_age=.true.
#
# orbital forcing
# 
ea_opt_orbit_old=.false. # New model of radiation forcing: old = false.
ea_par_orbit_osce=0 # eccentricity
ea_par_orbit_oscobl=0.40142572795869574 # obliquity in rads
ea_par_orbit_osclonperi=0 # longitude of perihelion in rads
#  
ea_dosc=.true.
go_dosc=.true.
gs_dosc=.true.
#
# --- END -------------------------------------------------------------
#
#

#####################################################################
# USERCONFIG.TdepPO4.PALEO.SPIN
# generic template paleo user-config
# based loosely on Meyer et al. [2016]
#####################################################################
#
# --- CLIMATE -------------------------------------------------------
#
# set climate feedback
# => radiative forcing is a function of atmospheric pCO2
ea_36=y
#
# --- BIOLOGICAL NEW PRODUCTION -------------------------------------
#
# biological scheme ID string
# bio_P ==  single (PO4) nutrient, T-dependent uptake
bg_par_bio_prodopt="bio_P"
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=0.10E-06
# biological uptake time-scale
bg_par_bio_tau=63.3827
#
# --- ORGANIC MATTER EXPORT RATIOS ----------------------------------
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS --------------------------------
#
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
# 0.0 == deep-time (>150 ca. Ma) for no pelagic calcifiers
# 0.2 == generic shallow time globally uniform fixed value
bg_par_bio_red_POC_CaCO3=0.2
# exponent for modifier of CaCO3:POC export ratio
# 0.0 == globally uniform
bg_par_bio_red_POC_CaCO3_pP=0.0
#
# --- REMINERALIZATION ----------------------------------------------
#
# NOTE: by default -- fixed double exponential formulation for POC remin
# DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=0.5
#initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=0.0557
# depth of remineralization or particulate organic matter
bg_par_bio_remin_POC_eL1=589.9451
#remineralization length #2 for POC
bg_par_bio_remin_POC_eL2=1000000.0
#initial fractional abundance of CaCO3 component #2
bg_par_bio_remin_CaCO3_frac2=0.45
# depth of remineralization or CaCO3
bg_par_bio_remin_CaCO3_eL1=1.8905e+003
#remineralization length #2 for CaCO3
bg_par_bio_remin_CaCO3_eL2=1000000.0
#
# --- DATA SAVING ---------------------------------------------------
#
bg_par_data_save_level=15
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS ------------------------------------------------------
#
# specify forcings
# pCO2 + 13C
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=278.0E-06 # x1 CO2
bg_par_atm_force_scale_val_4=-6.5
## pCO2 + 13C + pO2
#bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2.RpO2"
#bg_par_atm_force_scale_val_3=278.0E-06 # x1 CO2
#bg_par_atm_force_scale_val_4=-6.5
#bg_par_atm_force_scale_val_6=0.2095 # 1 PAL O2
#
# --- END -----------------------------------------------------------
#
#

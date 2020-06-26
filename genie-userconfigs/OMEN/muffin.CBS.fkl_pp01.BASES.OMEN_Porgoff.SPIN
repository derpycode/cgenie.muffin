#####################################################################
# USERCONFIG.
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
# Tie ALK with POC (rather than POP) Email 08.08.2017
bg_ctrl_bio_red_ALKwithPOC=.true.
#
# --- SEDIMENTS ------------------------------------------------
# sediment diagenesis options
sg_par_sed_diagen_CaCO3opt="NONE"
sg_ctrl_sed_bioturb=.false.
sg_ctrl_sed_bioturb_Archer=.false.
sg_par_n_sed_mix=20
# set invarient surface bioturbation mixing rate
sg_par_sed_mix_k_sur_max=0.15
sg_par_sed_mix_k_sur_min=0.15
# additional detrital flux (g cm-2 kyr-1)
sg_par_sed_fdet=0.180
#
# Use OMEN-SED
sg_par_sed_diagen_Corgopt = 'huelse2016'
# global OM degradation rate constants (1/yr)
sg_par_sed_huelse2017_k1=0.0065
sg_par_sed_huelse2017_k2=0.0001
sg_par_sed_huelse2017_k2_anoxic=1E-6
# Porg burial/regeneration
# sg_par_sed_huelse2017_sim_P_loss = .true.	# general loss, default being just sulf-OM
# sg_par_sed_huelse2017_sim_P_loss_pres_fracC = .true.	# preserve same as Corg
# DH: was:
# increase max allowed iterations for solving for Corg oxidation
# sg_par_sed_archer1991_iterationmax=40

# --- WEATHERING -----------------------------------------------
#
# set a OPEN system
bg_ctrl_force_sed_closedsystem=.false.
# set prescribed weathering rate
rg_par_weather_CaSiO3=0.0
rg_par_weather_CaSiO3_fracC=0.0
rg_par_weather_CaSiO3_fracP=0.0E+010 	# modern value: 3.0E+010 mol P yr^-1, Wallmann (2010) riverine dissolved P input
rg_par_weather_CaCO3=0.0E+14
rg_par_weather_CaCO3_d13C=0.0
#
#
# --- DATA SAVING -----------------------------------------------------
#
bg_par_data_save_level=8
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS ------------------------------------------------------
#
# specify forcings
# pCO2 + 13C + O2
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2_RpO2"
# atm CO2:
bg_par_atm_force_scale_val_3=278.0E-06
bg_par_atm_force_scale_val_4=-6.5
# atm O2:
bg_par_atm_force_scale_val_6=0.2095
#
#
# --- MISC -----------------------------------------------------
#
# Initial atm O2 concentration 100% of modern
ac_atm_init_6=0.2095
# Initial oceanic PO4 concentration (1xmodern PO4)
bg_ocn_init_8=2.159E-06
#
# --- END -------------------------------------------------------------
#
#

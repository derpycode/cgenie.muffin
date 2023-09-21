#####################################################################
# TEMPLATE.TdepPO4.SPIN
# generic template paleo user-config
# based loosely on Meyer et al. [2016]
#####################################################################
#
# --- CLIMATE -------------------------------------------------------
#
# set climate feedback
# => radiative forcing is a function of atmospheric pCO2
ea_36=y
# solar constant (Gough, 1981)
ma_genie_solar_constant=1327.3
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
bg_par_bio_red_POC_CaCO3=0.0
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
# T-dep scheme added after EXAMPLE.p0055c.EleanorJohnetal2014.SPIN
# ... with 2 new params bg_par_bio_remin_sinkingrate_physical=9.9E9 and bg_par_bio_remin_sinkingrate_reaction=125.0 after Dom's mail
#bg_ctrl_bio_remin_POC_fixed=.false.
bg_par_bio_remin_sinkingrate_physical=9.9E9    # set to infinity for the temperature dependent remin-scheme
bg_par_bio_remin_sinkingrate_reaction=125.0     # play with this if you want particles to sink slower
#bg_par_bio_remin_POC_K1=9.0E11
#bg_par_bio_remin_POC_Ea1=55000.0
#bg_par_bio_remin_POC_K2=1.0E14
#bg_par_bio_remin_POC_Ea2=80000.0
#bg_par_bio_remin_POC_frac2=0.008
#
# --- DATA SAVING ---------------------------------------------------
#
bg_par_data_save_level=16
bg_par_infile_slice_name='save_timeslice_EVERY100_after20000.dat'
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS ------------------------------------------------------
#
# specify forcings
# NOTE: 'pyyyyz.RpCO2_Rp13CO2' to just force pCO2 and d13C
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2.RpO2"
bg_par_atm_force_scale_val_3=2240.0E-06
bg_par_atm_force_scale_val_4=-6.5
bg_par_atm_force_scale_val_6=0.2095 # 0.2095 = 1 PAL O2
#
# --- INITIAL CONDITIONS ---------------------------------------
#
# mean ocean PO4
#bg_ocn_init_8=2.159E-06 # default PD value included in the base config
bg_ocn_init_8=2.5907999999999998E-06
#
# --- MISC -----------------------------------------------------

# age tracer, automatic method
bg_ctrl_force_ocn_age=.true.

#### IODINE CYCLE CONTROLS ###
## adapted from Fig. S5 of LU et al., Science 
## set biological IO3 uptake
#bg_par_bio_red_POC_POI = 1.0E-4
## select basic reduction and oxidation options
#bg_opt_bio_remin_reduce_IO3toI='threshold'
#bg_opt_bio_remin_oxidize_ItoIO3='lifetime'
## set [O2] threshold (mol kg-1) for (complete) IO3 reduction to I
#bg_par_bio_remin_cO2_IO3toI=30.0E-6
## set I lifetime (yrs)
#bg_par_bio_remin_Ilifetime=40.0

# --- END -----------------------------------------------------------
#
#

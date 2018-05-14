#
#
# --- CLIMATE ---------------------------------------------------------
#
# enable climate feedback
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
# basic output options
bg_par_data_save_level=4
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS --------------------------------------------------------
#
# specify forcings -- restore to pre-industrial pCO2 (and d13C)
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=278.0E-06
bg_par_atm_force_scale_val_4=-6.5
#
# --- MISC ----------------------------------------------------------
#
### IODINE CYCLE CONTROLS ###
# set no biological IO3 uptake
bg_par_bio_red_POC_POI = 0.0
# select basic reduction and oxidation options
bg_opt_bio_remin_reduce_IO3toI='threshold'
bg_opt_bio_remin_oxidize_ItoIO3='lifetime'
# set [O2] threshold (mol kg-1) for (complete) IO3 reduction to I
bg_par_bio_remin_cO2_IO3toI=50.0E-6
# set I lifetime (yrs)
bg_par_bio_remin_Ilifetime=1.0
#
# --- END -------------------------------------------------------------
#
#

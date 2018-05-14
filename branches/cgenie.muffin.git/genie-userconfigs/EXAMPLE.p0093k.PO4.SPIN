#
#
# --- CLIMATE --------------------------------------------------
#
# set climate feedback
ea_36=y
#
# --- BIOLOGICAL NEW PRODUCTION --------------------------------
#
# maximum rate of conversion of dissolved PO4 into organic matter by phytoplankton (mol kg-1 yr-1)
bg_par_bio_k0_PO4=1.9582242E-06
#[PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=2.1989611E-07
#
# --- ORGANIC MATTER EXPORT RATIOS -----------------------------
#
#production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS ---------------------------
#
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.200
#exponent for modifier of CaCO3:POC export ratio
bg_par_bio_red_POC_CaCO3_pP=0.0
#
# --- REMINERALIZATION -----------------------------------------
#
#DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=0.5
#initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=6.4591110E-02
# depth of remineralization or particulate organic matter
bg_par_bio_remin_POC_eL1=550.5195
#remineralization length #2 for POC
bg_par_bio_remin_POC_eL2=1000000.0
#initial fractional abundance of CaCO3 component #2
bg_par_bio_remin_CaCO3_frac2=0.4325
# depth of remineralization or CaCO3
bg_par_bio_remin_CaCO3_eL1=1083.486
#remineralization length #2 for CaCO3
bg_par_bio_remin_CaCO3_eL2=1000000.0
#
# --- DATA SAVING ----------------------------------------------
#
bg_par_data_save_level=4
bg_ctrl_debug_lvl0=.false.
ma_debug_loop=1
#
# --- FORCINGS -------------------------------------------------
#
bg_par_forcing_name="pyyyyz_RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=556.0E-06
bg_par_atm_force_scale_val_4=-4.5
bg_par_windspeed_file="p0093k_windspeed.dat"
#re-scale gas transfer coeff to give ~0.058 mol m-2 yr-1 uatm-1 global mean air-sea coefficient (original: 0.310)
bg_par_gastransfer_a=1.385
#
# --- MISC -----------------------------------------------------
#
# INIT ALK
bg_ocn_init_12=2.000-03
#
# --- END ------------------------------------------------------
#
#

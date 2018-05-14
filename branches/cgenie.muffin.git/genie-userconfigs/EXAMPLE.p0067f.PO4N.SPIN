#
#
# --- CLIMATE --------------------------------------------------
#
# set climate feedback
ea_36=y
#
# --- BIOLOGICAL NEW PRODUCTION --------------------------------
#
#production with N2 fixation
bg_par_bio_prodopt='2N2T_PN_Tdep'
# dynN threshold constant
bg_par_bio_N2fixdyn=0.01
# N:P elemental ratio for diazotrophs
bg_par_bio_NPdiaz=40.0
#Max growth rate for non-diazotroph phyto (yr-1)
bg_par_bio_mu1=65.0
#Max growth rate for diazotroph phyto (yr-1)
bg_par_bio_mu2=2.0
#[PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=0.1E-06
#[N] M-M half-sat value (mol kg-1)
bg_par_bio_c0_N=1.6E-06
#Nstar offset
bg_par_bio_Nstar_offset=0.0
#
# --- ORGANIC MATTER EXPORT RATIOS -----------------------------
#
#production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS ---------------------------
#
# set fixed 0.2 CaCO3:POC rain ratio
bg_par_bio_red_POC_CaCO3=0.2
bg_par_bio_red_POC_CaCO3_pP=0.0
#
# --- REMINERALIZATION -----------------------------------------
#
#DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=1.0
#initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=0.055
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
#F denitrification O2 threshold (5E-6, Schmittner etal 2008)
bg_par_bio_remin_denitrO2thresh=30E-6
#
# --- DATA SAVING ----------------------------------------------
#
bg_par_data_save_level=4
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS -------------------------------------------------
#
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=1112.0E-06
bg_par_atm_force_scale_val_4=-4.9
# set alternative wind speed field
bg_par_windspeed_file="p0067f_windspeed.dat"
#re-scale gas transfer coeff to give ~0.058 mol m-2 yr-1 uatm-1 global mean air-sea coefficient (original: 0.310)
bg_par_gastransfer_a=0.4675
#
# --- MISC -----------------------------------------------------
#
# INIT ALK
bg_ocn_init_12=2.000-03
#
# --- END ------------------------------------------------------
#
#

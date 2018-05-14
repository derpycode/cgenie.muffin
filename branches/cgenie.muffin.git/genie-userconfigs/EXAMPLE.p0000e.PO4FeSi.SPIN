#
#
# --- CLIMATE --------------------------------------------------
#
#
# --- BIOLOGICAL NEW PRODUCTION --------------------------------
#
# biological scheme ID string
bg_par_bio_prodopt="bio_PFeSi"
# biological uptake time-scales
bg_par_bio_tau_sp=90.0
bg_par_bio_tau_nsp=180.0
# epsilon 30Si associated with opal formation [DEFAULT: -1.1]
bg_par_d30Si_opal_epsilon=0.0
#
# --- ORGANIC MATTER EXPORT RATIOS ----------------------------------
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS --------------------------------
#
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
# NOTE: ratio is ONLY appplied to non-siliceous plankton (NOT bulk) POC export
bg_par_bio_red_POC_CaCO3=0.10
# exponent for modifier of CaCO3:POC export ratio
bg_par_bio_red_POC_CaCO3_pP=0.7440
#
# --- REMINERALIZATION ----------------------------------------------
#
# DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=1.0
# initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=0.045
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
# OPAL
bg_ctrl_bio_remin_opal_fixed=.false.
bg_par_bio_remin_sinkingrate=125.0
#
# --- IRON ----------------------------------------------------------
#
# aeolian Fe solubility
bg_par_det_Fe_sol=0.00125
#exponent for aeolian Fe solubility [use 1.0 for uniform solubility]
bg_par_det_Fe_sol_exp=0.500
# modifier of the scavenging rate of dissolved Fe
bg_par_scav_Fe_sf_POC=1.338130
bg_par_scav_Fe_sf_CaCO3=0.000
bg_par_scav_Fe_sf_opal=0.000
bg_par_scav_Fe_sf_det=0.000
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
# --- TRACE METALS --------------------------------------------------
#
#
# --- SEDIMENTS -----------------------------------------------------
#
#
# --- WEATHERING ----------------------------------------------------
#
#
# --- DATA SAVING ---------------------------------------------------
#
# save frequency definitions
bg_par_infile_slice_name='save_timeslice.dat'
bg_par_infile_sig_name='save_sig.dat'
# additional biogeochem time-slice iagnostics
bg_ctrl_data_save_slice_diag=.true.
bg_ctrl_data_save_sig_diag=.true.
bg_ctrl_data_save_slice_misc=.true.
# full frequency of run-time reporting
bg_ctrl_debug_lvl0=.true.
# adjust on-screen output
ma_debug_loop=2
#
# --- FORCINGS ------------------------------------------------------
#
bg_ctrl_force_oldformat=.false.
bg_par_forcing_name='p0000e_FeMahowald2006modern_RpCO2_Rp13CO2'
bg_par_atm_force_scale_val_3=278.0E-6
bg_par_atm_force_scale_val_4=-6.5
# apply alternative wind-speed field
bg_par_windspeed_file="p0000e_windspeed.dat"
# re-scale gas transfer coeff to give ~0.058 mol m-2 yr-1 uatm-1 global mean air-sea coefficient (original: 0.310)
bg_par_gastransfer_a=0.38
#
# --- MISC ----------------------------------------------------------
#

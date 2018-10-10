#
#
# --- CLIMATE --------------------------------------------------
#
# set climate feedback
ea_36=y
#
# --- BIOLOGICAL NEW PRODUCTION --------------------------------
#
# biological scheme ID string
bg_par_bio_prodopt="bio_P"
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=0.10E-06
# biological uptake time-scale
bg_par_bio_tau=63.3827
#
# --- ORGANIC MATTER EXPORT RATIOS -----------------------------
#
#production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS ---------------------------
#
# SET NO PELAGIC CARBONATE PRODUCTION
bg_par_bio_red_POC_CaCO3=0.0
bg_par_bio_red_POC_CaCO3_pP=0.0
#
# --- REMINERALIZATION -----------------------------------------
#
#DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=0.5
#initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=0.0
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
# sulfide oxidation rate form
bg_opt_bio_remin_oxidize_H2StoSO4="OLD"
# sulfide oxidation rate constant [only used if rate law is linear]
bg_par_bio_remin_kH2StoSO4=1.0E07
#
# --- DATA SAVING ----------------------------------------------
#
# save frequency definitions
bg_par_infile_slice_name=save_timeslice.dat
bg_par_infile_sig_name=save_sig.dat
# SELECT: time-slice data save: Atmospheric (interface) composition (2D)?
bg_ctrl_data_save_slice_ocnatm=.false.
# SELECT: time-slice data save: Ocean composition (3D)?
bg_ctrl_data_save_slice_ocn=.true.
# SELECT: time-slice data save: Sediment (interface) composition (2D)?
bg_ctrl_data_save_slice_ocnsed=.true.
# SELECT: time-slice data save: Air-sea gas exchange (2D)?
bg_ctrl_data_save_slice_fairsea=.true.
# SELECT: time-slice data save: Ocean-atmosphere flux (2D)?
bg_ctrl_data_save_slice_focnatm=.true.
# SELECT: time-slice data save: Ocean-sediment flux (2D)?
bg_ctrl_data_save_slice_focnsed=.false.
# SELECT: time-slice data save: Sediment-ocean flux (2D)?
bg_ctrl_data_save_slice_fsedocn=.false.
# SELECT: time-slice data save: Biological fluxes (3D)?
bg_ctrl_data_save_slice_bio=.true.
# SELECT: additional biogeochem diagnostics
bg_ctrl_data_save_slice_diag=.true.
bg_ctrl_data_save_slice_misc=.true.
bg_ctrl_data_save_derived=.true.
bg_ctrl_data_save_sig_diag_redox_old=.false.
bg_ctrl_data_save_slice_diag_redox_old=.false.
bg_ctrl_bio_remin_redox_save=.true.
#bg_ctrl_carbchemupdate_full=.true.
# misc
bg_par_data_save_level=8
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS -------------------------------------------------
#
bg_par_forcing_name="pyyyyz_RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=2800.0E-06
bg_par_atm_force_scale_val_4=-6.5
bg_par_windspeed_file="p0251b_windspeed.dat"
#re-scale gas transfer coeff to give ~0.058 mol m-2 yr-1 uatm-1 global mean air-sea coefficient (original: 0.310)
bg_par_gastransfer_a=0.7096
#
# --- MISC -----------------------------------------------------
#
bg_ocn_init_8=2.159E-05             # PO4 [mol/kg]
bg_ocn_init_12=2.000E-03            # ALK [mol/kg]
#
# --- END ------------------------------------------------------
#

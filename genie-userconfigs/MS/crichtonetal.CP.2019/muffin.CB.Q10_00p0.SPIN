#
#
# --- CLIMATE -------------------------------------------------------
#
# set climate feedback
ea_36=y
#
# --- BIOLOGICAL NEW PRODUCTION -------------------------------------
#
# maximum rate of conversion of dissolved PO4 into organic matter by phytoplankton (mol kg-1 yr-1)
bg_par_bio_k0_PO4=8.9876e-006
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=8.9369e-007
#
# --- ORGANIC MATTER EXPORT RATIOS ----------------------------------
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS --------------------------------
#
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.0485
# exponent for modifier of CaCO3:POC export ratio
bg_par_bio_red_POC_CaCO3_pP=0.7440
#
# --- REMINERALIZATION ----------------------------------------------
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
# --- DATA SAVING ----------------------------------------------
#
bg_par_data_save_level=7
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
# reduce output
bg_ctrl_data_save_2d=.true.
bg_ctrl_data_save_3d=.false.
# save frequency
bg_par_infile_slice_name='save_timeslice_EVERY000100.dat'
bg_par_infile_sig_name='save_timeseries_EVERY000100.dat'
#
# --- FORCINGS -------------------------------------------------
#
# specify forcings
bg_par_forcing_name="Q_xxxxxx.RpCO2_Rp13CO2.Fsal_SUR"
bg_par_atm_force_scale_val_4=-6.232
# parameter: bg_par_atm_force_scale_val_3 -- default value modified by factor: 280
bg_par_atm_force_scale_val_3=0.000280
# parameter: bg_par_ocn_force_scale_val_2 -- default value modified by factor: 0.2
bg_par_ocn_force_scale_val_2=0.2
#
# --- END ------------------------------------------------------
#

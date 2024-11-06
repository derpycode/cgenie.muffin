# *******************************************************************
# *** I CYCLE *******************************************************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=y
#
# *** BIOLOGICAL NEW PRODUCTION *************************************
#
# *** Crichton et al. [2020] calibration ****************************
# NOTE: light limitation term is different in biological scheme 1N1T_PO4MM_Tdep compared to bio_P
bg_par_bio_prodopt='1N1T_PO4MM_Tdep'
bg_par_bio_mu1=10
bg_par_bio_c0_PO4=8.9369e-007
#
# *** ORGANIC MATTER EXPORT RATIOS **********************************
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# *** INORGANIC MATTER EXPORT RATIOS ********************************
#
# fixed CaCO3:POC
bg_opt_bio_CaCO3toPOCrainratio='FIXED'
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.200
#
# *** REMINERALIZATION **********************************************
#
# *** 16-level ocean as per Cao et al. [2009] ***********************
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
# *** Crichton et al. [2020] temeprature-dependent remin ************
bg_ctrl_bio_remin_POC_fixed=.false.
bg_par_bio_remin_sinkingrate=125.0
bg_par_bio_remin_POC_K1=9.0E11
bg_par_bio_remin_POC_Ea1=54000.0
bg_par_bio_remin_POC_K2=1.0E14
bg_par_bio_remin_POC_Ea2=80000.0
bg_par_bio_remin_POC_frac2=0.008
#
# *** DATA SAVING ***************************************************
#
# BASIC + biology + tracer + proxy diagnostics
bg_par_data_save_level=7
# *** recommended ***************************************************
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
#
# *** FORCINGS ******************************************************
#
# specify forcings -- restoring of atmopsheric pCO2 and d13C
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=556.0E-06
bg_par_atm_force_scale_val_4=-4.9
#
# *** MISC **********************************************************
#
# parameter: bg_ocn_init_8 -- default value (2.159e-06) modified by factor: 0.6
bg_ocn_init_8=1.2954e-06
#
# *** IODINE CYCLE **************************************************
#
# Iodine cycle parameter values as per Cheng et al. [in prep]
# >>> Simulation 4a <<<
# (1) I -> IO3
# select basic oxidation option
bg_opt_bio_remin_oxidize_ItoIO3='lifetime'
# set I lifetime (yrs)
bg_par_bio_remin_Ilifetime=50.0
# (2) IO3 -> 1
# select option for watercolumn reduction
bg_opt_bio_remin_reduce_IO3toI='inhibition'
# set [O2] inhibition threshold (mol kg-1)
bg_par_bio_remin_iO2_IO3toI=1.0e-6
# set [O2] inhibition rate constant (yr-1)
bg_par_bio_remin_kIO3toI=0.1
# (3) I/Ca
# set biological IO3 uptake (as a Redfield ratio with carbon)
bg_par_bio_red_POC_POI=1.5E-4
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

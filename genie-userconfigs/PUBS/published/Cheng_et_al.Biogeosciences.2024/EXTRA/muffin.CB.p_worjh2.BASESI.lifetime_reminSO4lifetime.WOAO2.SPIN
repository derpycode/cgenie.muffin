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
# *** recommended ***************************************************
# biological scheme ID string
bg_par_bio_prodopt="bio_P"
# biological uptake time-scale
bg_par_bio_tau=63.3827
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=0.10E-06
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
# *** common settings ***********************************************
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
# specify forcings -- generic forcing of atmopsheric pCO2 and d13C + restoring of ocean [O2] to WOA
bg_par_forcing_name="worjh2.RpCO2_Rp13CO2.RWOAO2"
bg_par_atm_force_scale_val_3=280.0E-06
bg_par_atm_force_scale_val_4=-6.5
#
# *** MISC **********************************************************
#
#
# *** IODINE CYCLE **************************************************
#
# Iodine cycle parameter values as per Cheng et al. [in prep]
# >>> Simulation 5c <<<
# (1) I -> IO3
# select basic oxidation option
bg_opt_bio_remin_oxidize_ItoIO3='lifetime'
# set I lifetime (yrs)
bg_par_bio_remin_Ilifetime=10.0
# (2) IO3 -> 1
# select option for watercolumn reduction
bg_opt_bio_remin_reduce_IO3toI='reminSO4lifetime'
# mean lifetime
bg_par_bio_remin_SO4toIO3lifetime=1.0E-7
# (3) I/Ca
# set biological IO3 uptake (as a Redfield ratio with carbon)
bg_par_bio_red_POC_POI=2.5E-4
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

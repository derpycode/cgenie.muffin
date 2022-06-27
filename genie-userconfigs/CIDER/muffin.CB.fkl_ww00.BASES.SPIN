# *******************************************************************
# *** USERCONFIG.MODERN.BIOGEM.PO4.SPIN *****************************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=y
#
# *** BIOLOGICAL NEW PRODUCTION *************************************
#
# *** 16-level ocean as per Cao et al. [2009] ***********************
# biological scheme ID string
bg_par_bio_prodopt='1N1T_PO4MM'
# maximum rate of conversion of dissolved PO4 into organic matter by phytoplankton (mol kg-1 yr-1)
bg_par_bio_k0_PO4=8.9876e-006
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=8.9369e-007
# *** 8-level ocean as per Ridgwell et al. [2007] *******************
###bg_par_bio_prodopt='1N1T_PO4MM'
###bg_par_bio_k0_PO4=1.9582242E-06
###bg_par_bio_c0_PO4=2.1989611E-07
# *** optional changes to align with PALEO recommendations **********
###bg_par_bio_prodopt="bio_P"
###bg_par_bio_tau=63.3827
###bg_par_bio_c0_PO4=0.10E-06
# *** Crichton et al. [2020] calibration ****************************
# NOTE: light limitation term is different in biological scheme 1N1T_PO4MM_Tdep compared to bio_P
###bg_par_bio_prodopt='1N1T_PO4MM_Tdep'
###bg_par_bio_mu1=10
###bg_par_bio_c0_PO4=8.9369e-007
#
# *** ORGANIC MATTER EXPORT RATIOS **********************************
#
# production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# *** INORGANIC MATTER EXPORT RATIOS ********************************
#
# *** 16-level ocean as per Cao et al. [2009] ***********************
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.0485
# exponent for modifier of CaCO3:POC export ratio
bg_par_bio_red_POC_CaCO3_pP=0.7440
# *** 8-level ocean as per Ridgwell et al. [2007] *******************
###bg_par_bio_red_POC_CaCO3=0.044372
###bg_par_bio_red_POC_CaCO3_pP=0.8053406
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
# *** 8-level ocean as per Ridgwell et al. [2007] *******************
###bg_par_bio_remin_DOMlifetime=0.5
###bg_par_bio_remin_POC_frac2=6.4591110E-02
###bg_par_bio_remin_POC_eL1=550.5195
###bg_par_bio_remin_POC_eL2=1000000.0
###bg_par_bio_remin_CaCO3_frac2=0.4325
###bg_par_bio_remin_CaCO3_eL1=1083.486
###bg_par_bio_remin_CaCO3_eL2=1000000.0
# *** optional changes to align with PALEO recommendations **********
#### set 'instantaneous' water column remineralziation
###bg_par_bio_remin_sinkingrate_physical=9.9E9
###bg_par_bio_remin_sinkingrate_reaction=125.0
# *** Crichton et al. [2020] temeprature-dependent remin ************
###bg_ctrl_bio_remin_POC_fixed=.false.
###bg_par_bio_remin_POC_K1=9.0E11
###bg_par_bio_remin_POC_Ea1=54000.0
###bg_par_bio_remin_POC_K2=1.0E14
###bg_par_bio_remin_POC_Ea2=80000.0
###bg_par_bio_remin_POC_frac2=0.008
#
# *** DATA SAVING ***************************************************
#
# Ocean acidification save option == biology and geochemical output fields plus all carbonate chemistry
###bg_par_data_save_level=10
# *** optional changes to align with PALEO recommendations **********
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
#
# *** FORCINGS ******************************************************
#
# specify forcings
#### *** 16-level ocean as per Cao et al. [2009] ***********************
#### NOTE: including 14C and CFCs
###bg_par_forcing_name="worjh2.preindustrial"
# *** 8-level ocean as per Ridgwell et al. [2007] *******************
# NOTE: basic generic forcing of atmopsheric pCO2 and d13C
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=278.0E-06
bg_par_atm_force_scale_val_4=-6.5
#
# *** MISC **********************************************************
#
# *** optional changes to align with PALEO recommendations **********
#### maximum time-scale to geochemical reaction completion (days)
###bg_par_bio_geochem_tau=45.0
#### extend solubility and geochem constant T range (leave S range as default)
###gm_par_geochem_Tmin  = -2.0
###gm_par_geochem_Tmax  = 45.0
###gm_par_carbchem_Tmin = -2.0
###gm_par_carbchem_Tmax = 45.0
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

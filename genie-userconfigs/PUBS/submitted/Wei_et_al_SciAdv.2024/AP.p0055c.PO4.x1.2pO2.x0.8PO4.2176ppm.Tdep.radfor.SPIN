# *******************************************************************
# *** USERCONFIG.PALEO.BIOGEM.PO4.SPIN ******************************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=n
ea_radfor_scl_co2 = 7.827338129496403 
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
# *** published paleo configuration *********************************
###bg_par_bio_prodopt="1N1T_PO4MM"
###bg_par_bio_k0_PO4=8.9876e-006
###bg_par_bio_c0_PO4=8.9369e-007
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
# fixed CaCO3:POC
bg_opt_bio_CaCO3toPOCrainratio='FIXED'
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.200
# *** alternative for > 200 Ma **************************************
###bg_par_bio_red_POC_CaCO3=0.0
#
# *** REMINERALIZATION **********************************************
#
# *** recommended ***************************************************
# set 'instantaneous' water column remineralziation
bg_par_bio_remin_sinkingrate_physical=9.9E9
bg_par_bio_remin_sinkingrate_reaction=125.0
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
bg_par_bio_remin_POC_K1=9.0E11
bg_par_bio_remin_POC_Ea1=54000.0
bg_par_bio_remin_POC_K2=1.0E14
bg_par_bio_remin_POC_Ea2=80000.0
bg_par_bio_remin_POC_frac2=0.008
#
# *** DATA SAVING ***************************************************
#
# BASIC + biology + tracer + proxy diagnostics
bg_par_data_save_level=16
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
# *** recommended ***************************************************
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
#
# *** FORCINGS ******************************************************
#
# specify forcings -- restoring of atmopsheric pCO2 and d13C
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2.RpO2"
bg_par_atm_force_scale_val_3=834.0E-06
bg_par_atm_force_scale_val_4=-4.9
bg_par_atm_force_scale_val_6=0.25139999999999996 # 0.2095 = 1 PAL O2
bg_par_windspeed_file="p0055c_windspeed.dat"
# re-scale gas transfer coeff to give ~0.058 mol m-2 yr-1 uatm-1 global mean air-sea coefficient (original: 0.310)
# after [cgenie.muffin/genie-userconfigs/MS/gutjahretal.2017/SPIN1] and [cgenie.muffin/genie-userconfigs/PUBS/published/Barrett_et_al.2023/Barrettetal.Eocene.p0055c.3x.SPIN1]
bg_par_gastransfer_a=0.5196
#
# --- INITIAL CONDITIONS ---------------------------------------
#
# mean ocean PO4
#bg_ocn_init_8=2.159E-06 # default PD value included in the base config
bg_ocn_init_8=1.7271999999999998E-06
#
#
# *** MISC **********************************************************
#
# Set initial ocean alkalinity
# after [cgenie.muffin/genie-userconfigs/MS/gutjahretal.2017/SPIN1] and [cgenie.muffin/genie-userconfigs/PUBS/published/Barrett_et_al.2023/Barrettetal.Eocene.p0055c.3x.SPIN1]
bg_ocn_init_12=1.975E-03 # OK since we use radfor to simulate warmer worlds
#
# age tracer, automatic method
bg_ctrl_force_ocn_age=.true
#
# *** recommended ***************************************************
# maximum time-scale to geochemical reaction completion (days)
bg_par_bio_geochem_tau=45.0
# extend solubility and geochem constant T range (leave S range as default)
#gm_par_geochem_Tmin  = -2.0
#gm_par_geochem_Tmax  = 45.0
#gm_par_carbchem_Tmin = -2.0
#gm_par_carbchem_Tmax = 45.0
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

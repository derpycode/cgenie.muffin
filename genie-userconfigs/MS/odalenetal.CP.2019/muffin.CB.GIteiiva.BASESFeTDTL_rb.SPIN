#
#
# --- CLIMATE -------------------------------------------------------
#
# set climate feedback
ea_36=y
#### scaling for atmospheric CO2 radiative forcing (190 ppm), relative to 278.0e-6
###ea_radfor_scl_co2=0.683
#### scaling for atmospheric CH4 radiative forcing (350 ppb), relative to 700.e-9
###ea_radfor_scl_ch4=0.5
#### scaling for atmospheric N2O radiative forcing (), relative to 275.e-9
###ea_radfor_scl_no2=1.0
#
# --- BIOLOGICAL NEW PRODUCTION -------------------------------------
#
# biological scheme ID string
bg_par_bio_prodopt="bio_PFe"
# [PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=0.10E-06
# [Fe] M-M half-sat value (mol kg-1)
bg_par_bio_c0_Fe=0.10E-09
# biological uptake time-scale
bg_par_bio_tau=95.6337
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
bg_par_bio_remin_DOMlifetime=1.0
# initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=0.055
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
# --- IRON ----------------------------------------------------------
#
bg_opt_geochem_Fe='hybrid'
# aeolian Fe solubility
bg_par_det_Fe_sol=0.002014275
#exponent for aeolian Fe solubility [use 1.0 for uniform solubility]
bg_par_det_Fe_sol_exp=0.500
# modifier of the scavenging rate of dissolved Fe
bg_par_scav_Fe_sf_POC=0.344104
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
# --- DATA SAVING -----------------------------------------------------
#
bg_par_data_save_level=5
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS ------------------------------------------------------
#
# specify forcings
bg_par_forcing_name="GIteiiva.RpCO2_Rp13CO2.Fsal_SUR.Albani.21ka"
bg_par_atm_force_scale_val_3=193.0E-06
bg_par_atm_force_scale_val_4=-6.46
# FwF (SV)
bg_par_ocn_force_scale_val_2=0.1
# Orbital parameters
ea_par_orbit_osce=0.018994 # eccentricity
ea_par_orbit_oscsob=0.389911 # sine of obliquity
ea_par_orbit_oscgam=114.42 # longitude of perihelion (degrees)
#
# --- MISC ----------------------------------------------------------
#
# add ventillation tracers
bg_ctrl_force_ocn_age=.true.
# add brine rejection SH limit
bg_par_misc_brinerejection_jmax=19
# wind-stress
go_13=0.8
ea_11=0.8
# extend solubility and geochem constant T range (leave S range as default)
gm_par_geochem_Tmin  = -2.0
gm_par_geochem_Tmax  = 45.0
gm_par_carbchem_Tmin = -2.0
gm_par_carbchem_Tmax = 45.0
#
# --- INITIAL CONDITIONS ---------------------------------------
#
# modern DIC (2.244E-03) + 3%
bg_ocn_init_3=2.311320000000000e-03
# modern 13C DIC
bg_ocn_init_4=0.4
# modern ALK (2.363E-03) + 3%
bg_ocn_init_12=2.433890000000000e-03
# modern PO4 (2.159E-06) + 3%
bg_ocn_init_8=2.223770000000000e-06
# modern SAL + 1 PSU
go_saln0=35.9
#
# --- END -----------------------------------------------------------
#

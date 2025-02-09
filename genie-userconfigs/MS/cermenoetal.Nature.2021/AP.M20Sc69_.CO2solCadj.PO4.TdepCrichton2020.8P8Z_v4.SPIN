# *******************************************************************
# *** USERCONFIG.PALEO.ECOGEM.PO4.SPIN ******************************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=y
# solar constant (Gough, 1981)
ma_genie_solar_constant=1360.01
#
# *** BIOLOGICAL NEW PRODUCTION *************************************
#
# biological scheme ID string
# NOTE: with ECOGEM, no BIOGEM biological scheme must be selected
bg_par_bio_prodopt="NONE"
#
# *** ORGANIC MATTER EXPORT RATIOS **********************************
#
# [dealt with by ECOGEM]
#
# *** INORGANIC MATTER EXPORT RATIOS ********************************
#
# [dealt with by ECOGEM]
#
# *** REMINERALIZATION **********************************************
#
# *** recommended ***************************************************
# DOC lifetime (yrs) -- following Doney et al. [2006]
bg_par_bio_remin_DOMlifetime=0.5
# set 'instantaneous' water column remineralziation
bg_par_bio_remin_sinkingrate_physical=9.9E9
bg_par_bio_remin_sinkingrate_reaction=125.0
# *** Ward et al. [2018] ********************************************
###bg_par_bio_remin_sinkingrate_physical=125.0
###bg_par_bio_remin_sinkingrate_reaction=125.0
###bg_par_bio_remin_DOMlifetime=1.0
# *** common settings ***********************************************
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
# *** ECOGEM ********************************************************
#
# ecosystem configuration
eg_par_ecogem_plankton_file='8P8Z.eco'
#################### Nitrogen ############################
eg_useNO3                   =.false.
eg_nquota                   =.false.
################## Phosphorus ############################
eg_usePO4                   =.true.
eg_pquota                   =.true.
######################## Iron ############################
eg_useFe                    =.false.
eg_fquota                   =.false.
################# Chlorophyll ############################
eg_chlquota                 =.true.
################# d13C ###################################
eg_useDIC_13C               =.true.
# Tuned parameters
eg_qminP_a     =   3.33e-3
eg_qminFe_a    =   1.00e-6
eg_qmaxFe_a    =   4.00e-6
# ???
eg_ns=2
# ???
eg_respir_a=0
# DOM parameters
eg_beta_mort_a =   0.8 # Maximum fraction to DOM as ESD --> zero
eg_beta_mort_b =   0.4 # Minimum fraction to DOM as ESD --> infinity 
eg_beta_mort_c = 100.0 # Size at 50:50 partition (default = 100 µm^3)
eg_beta_graz_a =   0.8 # Maximum fraction to DOM as ESD --> zero
eg_beta_graz_b =   0.4 # Minimum fraction to DOM as ESD --> infinity
eg_beta_graz_c = 100.0 # Size at 50:50 partition (default = 100 µm^3)
# ???
eg_nsubtime=25
eg_n_keco=1
# exponent for modifier of CaCO3:POC export ratio
# NOTE: set power to zero for invarient ratio
eg_par_bio_red_POC_CaCO3_pP=0.0
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
# NOTE: reduced by 50% on the basis that the modern ECOGEM scaling parameter is also ca. 50% lower
eg_par_bio_red_POC_CaCO3 = 0.01 # underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
# *** alternative for > 200 Ma **************************************
###eg_par_bio_red_POC_CaCO3=0.0
#
# *** DATA SAVING ***************************************************
#
# Basic + biology diagnostics save option
bg_par_data_save_level=16
bg_par_infile_slice_name='save_timeslice_EVERY001000.dat'
# disable ECOGEM restarts (as not currently coded up / used)
eg_ctrl_ncrst      = .false.
eg_ctrl_continuing = .false.
#
# *** FORCINGS ******************************************************
#
# specify forcings -- restoring of atmopsheric pCO2, pO2 and d13C
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2.RpO2"
bg_par_atm_force_scale_val_3=245.28E-06 # Krause et al., 2017
bg_par_atm_force_scale_val_4=-6.5
bg_par_atm_force_scale_val_6=0.2095 # 1 PAL O2
# *** alternative Fe-limitation-enabled forcing *********************
#### specify forcings -- restoring of atmopsheric pCO2 and d13C + uniform dust
###bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2.DUST"
#
# *** MISC **********************************************************
#
# age tracer, automatic method
bg_ctrl_force_ocn_age=.true.
# kraus-turner mixed layer scheme on (1) or off (0)
go_imld = 1
# *** recommended ***************************************************
# set mixed layer to be only diagnosed (for ECOGEM)
go_ctrl_diagmld=.true.
# add seaice attenuation of PAR
eg_ctrl_PARseaicelimit=.true.
# relative partitioning of C into DOM
eg_par_beta_POCtoDOC=0.70
# maximum time-scale to geochemical reaction completion (days)
bg_par_bio_geochem_tau=90.0
# extend solubility and geochem constant T range (leave S range as default)
gm_par_geochem_Tmin  = -2.0
gm_par_geochem_Tmax  = 45.0
gm_par_carbchem_Tmin = -2.0
gm_par_carbchem_Tmax = 45.0
# *** alternative Fe-limitation-enabled calibrated value ************
#### relative partitioning of C into DOM
###eg_par_beta_POCtoDOC=0.75
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

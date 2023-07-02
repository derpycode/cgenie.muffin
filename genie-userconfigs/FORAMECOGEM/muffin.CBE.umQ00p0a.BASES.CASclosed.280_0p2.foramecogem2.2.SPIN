# *** CLIMATE *******************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=y
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
# *** optional changes to align with PALEO recommendations **********
# set 'instantaneous' water column remineralziation
bg_par_bio_remin_sinkingrate_physical=9.9E9
bg_par_bio_remin_sinkingrate_reaction=125.0
#
# *** IRON CYCLING **************************************************
#
# [no iron]
#
# *** ECOGEM ********************************************************
#
gm_ctrl_debug_init          =0
eg_ctrl_debug_eco_init=.false.

eg_gkernel_cap=.false.
eg_ctrl_grazing_explicit=.true.

eg_ctrl_use_foramecogenie=.true.
eg_ctrl_foramecogenie_oa=.false.
eg_ctrl_foramecogenie_bleach=.false.

eg_par_ecogem_plankton_file='8P7Z4F.eco'
eg_par_ecogem_grazing_file='FORAMECOGEM.zoo'
eg_foram_symbiont_esd_scale=0.01
eg_foram_auto_cost=0.04
eg_foram_hetero_cost=0.35
eg_foram_grazing_scale=3.5

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
eg_par_bio_red_POC_CaCO3_pP  = 0.7440 # exponent for modifier of CaCO3:POC export ratio
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
eg_par_bio_red_POC_CaCO3     = 0.0285 # underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
#
# --- DATA SAVING ----------------------------------------------
#
bg_par_data_save_level=7
# reduce output
bg_ctrl_data_save_2d=.true.
bg_ctrl_data_save_3d=.true. # JDW - set to true
#
# --- FORCINGS -------------------------------------------------
#
# specify forcings
bg_par_forcing_name="Q_xxxxxx.RpCO2_Rp13CO2.Fsal_SUR"
bg_par_atm_force_scale_val_4=-6.428
# parameter: bg_par_atm_force_scale_val_3 -- default value modified by factor: 280
bg_par_atm_force_scale_val_3=0.000280
# parameter: bg_par_ocn_force_scale_val_2 -- default value modified by factor: 0.2
bg_par_ocn_force_scale_val_2=0.2
#
# *** MISC **********************************************************
#
# kraus-turner mixed layer scheme on (1) or off (0)
go_imld = 1
# *** optional changes to align with PALEO recommendations **********
# set mixed layer to be only diagnosed (for ECOGEM)
go_ctrl_diagmld=.true.
# add seaice attenuation of PAR
eg_ctrl_PARseaicelimit=.false.
# relative partitioning of C into DOM
eg_par_beta_POCtoDOC=0.75
# maximum time-scale to geochemical reaction completion (days)
bg_par_bio_geochem_tau=90.0
# extend solubility and geochem constant T range (leave S range as default)
gm_par_geochem_Tmin  = -2.0
gm_par_geochem_Tmax  = 45.0
gm_par_carbchem_Tmin = -2.0
gm_par_carbchem_Tmax = 45.0
#
ma_flag_ecogem=.TRUE.
GOLDSTEINNTRACSOPTS='$(DEFINE)GOLDSTEINNTRACS=16'
gm_ocn_select_60=.true. # preformed PO4
gm_ocn_select_66=.true. # Csoft
bg_ctrl_bio_preformed=.true.
bg_ctrl_bio_remin_redox_save=.true.
bg_ctrl_bio_preformed_CsoftPOConly=.true. # Ppre = all Csoft
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

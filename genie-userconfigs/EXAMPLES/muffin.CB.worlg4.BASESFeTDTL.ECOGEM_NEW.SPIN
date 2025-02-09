# *******************************************************************
# *** USERCONFIG.MODERN.ECOGEM.PO4Fe.SPIN ***************************
# *******************************************************************
#
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
# iron tracer scheme
bg_opt_geochem_Fe='hybrid'
# aeolian Fe solubility
bg_par_det_Fe_sol=0.002014275
# modifier of the scavenging rate of dissolved Fe
bg_par_scav_Fe_sf_POC=0.344104
# exponent for aeolian Fe solubility [use 1.0 for uniform solubility]
bg_par_det_Fe_sol_exp=0.500
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
# *** ECOGEM ********************************************************
#
# specify ecosystem structure definition file
eg_par_ecogem_plankton_file='8P8Z.eco'
# include cellular quota: N
eg_useNO3                   =.false.
eg_nquota                   =.false.
# include cellular quota: PO4
eg_usePO4                   =.true.
eg_pquota                   =.true.
# include cellular quota: Fe
eg_useFe                    =.true.
eg_fquota                   =.true.
# include cellular quota: Chlorophyll
eg_chlquota                 =.true.
# d13C fractionaton based on d13C of DIC?
eg_useDIC_13C               =.true.
# P and Fe quota parameters (tuned values)
eg_qminP_a     =   3.33e-3
eg_qminFe_a    =   1.00e-6
eg_qmaxFe_a    =   4.00e-6
# Prey switching exponent (Holling 2 or 3 [= ns+1])
eg_ns=2
# Carbon respiration rate - intercept
eg_respir_a=0.0
# DOM (ca. POM) partitioning parameters
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
# *** DATA SAVING ***************************************************
#
# Basic + biology diagnostics save option
bg_par_data_save_level=3
#
# *** FORCINGS ******************************************************
#
# specify forcings -- dust field and restoring of atmopsheric pCO2 and d13C
bg_par_forcing_name="worlg4.RpCO2_Rp13CO2.FeMahowald2006"
bg_par_atm_force_scale_val_3=278.0E-06
bg_par_atm_force_scale_val_4=-6.5
#
# *** MISC **********************************************************
#
# kraus-turner mixed layer scheme on (1) or off (0)
go_imld = 1
# *** optional changes to align with PALEO recommendations **********
# set mixed layer to be only diagnosed (for ECOGEM)
go_ctrl_diagmld=.true.
# add seaice attenuation of PAR
eg_ctrl_PARseaicelimit=.true.
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
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

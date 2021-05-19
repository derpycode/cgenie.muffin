#
#
# --- CLIMATE --------------------------------------------------
#
# set no climate feedback
ea_36=y
#
# --- BIOLOGICAL NEW PRODUCTION --------------------------------
#
# maximum rate of conversion of dissolved PO4 into organic matter by phytoplankton (mol kg-1 yr-1)
bg_par_bio_k0_PO4=1.9582242E-06
#[PO4] M-M half-sat value (mol kg-1)
bg_par_bio_c0_PO4=2.1989611E-07
#
# --- ORGANIC MATTER EXPORT RATIOS -----------------------------
#
#production fraction of dissolved organic matter
bg_par_bio_red_DOMfrac=0.66
#
# --- INORGANIC MATTER EXPORT RATIOS ---------------------------
#
# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)
bg_par_bio_red_POC_CaCO3=0.044372
# exponent for modifier of CaCO3:POC export ratio
bg_par_bio_red_POC_CaCO3_pP=0.8053406
#
# --- REMINERALIZATION -----------------------------------------
#
#DOC lifetime (yrs)
bg_par_bio_remin_DOMlifetime=0.5
#initial fractional abundance of POC component #2
bg_par_bio_remin_POC_frac2=6.4591110E-02
# depth of remineralization or particulate organic matter
bg_par_bio_remin_POC_eL1=550.5195
#remineralization length #2 for POC
bg_par_bio_remin_POC_eL2=1000000.0
#initial fractional abundance of CaCO3 component #2
bg_par_bio_remin_CaCO3_frac2=0.468
# depth of remineralization or CaCO3
bg_par_bio_remin_CaCO3_eL1=1083.486
#remineralization length #2 for CaCO3
bg_par_bio_remin_CaCO3_eL2=1000000.0
#
# --- SEDIMENTS ------------------------------------------------
#
# sediment  grid options
sg_par_sed_Dmax_neritic=1000.0
sg_par_sed_topo_D_name="worbe2.190813.depth.36x36"
sg_par_sed_reef_mask_name="worbe2.190813.reefmask.36x36"
sg_par_sedcore_save_mask_name="worbe2.190813.sedcoremask.36x36"
# sediment diagenesis options
sg_par_sed_diagen_CaCO3opt="ridgwell2001lookup"
sg_ctrl_sed_bioturb=.false.
sg_ctrl_sed_bioturb_Archer=.false.
sg_par_n_sed_mix=20
# set invarient surface bioturbation mixing rate
sg_par_sed_mix_k_sur_max=0.15
sg_par_sed_mix_k_sur_min=0.15
# no additional detrital flux
sg_par_sed_fdet=0.0
# total reefal burial rate -- 4 Tmol CaCO3 yr-1
sg_par_sed_CaCO3burialTOT=4E12
#
# --- WEATHERING -----------------------------------------------
#
# set a 'CLOSED' system
bg_ctrl_force_sed_closedsystem=.true.
# set total weathering rate
rg_par_weather_CaCO3=10.0E+12
# set bulk weathering isotopic composition
rg_par_weather_CaCO3_d13C=3.0
#
# --- DATA SAVING ----------------------------------------------
#
bg_par_data_save_level=4
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=1
#
# --- FORCINGS -------------------------------------------------
#
# specify forcings
bg_par_forcing_name="worbe2.RpCO2_Rp13CO2_detplusopalSED"
bg_par_atm_force_scale_val_3=278.0E-06
bg_par_atm_force_scale_val_4=-6.5
#
# --- END ------------------------------------------------------
#
#

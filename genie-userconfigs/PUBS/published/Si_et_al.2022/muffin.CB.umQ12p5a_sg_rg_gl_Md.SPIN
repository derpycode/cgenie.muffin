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
#bg_par_bio_red_POC_CaCO3=0.0485
bg_par_bio_red_POC_CaCO3=0.225
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
# --- WEATHERING ------------------------------------------------------
#
# set a 'OPEN' system # do a close system first
bg_ctrl_force_sed_closedsystem=.false.
# set CaCO3_weathering-temperature feedback
rg_opt_weather_T_Ca=.false.
# set CaSiO3_weathering-temperature feedback
rg_opt_weather_T_Si=.false.
# weathering reference mean global land surface temperature (C)
rg_par_ref_T0=13.02
#CO2 outgassing rate (mol C yr-1)
rg_par_outgas_CO2=0
# global silicate weathering rate (mol Ca2+ yr-1)
rg_par_weather_CaSiO3=0
# global carbonate weathering rate (mol Ca2+ yr-1) - A first guess for the weathering flux
#rg_par_weather_CaCO3=16.5E+12
rg_par_weather_CaCO3=21.45E+12
# d13C
rg_par_outgas_CO2_d13C=-6.0
rg_par_weather_CaCO3_d13C=3.00
#
# ----- Sediments -----------------------------------------------------------------------------------------------------#
# note that this part can be done either in base-config or user-config
# Grid resolution of solid Earth components
SEDGEMNLONSOPTS='$(DEFINE)SEDGEMNLONS=36'
SEDGEMNLATSOPTS='$(DEFINE)SEDGEMNLATS=36'
ROKGEMNLONSOPTS='$(DEFINE)ROKGEMNLONS=36'
ROKGEMNLATSOPTS='$(DEFINE)ROKGEMNLATS=36'
# sediment grid options
# Several data input files are required by SEDGEM consistent with the specified grid.
sg_par_pindir_name='../../cgenie.muffin/genie-paleo/umQ12p5a/'
rg_par_pindir_name='../../cgenie.muffin/genie-paleo/umQ12p5a/'
sg_par_sed_topo_D_name='umQ12p5a.depth.dat'
sg_par_sed_reef_mask_name='umQ12p5a.reefmask.dat'
sg_par_sedcore_save_mask_name='umQ12p5a.sedcoremask.dat'
sg_par_sed_Dmax_neritic=176.0
# sediment diagenesis options
sg_par_sed_diagen_CaCO3opt="ridgwell2001lookup"
sg_ctrl_sed_bioturb=.TRUE.
sg_ctrl_sed_bioturb_Archer=.false.
sg_par_n_sed_mix=20
# set invarient surface bioturbation mixing rate
sg_par_sed_mix_k_sur_max=0.15
sg_par_sed_mix_k_sur_min=0.15
# additional detrital flux (g cm-2 kyr-1)
sg_par_sed_fdet=0.180
# ----- Sediments -----------------------------------------------------------------------------------------------------
#
# --- GEOCHEM ACCELERATION -------------------------------------
#
gl_ctrl_update_pCO2=.true.
ma_gem_notyr_min=10
ma_gem_notyr_max=10
ma_gem_yr_min=990
ma_gem_yr_max=990
ma_gem_dyr=0
ma_gem_adapt_auto=.false. 
#
# --- DATA SAVING ----------------------------------------------
#
bg_par_data_save_level=7
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
bg_par_atm_force_scale_val_4=-5.626
# parameter: bg_par_atm_force_scale_val_3 -- default value modified by factor: 600
bg_par_atm_force_scale_val_3=0.000600
# parameter: bg_par_ocn_force_scale_val_2 -- default value modified by factor: 0.2
bg_par_ocn_force_scale_val_2=0.2
#
# --- END ------------------------------------------------------
#

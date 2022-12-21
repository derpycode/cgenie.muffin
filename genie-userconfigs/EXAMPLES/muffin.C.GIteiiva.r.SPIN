# *******************************************************************
# *** USERCONFIG.ABIOTIC.TRACER.SPIN ********************************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set no climate feedback (climate responding to changing pCO2)
ea_36=n
# scaling for atmospheric CO2 radiative forcing, calculated as the multiplier of 278 ppm CO2
ea_radfor_scl_co2=0.694
#
# *** BIOLOGICAL NEW PRODUCTION *************************************
#
# Set no biological production
bg_par_bio_prodopt='NONE'
#
# *** DATA SAVING ***************************************************
#
# Set BASIC + tracer + full physics save level (12)
# (assume default time-series and time-slice save intervals)
bg_par_data_save_level=12
# *** optional changes to align with PALEO recommendations **********
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
#
# *** FORCINGS ******************************************************
#
# specify forcings
bg_par_forcing_name="GIteiiva.Fsal_SUR"
# add brine rejection SH limit
bg_par_misc_brinerejection_jmax=19
# Orbital parameters
ea_par_orbit_osce=0.018994 # eccentricity
ea_par_orbit_oscsob=0.389911 # sine of obliquity
ea_par_orbit_oscgam=114.42 # longitude of perihelion (degrees)
#
# *** MISC **********************************************************
#
# *** optional changes to add ventillation age as a tracer ************
# activate single-tracer ventillation age tracer
# NOTE: you *must* have the 'red' color tracer colr included in the base-config and hence gm_ocn_select_48=.true.
bg_ctrl_force_ocn_age1=.true.
#
# *** TUNING ********************************************************
#
# wind-stress
go_13=1.3
ea_11=1.3
# parameter: bg_par_misc_brinerejection_frac -- default value (1) modified by factor: 0.2
bg_par_misc_brinerejection_frac=0.2
# parameter: bg_par_ocn_force_scale_val_2 -- default value (1) modified by factor: 0.05
bg_par_ocn_force_scale_val_2=0.05
#
# *** INITIAL CONDITIONS ********************************************
#
# modern SAL + 0.33 PSU
go_saln0=35.23
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************
#

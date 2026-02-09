# *******************************************************************
# *** EXAMPLE *******************************************************
# *******************************************************************
#
# *** CLIMATE *******************************************************
#
# set no climate feedback (climate responding to changing pCO2)
ea_36=n
# scaling for atmospheric CO2 radiative forcing, calculated as the multiplier of 278 ppm CO2
ea_radfor_scl_co2 = 39.479511008636244
#
# solar constant (Gough, 1981)
ma_genie_solar_constant=1356.8
#
# orbit Orbit10
ea_opt_orbit_old=.false. # new orbital radiative code flag
ea_par_orbit_osce= 0.0 # modern: 0.0167 # eccentricity
ea_par_orbit_oscobl= 0.41015237421866746 # modern: 0.409093 # obliquity (in radians) [obliquity in degree *pi/180]
ea_par_orbit_osclonperi= 0.0 # modern: 1.796257 # true longitude of perihelion (in radians)
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
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
#
# *** MISC **********************************************************
# age tracer
bg_ctrl_force_ocn_age1=.true.
#
# *******************************************************************
# *** END ***********************************************************
# *******************************************************************

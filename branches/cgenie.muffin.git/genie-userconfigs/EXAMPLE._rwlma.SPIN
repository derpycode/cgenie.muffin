#
#
# --- CLIMATE ---------------------------------------------------------
#
# solar constant
# NOTE: modern S0 is 1368 W m-2
#       for a ca. 720 Ma time-interval (Sturtian onset), S0 will be 1286.9
#       for a ca. 635 Ma time-interval (Marinoan onset), S0 will be 1295.4
ma_genie_solar_constant=1295.4
# scaling for atmospheric CO2 radiative forcing, relative to 278 ppm
ea_radfor_scl_co2=10.0  
#
# --- BIOLOGICAL NEW PRODUCTION ---------------------------------------
#
# No biology
bg_par_bio_prodopt='NONE'
#
# --- ORGANIC MATTER EXPORT RATIOS ------------------------------------
#
#
# --- INORGANIC MATTER EXPORT RATIOS ----------------------------------
#
#
# --- REMINERALIZATION ------------------------------------------------
#
#
# --- TRACE METALS ----------------------------------------------------
#
#
# --- SEDIMENTS -------------------------------------------------------
#
#
# --- WEATHERING ------------------------------------------------------
#
#
# --- DATA SAVING -----------------------------------------------------
#
# date saving & reporting options
bg_par_data_save_level=9
bg_ctrl_debug_lvl0=.true.
ma_debug_loop=2
#
# --- FORCINGS --------------------------------------------------------
#
# use internal wind-speed
bg_ctrl_force_windspeed=.false.
# add a geothermal heat flux (W m-2)
bg_ctrl_force_GOLDSTEInTS=.TRUE.
bg_par_Fgeothermal=100.0E-3
#
# --- MISC ------------------------------------------------------------
#
go_debug_init=.true.
#
# --- END ------------------------------------------------------
#

# **********************************************************************
# adapted from: muffin.C.p_worjh2.r.SPIN
# base-config: 	cookie.C.p_worjh2.rb
# **********************************************************************
#
# *** CLIMATE **********************************************************
#
# set NO CO2 climate feedback (climate not responding to changing pCO2)
ea_36=n
# scaling for atmospheric CO2 radiative forcing, relative to 278 ppm
ea_radfor_scl_co2=1.0
#
# *** BIOLOGICAL NEW PRODUCTION ****************************************
#
# No biology
bg_par_bio_prodopt='NONE'
#
# *** DATA SAVING ***************************************************
#
# Preformed diagnostics option == BASIC
bg_par_data_save_level=2
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
# Include experiment name in BIOGEM netCDF filenames? 
bg_ctrl_ncout_expid_name=.false.
#
# *** FORCINGS *********************************************************
#
# apply numerical dye circulation tracer
# NOTE: flux set to ZERO
bg_par_forcing_name="pyyyyz.Fblue"
bg_par_force_point_i=22
bg_par_force_point_j=33
bg_par_force_point_k=8
bg_par_ocn_force_scale_val_49=0.0
#
# *** MISC *************************************************************
#
# activate single-tracer based ventillation age tracer (using the 'red' numerical tracer)
bg_ctrl_force_ocn_age1=.true.
#
# **********************************************************************
# *** END **************************************************************
# **********************************************************************


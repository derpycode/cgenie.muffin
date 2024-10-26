# **********************************************************************
# adapted from:	cookie.C.p_worjh2.rCARB.SPIN
# base-config: 	cookie.C.p_worjh2.rCARB
# **********************************************************************
#
# *** CLIMATE **********************************************************
#
# set climate feedback (climate responding to changing pCO2)
ea_36=y
#
# *** BIOLOGICAL NEW PRODUCTION ****************************************
#
# No biology
bg_par_bio_prodopt='NONE'
#
# *** DATA SAVING ******************************************************
#
# OCEAN ACIDIFICATION & FOSSIL FUEL GAMES
bg_par_data_save_level=10
# force time-slice save at run end only
bg_par_infile_slice_name='save_timeslice_NONE.dat'
# Include experiment name in BIOGEM netCDF filenames? 
bg_ctrl_ncout_expid_name=.false.
#
# *** FORCINGS *********************************************************
#
# specify forcings -- generic forcing of atmopsheric pCO2 and d13C
bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2"
bg_par_atm_force_scale_val_3=278.0E-06
bg_par_atm_force_scale_val_4=-6.5
#
# *** MISC *************************************************************
#
# activate single-tracer based ventillation age tracer (using the 'red' numerical tracer)
bg_ctrl_force_ocn_age1=.true.
#
# **********************************************************************
# *** END **************************************************************
# **********************************************************************


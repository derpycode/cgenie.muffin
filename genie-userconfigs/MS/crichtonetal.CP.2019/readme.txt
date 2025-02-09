################################################################
### readme.txt #################################################
################################################################

For:
'Modelling ocean circulation and CO2 since the middle Miocene'
Katherine A. Crichton, Andy Ridgwell, Dan Lunt, Paul N. Pearson

################################################################
19/10/15 -- README.txt file creation (A.R.)
################################################################

Provided is the code used to create the tuned model experiments presented in the paper (i.e. the best fit ensemble members).
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### model experiments ##########################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

Note that there is no spin-up for any of these experiments.
All experiments are run for 10,000 years.

The mid-point of the time-slice is given in Q10_yypy notation in the filenames:

umQ00p0a == 0 Ma    == Holocene
umQ02p5a == 2.5 Ma  == Piacenzian (Pliocene/Pleistocene)
umQ04p5a == 4.5 Ma  == Zanclean (Pliocene)
umQ07p5a == 7.5 Ma  == Tortonian/Messinian (Miocene)
umQ10p0a == 10 Ma   == Tortonian (Miocene)
umQ12p5a == 12.5 Ma == Serravalian (Miocene)
umQ15p0a == 15.0 Ma == Langhian (Miocene)

The commands to run the tuned experiments are listed as follows:

-------------------------------------------
./runmuffin.sh muffin.CB.umQ00p0a MS/crichtonetal.CP.2019 muffin.CB.umQ00p0a.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5a MS/crichtonetal.CP.2019 muffin.CB.umQ02p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5a MS/crichtonetal.CP.2019 muffin.CB.umQ04p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5a MS/crichtonetal.CP.2019 muffin.CB.umQ07p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0a MS/crichtonetal.CP.2019 muffin.CB.umQ10p0a.SPIN 10000
./runmuffin.sh muffin.CB.umQ12p5a MS/crichtonetal.CP.2019 muffin.CB.umQ12p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a MS/crichtonetal.CP.2019 muffin.CB.umQ15p0a.SPIN 10000
-------------------------------------------

### NOTES ######################################################

In the user-configs run for the paper, a more frequent, but reduced data saving was used.
This is becasue the nature of the model-data was 2D -- either surface or benthic fields.
Also, frequent time-slice saving was necessary in order to automatically identify the presence of oscillations in ocean circulation.

The '# --- DATA SAVING ---' section used in the experiments in the paper is actually:

bg_par_data_save_level=7
# reduce output
bg_ctrl_data_save_2d=.true.
bg_ctrl_data_save_3d=.false.
# save frequency
bg_par_infile_slice_name='save_timeslice_EVERY000100.dat'
bg_par_infile_sig_name='save_timeseries_EVERY000100.dat'

However, a more 'normal' output, under '# --- DATA SAVING ---', is given in the user-config files provided here:

bg_par_data_save_level=15

The salinity forcing, to contorl the AMOC strength, 
is applied by means of a 2D forcing that is generic across all the reconstructions:

bg_par_forcing_name="Q_xxxxxx.RpCO2_Rp13CO2.Fsal_SUR"

and scaled by e.g.

bg_par_ocn_force_scale_val_2=0.2

in units of Sv.

Atmospheric CO2 is restored in units of atmosphere, e.g.

bg_par_atm_force_scale_val_3=0.000280

Atmospheric CO2 d13C is restored in units of o/oo, e.g.

bg_par_atm_force_scale_val_4=-6.5

Note that atmospheric d13C is set to minimize the bias between model d13C and benthic data (Figure 13)

The complete set of tuned d13C, FwF, and CO2 values are:

time (Ma)   atm d13CO2  FwF (Sv)    CO2 (ppm)
0.006	    -6.232	    0.200	    280
2.5	        -6.261	    0.300	    400
4.5	        -5.954	    0.500	    400
7.5	        -5.624	    0.400	    800
10	        -5.497	    0.400	    800
12.5	    -5.554	    0.200	    1120
15	        -5.318	    0.100	    1120

################################################################
################################################################
################################################################

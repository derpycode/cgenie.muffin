################################################################
### readme.txt #################################################
################################################################

For:
'Modelling ocean circulation and CO2 since the middle Miocene'
Katherine A. Crichton, Andy Ridgwell, Dan Lunt, Paul N. Pearson

################################################################
19/10/15 -- README.txt file creation (A.R.)
21/05/06 -- revised (A.R.)
################################################################

Provided is the code used to create the tuned model experiments presented in the paper (i.e. the best fit ensemble members).
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### model experiments ##########################################

All experiments are by default run from:
$HOME/cgenie.muffin/genie-main

All experiments are run for 10,000 years.

The mid-point of the time-slice is given in the umQ_yypy notation part of the user-config filenames:

umQ00p0z == 0 Ma    == Holocene
umQ02p5z == 2.5 Ma  == Piacenzian (Pliocene/Pleistocene)
umQ04p5z == 4.5 Ma  == Zanclean (Pliocene)
umQ07p5z == 7.5 Ma  == Tortonian/Messinian (Miocene)
umQ10p0z == 10 Ma   == Tortonian (Miocene)
umQ12p5z == 12.5 Ma == Serravalian (Miocene)
umQ15p0z == 15.0 Ma == Langhian (Miocene)

with the 'z' designating the series of configurations:

'a' == GMDD paper
'b' == additional alternative CAS configurations included in this paper

'CASopen' and 'CASclosed' is incorportated to the filename to make the configurations unambigeous as to open vs. closed.

Finally, the CO2 and FwF forcing is also included in the filename in the form (u)uuu_vpvv,
where (u)uuu is the CO2 concentration in ppm, and vpvv the FwF in Sv.

The commands to run the tuned experiments are listed as follows:

-------------------------------------------
./runmuffin.sh muffin.CB.umQ00p0a.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ00p0a.BASES.CASclosed.280_0p2.SPIN 10000
./runmuffin.sh muffin.CB.umQ00p0b.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ00p0b.BASES.CASopen.280_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5a.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ02p5a.BASES.CASopen.400_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5b.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ02p5b.BASES.CASclosed.400_0p1.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5a.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ04p5a.BASES.CASopen.400_0p5.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5b.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ04p5b.BASES.CASclosed.400_0p2.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5a.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ07p5a.BASES.CASopen.800_0p4.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5b.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ07p5b.BASES.CASclosed.800_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0a.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ10p0a.BASES.CASopen.800_0p4.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0b.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ10p0b.BASES.CASclosed.800_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ12p5a.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ12p5a.BASES.CASopen.1120_0p2.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a.BASES MS/crichtonetal.CP.2021 muffin.CB.umQ15p0a.BASES.CASopen.1120_0p1.SPIN 10000
-------------------------------------------

### NOTES ######################################################

(i)

In the user-configs run for the paper, a more frequent, but reduced data saving was used.
This is becasue the nature of the model-data was 2D -- either surface or benthic fields.
Also, frequent time-slice saving was necessary in order to automatically identify the presence of oscillations in ocean circulation.
This is all specified by:

bg_par_data_save_level=7
# reduce output
bg_ctrl_data_save_2d=.true.
bg_ctrl_data_save_3d=.false.
# save frequency
bg_par_infile_slice_name='save_timeslice_EVERY000100.dat'
bg_par_infile_sig_name='save_timeseries_EVERY000100.dat'

(ii)

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

The specific value of atmospheric d13C is set to minimize the bias between model d13C and benthic data.

(iii)

For CAS open, the complete set of tuned d13C, FwF, and CO2 values are:

time (Ma)   atm d13CO2  FwF (Sv)    CO2 (ppm)
0.006	    -6.275	    0.300	    280
2.5	        -6.443	    0.300	    400
4.5	        -6.202	    0.500	    400
7.5	        -5.724	    0.400	    800
10	        -5.493	    0.400	    800
12.5	    -5.626	    0.200	    1120
15	        -5.103	    0.100	    1120

For CAS closed, the complete set of tuned d13C, FwF, and CO2 values are:

0.006	    -6.428	    0.200	    280
2.5	        -6.479	    0.100	    400
4.5	        -6.265	    0.200	    400
7.5	        -5.828      0.300	    800
10	        -5.738	    0.300	    800

### model experiments -- tracer diagnostics ####################

We also provide a parallel set of model configurations that include a compelte set of diagnostic tracers:

(r) numerical ventillation age
(0) preformed DIC
(2) preformed O2
(3) preformed PO4
(7) preformed d13C (of DIC)
(9) Csoft
(8) d13C of Csoft

The user-config files can be found in the subdirectory: tracers

Further details of these tracers can be found in the 'muffin' user manual,
Section 21.1 under 'Include ’preformed’ tracers'.

The commands to these experiments are:

-------------------------------------------
./runmuffin.sh muffin.CB.umQ00p0a.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ00p0a.BASEScolr023789.CASclosed.280_0p2.SPIN 10000
./runmuffin.sh muffin.CB.umQ00p0b.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ00p0b.BASEScolr023789.CASopen.280_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5a.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ02p5a.BASEScolr023789.CASopen.400_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5b.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ02p5b.BASEScolr023789.CASclosed.400_0p1.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5a.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ04p5a.BASEScolr023789.CASopen.400_0p5.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5b.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ04p5b.BASEScolr023789.CASclosed.400_0p2.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5a.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ07p5a.BASEScolr023789.CASopen.800_0p4.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5b.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ07p5b.BASEScolr023789.CASclosed.800_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0a.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ10p0a.BASEScolr023789.CASopen.800_0p4.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0b.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ10p0b.BASEScolr023789.CASclosed.800_0p3.SPIN 10000
./runmuffin.sh muffin.CB.umQ12p5a.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ12p5a.BASEScolr023789.CASopen.1120_0p2.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a.BASEScolr023789 MS/crichtonetal.CP.2021/tracers muffin.CB.umQ15p0a.BASEScolr023789.CASopen.1120_0p1.SPIN 10000
-------------------------------------------

################################################################
################################################################
################################################################

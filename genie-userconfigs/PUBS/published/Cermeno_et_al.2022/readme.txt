################################################################
### readme.txt #################################################
################################################################

For: 'Post-extinction recovery of the Phanerozoic oceans and biodiversity hotspots'

Pedro Cermeño,1,9, Carmen García-Comas,1,9, Alexandre Pohl,2,3, Simon Williams,4,5, Michael J. Benton,6, Chhaya Chaudhary,7, Guillaume Le Gland,1, R. Dietmar Müller,5, Andy Ridgwell,2 & Sergio M. Vallina,8

1Institut de Ciències del Mar, Consejo Superior de Investigaciones Científicas, Barcelona, Spain. 2Department of Earth and Planetary Sciences, University of California, Riverside, Riverside, CA,
USA. 3Biogéosciences, UMR 6282, UBFC/CNRS, Université Bourgogne Franche-Comté, Dijon, France. 4State Key Laboratory of Continental Dynamics, Department of Geology, Northwest
University, Xi’an, China. 5EarthByte Group, School of Geosciences, University of Sydney, Sydney, New South Wales, Australia. 6School of Earth Sciences, University of Bristol, Bristol, UK. 7Alfred
Wegener Institute, Helmholtz Centre for Polar and Marine Research, Bremerhaven, Germany. 8Instituto Español de Oceanografía, Consejo Superior de Investigaciones Científicas, Gijón, Spain.

################################################################
20/08/2021 -- README.txt file creation (A.P.)
20/08/2021 -- added files
25/08/2021 -- added notes, revised file directory
20/06/2022 -- revised for publication 
21/06/2022 -- adding instructions for alternative mean ocean [PO4] values + muffingen files
################################################################

Provided are the configuration files necessary to run the baseline experiments with present-day mean ocean [PO4],
from most recent (0 Ma / Holocene) back through time (to 541 Ma). 
The time in Ma is given in the configuration filename after 'M20Sc'.

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

Additional instructions are provided to run the simulations with alternative mean ocean [PO4] values 
used as sensitivity tests in Extended Data Fig. 3 g-i.

Muffingen .m instruction files and INPUT topography/bathymetry .dat files are provided for the record.

# =========== Modern [PO4] simulations =========== #

./runmuffin.sh muffin.AP.M20Sc0__.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc0__.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc15_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc15_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc26_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc26_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc36_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc36_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc52_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc52_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc61_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc61_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc69_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc69_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc75_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc75_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc87_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc87_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc97_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc97_.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc107.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc107.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc116.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc116.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc131.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc131.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc149.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc149.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc168.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc168.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc178.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc178.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc196.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc196.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc234.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc234.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc265.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc265.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc301.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc301.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc327.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc327.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc366.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc366.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc400.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc400.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc415.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc415.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc436.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc436.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc496.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc496.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc510.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc510.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc520.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc520.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc530.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc530.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000
./runmuffin.sh muffin.AP.M20Sc541.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Cermeno_et_al.2022 AP.M20Sc541.CO2solCadj.PO4.TdepCrichton2020.8P8Z_v4.SPIN 20000

# =========== Alternative [PO4] scenarios =========== #

To alter the mean ocean [PO4] value, add the following lines in the userconfigs provided above 
(probably making a copy with a different file name to keep track of the changes)
in the 'MISC' section (for instance, just before line '# age tracer, automatic method'):

To half mean ocean [PO4] compared to modern [PO4] simulations:
> # Initial global ocean [PO4] value (default: bg_ocn_init_8=2.159E-06)
> bg_ocn_init_8=1.0795e-06

To double mean ocean [PO4] compared to modern [PO4] simulations:
> # Initial global ocean [PO4] value (default: bg_ocn_init_8=2.159E-06)
> bg_ocn_init_8=4.318e-06

# =========== Muffingen files =========== #

First clone muffingen.
Set the MATLAB working dirctory to be the muffingenconfig subdirectory of this current directory, and >> addpath to where you have cloned muffingen.
Then run muffingen using the files provided.

For instance, for 0 Ma, you pass the name of the corresponding configuration file (M2020_merge_Haq60_0_1deg):

>> muffingen M2020_merge_Haq60_0_1deg 

(the latter, in turn, reading the corresponding 1 degree topography file 'INPUT/M2020_merge_Haq60_0_1deg.mat')

To increase the speed of configuration generation, in the configuration files, turn off the plotting:

% *** OPTIONS -- MAIN *************************************************** %
opt_plots=false;                % [false/true] plot all input and output?

################################################################
################################################################
################################################################

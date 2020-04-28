################################################################
### readme.txt #################################################
################################################################

For: ‘’
Jamie D. Wilson, Daniela N. Schmidt, Ben A. Ward and Andy Ridgwell

################################################################
08/11/2019 -- README.txt file creation (J.D.W)
################################################################

This readme details instructions to run example model experiments in the paper.
The example user-config files given, other than the spin-up, give an example for one combination of phytoplankton and zooplankton size classes.
To run with an alternative size-class combination, change the following parameter in the user-config file: eg_par_ecogem_plankton_file = #P#Z where user-defined numbers replace #s
The plankton definition file are found in ~/cgenie.muffin/genie-ecogem/data/input. THe naming convention is the number of phytoplankton and zooplankton, e.g., 2P4Z has 2 phytoplankton & 4 zooplankton.
Plankton are counted from the smallest size upwards, from 1 to 8, e.g., 1P is the smallest phytoplankton (0.6 um); 2P is the two smallest phytoplankton (0.6 and 1.9 um); 8P8Z is the full ecosystem.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################## Model Experiments ###########################

The commands to run the model configurations are:

(1) end-Cretaceous pre-extinction spin-up

./runmuffin.sh cgenie.eb_go_gs_ac_bg_eg.u067bc.BASES MS/wilsonetal_NG.2019 wilsonetal.u067c.PO4.export.8P8Z.SPIN 10000

(2) Extinction experiments

./runmuffin.sh cgenie.eb_go_gs_ac_bg_eg.u067bc.BASES MS/wilsonetal_NG.2019 wilsonetal.u067c.PO4.export.1P1Z 5000 wilsonetal.u067c.PO4.export.8P8Z.SPIN

(3) Extinction experiments- no size-dependent effect on POC:DOC export

./runmuffin.sh cgenie.eb_go_gs_ac_bg_eg.u067bc.BASES MS/wilsonetal_NG.2019 wilsonetal.u067c.PO4.1P1Z 5000 wilsonetal.u067c.PO4.export.8P8Z.SPIN

(4) Extinction experiments - no spatial heterogeneity in diversity

./runmuffin.sh cgenie.eb_go_gs_ac_bg_eg.u067bc.BASES MS/wilsonetal_NG.2019 wilsonetal.u067c.PO4restore.1P1Z 5000 wilsonetal.u067c.PO4.export.8P8Z.SPIN

################################################################
################################################################
################################################################

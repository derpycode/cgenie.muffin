################################################################
### README.txt #################################################
################################################################

For:
'The impact of marine nutrient abundance on early eukaryotic ecosystems'
Christopher T. Reinhard, Noah J. Planavsky, Ben A. Ward, Gordon D. Love, and Andy Ridgwell

################################################################
11/15/2019 -- README.txt file creation (C.T.R.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

### model experiments -- spinups ###############################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)


The commands to run the main model ensembles are listed as follows:


[1] Default climate state, all plankton configurations:

./runmuffin.sh {BASE} MS/reinhardetal.Geobiology.2020 {USER} 10000

where {BASE} is the base config --> muffin.CBE.fm0635ca.BASESCH4

and {USER} is the user config for the ensemble member, named and configured as:

eco.muffin.x_y_z_SPIN

x --> plankton scheme
	8P8Z   -- '2-guild' model
	9M     -- 'mixotroph-only' model
	32P32Z -- 'n64' model 
y --> atmospheric pO2 [10% PAL for all runs]
z --> ocean phosphate inventory [relative to modern]


[2] Additional climate sensitivity runs [all with '2-guild' ecosystem configuration]

eco.muffin.8P8Z_x_y_z_SPIN

x --> atmospheric pO2 [10% PAL for all runs]
y --> ocean phosphate inventory [relative to modern]
z --> atmospheric pCO2 [relative to modern]

################################################################
################################################################
################################################################

################################################################
### readme.txt #################################################
################################################################

For:
Overturning circulation, nutrient limitation, and warming in the Glacial North Pacific
J.W.B. Rae, W. Gray, R.C.J. Wills, I. Eisenman, B. Fitzhugh, E.F.M. Littley, P. Rafter, R Rees-Owen, A Ridgwell, B. Taylor, A. Burke

################################################################
20/09/30 -- README.txt file creation
################################################################

Provided is the code used to create the tuned model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### model experiments ##########################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The model spin for the pre-industrial state is run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx MS/raeetal.2020 SPIN.worjh2.Fe14C.preAge.Dye 20000

The model spin for the LGM state is run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx MS/raeetal.2020 SPIN.worjh2.Fe14C.preAge.Dye.LGM 20000

For the freshwater perturbation experiments, the following are run for 5000 years from the LGM equilibrium base state:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx MS/raeetal.2020 worjh2.Fe14C.preAge.Dye.LGM.PA35 5000 SPIN.worjh2.Fe14C.preAge.Dye.LGM

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx MS/raeetal.2020 worjh2.Fe14C.preAge.Dye.LGM.PA15 5000 SPIN.worjh2.Fe14C.preAge.Dye.LGM

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx MS/raeetal.2020 worjh2.Fe14C.preAge.Dye.LGM.PA-15 5000 SPIN.worjh2.Fe14C.preAge.Dye.LGM

The first experiment uses a Pacific-Atlantic (PA) freshwater factor of 0.35 which results in a -0.12 Sv freshwater forcing in the Pacific. N.B. the negative implies the Pacific is saltier.
The second experiment uses a Pacific-Atlantic (PA) freshwater factor of 0.15 which results in a -0.19 Sv freshwater forcing in the Pacific.
The second experiment uses a Pacific-Atlantic (PA) freshwater factor of -0.15 which results in a -0.28 Sv freshwater forcing in the Pacific.

### NOTES ######################################################

################################################################
################################################################
################################################################
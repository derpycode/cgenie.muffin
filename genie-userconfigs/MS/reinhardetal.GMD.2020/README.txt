################################################################
### readme.txt #################################################
################################################################

For:
'Oceanic and atmospheric methane cycling in the cGENIE Earth system model'
Christopher T. Reinhard, Stephanie L. Olson, Sandra Kirtland Turner, Cecily Pälike, Yoshiki Kanzaki, and Andy Ridgwell

################################################################
08/02/2019 -- README.txt file creation (C.T.R.)
2019/08/26 -- revision of README.txt file (A.R.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

# --------------------------------------------------------------
### model experiments -- benchmark spinups #####################
# --------------------------------------------------------------

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the two primary model benchmark configurations are listed as follows:

(1) 'modern' (high-O2) 10,000 year run:
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESCH4.iso MS/reinhardetal.GMD.2020 191210_GMD_mod_C06_20Tmol_SPIN 10000

(2) 'Proterozoic' (low-O2) 10,000 year run:
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESCH4.iso MS/reinhardetal.GMD.2020 191210_GMD_0.001PALO2_280uMSO4_SPIN 10000

# --------------------------------------------------------------
### model experiments -- atmospheric photochemistry ensembles ##
# --------------------------------------------------------------

For the ensembles used to test the alternative photochemical parameterizations (Figure 4):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESCH4.iso MS/reinhardetal.GMD.2020 191210_GMD_mod_X_Y_SPIN 10000

where the string X corresponds to either the Claire et al. (C06) or Schmidt and Shindell (SS03) schemes,
and the string Y corresponds to the imposed wetland CH4 flux in Tmol yr-1

# --------------------------------------------------------------
### model experiments -- time-dependent C injections ###########
# --------------------------------------------------------------

For the pulsed carbon injection experiment (as CH4 or CO2), based on the benchmark 'modern' spinup (Figure 9):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESCH4.iso MS/reinhardetal.GMD.2020 191215_GMD_mod_3000Pg_CH4_1kyr_PERT 10000 191210_GMD_mod_C06_20Tmol_SPIN
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESCH4.iso MS/reinhardetal.GMD.2020 191215_GMD_mod_3000Pg_CO2_1kyr_PERT 10000 191210_GMD_mod_C06_20Tmol_SPIN

# --------------------------------------------------------------
### model experiments -- O2 and SO4 ensembles ##################
# --------------------------------------------------------------

For the atmospheric O2 and marine SO4 ensembles (Figure 10a,b):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESCH4.iso MS/reinhardetal.GMD.2020 191210_GMD_X_Y_SPIN 10000

where the string X corresponds to the imposed atmospheric pO2 (relative to the present atmospheric level, PAL),
and the string Y corresponds to the initial marine SO4 concentration (in umol kg-1)

# --------------------------------------------------------------
### model experiments -- BEQ ensembles #########################
# --------------------------------------------------------------

For the biological energy quantum (BEQ) ensembles, based on the 'Proterozoic' benchmark spinup (Figure 10c):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESCH4.iso MS/reinhardetal.GMD.2020 200101_GMD_0.001PALO2_280uMSO4_BEQX_SPIN 10000

where the sub-string X (in the string BEQX) denotes the biological energy quantum (in kJ mol-1)

################################################################
################################################################
################################################################

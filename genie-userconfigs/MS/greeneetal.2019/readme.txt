################################################################
### readme.txt #################################################
################################################################

For:
'Early Cenozoic Decoupling of Climate and Carbonate Compensation Depth Trends'
S.E. Greene, A. Ridgwell, S. Kirtland Turner, D.N. Schmidt, H. Pälike, E. Thomas, L.K. Greene, B.A.A. Hoogakker

################################################################
19/03/01 -- README.txt file creation (A.R.)
19/04/30 -- experiments checked and instructions revised (A.R.)
################################################################

Provided are the configuration files necessary to run the model experiments presented in the paper.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### model experiments -- spinups ################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the spinups are listed as follows:

(1a) INITIAL SPINUP

The initial, 1st-stage closed system spin-up as detailed in the SI (e.g. see Ridgwell and Hargreaves [2007]):

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/greeneetal.2019 spinup1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage open system and accelerated spin-up (e.g. see Ridgwell and Hargreaves [2007] and Lord et al. [2015]):

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/greeneetal.2019 spinup2 200000 spinup1

### model experiments -- main ensemble ##########################

For the experiments comprising ensemble #1:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/greeneetal.2019 ensemble1.xypzCO2outgas 2000000 spinup2

where the sub-string xypz (in the string ensemble1.xypzCO2outgas) is one of: x0p6, x0p7, ...
x1p9, x2p0, corresponding to CO2 out-gassing flux enhancements of ×0.6, ×0.7, ... ×1.9, ×2.0.

### model experiments -- further ensembles ######################

Please contact the authors for the model parameter file configurations.

################################################################
################################################################
################################################################

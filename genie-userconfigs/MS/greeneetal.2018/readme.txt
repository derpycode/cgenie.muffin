################################################################
### readme.txt #################################################
################################################################

For:
'Early Cenozoic Decoupling of Climate and Carbonate Compensation Depth Trends'
S.E. Greene, A. Ridgwell, S. Kirtland Turner, D.N. Schmidt, H. Pälike, E. Thomas, L.K. Greene, B.A.A. Hoogakker

################################################################
18/03/5 -- README.txt file creation (A.R.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### code version and installation ###############################

The specific svn version of the code that runs the experiments, is:

r.10211

(but the expectation is that the current (head) version will also always replicate the published results)

Instructions for installing the model code, 
as well as a series of tutorials for learning how to configure, run, and analyses the model output, can be  found here:

http://www.seao2.info/mycgenie.html

See 'got muffin?' quick start guides for installing the code.

Refer to the cGENIE.muffin combined user-manual and an introduction to Earth system modelling tutorials for a complete set of model documentation:

http://www.seao2.info/cgenie/docs/muffin.pdf

### model experiments -- spinups ################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the spinups are listed as follows:

(1a) INITIAL SPINUP

The initial, 1st-stage closed system spin-up as detailed in the SI (e.g. see Ridgwell and Hargreaves [2007]):

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/greeneetal.2018 spinup1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage open system and accelerated spin-up (e.g. see Ridgwell and Hargreaves [2007] and Lord et al. [2015]):

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/greeneetal.2018 spinup1 200000 spinup1

### model experiments -- main ensemble ##########################

For the experiments comprising ensemble #1:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/greeneetal.2018 ensemble1.xypzCO2outgas 2000000 spinup2

where the sub-string xypz (in the string ensemble1.xypzCO2outgas) is one of: x0p6, x0p7, ...
x1p9, x2p0, corresponding to CO2 out-gassing flux enhancements of ×0.6, ×0.7, ... ×1.9, ×2.0.

### model experiments -- further ensembles ######################

Please contact the authors for the model parameter file configurations.

################################################################
################################################################
################################################################

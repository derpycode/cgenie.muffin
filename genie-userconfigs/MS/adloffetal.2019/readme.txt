################################################################
### readme.txt #################################################
################################################################

For:
'Unraveling the sources of carbon emissions at the onset of OAE 1a'
M. Adloff, S.E. Greene, I.J. Parkinson, F.M. Monteiro, B.D.A. Naafs, W. Preston, A. Ridgwell, D. Lunt and J.M. Castro Jiménez

################################################################
06/03/2019 -- README.txt file creation (M.A.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### code version and installation ###############################

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

The initial, 1st-stage closed system spin-up as detailed in the Methods section of the paper:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ spinup1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage open system and accelerated spin-up:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ spinup2 500000 spinup1

### model experiments -- main ensemble ##########################

For the scenarios presented in the main body of text of the paper:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ scenarioX_20kyrs 20000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ scenarioX_40kyrs 40000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ scenarioX_100kyrs 100000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ scenarioX_300kyrs 300000 spinup2

where the sub-string X (in the string scenarioX) is one of: 1,2,3,4 or 5 
corresponding to the pCO2 change scenarios defined in the paper.

### model experiments -- further sensitivity experiments ######################

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ scenarioX_100kyrs 100000 spinup2

where the sub-string X (in the string scenarioX) is 6 or Bauer2017
corresponding to scenarios with larger pCO2 changes than reconstructed by Naafs et al. 2016. Scenario6 is defined in the SI of the paper. ScenarioBauer2017 assumes a linear pCO2 increase by 3000ppm.

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0120.BASE MS/adloffetal.2019/ scenario3_100kyrs_XYZ 100000 spinup2

where the sub-string XYZ (in the string scenario3_100kyrs_XYZ) is one of: modernPO4 4xCO2 highAlk nobiotemp
corresponding to experiments with prescribed modern day nutrient inventory (modernPO4), initial CO2 concentration 4 times the preindustrial (4xCO2), increased initial alkalinity (highAlk) and without a temperature dependent biological pump (nobiotemp), respectively. 

Please contact the authors for the model parameter file configurations.

################################################################
################################################################
################################################################

################################################################
### README.txt #################################################
################################################################

For:
'Negative Carbon Isotope Excursions: an interpretative framework'
P. Vervoort, M. Adloff, S.E. Greene S. Kirtland Turner

################################################################
07/11/2019 -- README.txt file creation (P.V)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an opportunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### code version and installation ###############################

The specific svn version of the code that runs the experiments, is:

r.10203

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

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.nCIE.BASE MS/vervoortetal.2019 RWLMA834.SPIN1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage open system and accelerated spin-up (e.g. see Ridgwell and Hargreaves [2007] and Lord et al. [2015]):

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.nCIE.BASE MS/vervoortetal.2019 RWLMA834.SPIN2gl 200000 RWLMA834.SPIN1

### model experiments -- main ensemble ##########################

For nCIEs with a total duration of 50 kyr - for example:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.nCIE.BASE MS/vervoortetal.2019 CS05_CD050_CC06_11.warm 50000 RWLMA834.SPIN2gl

For nCIEs with a total duration of 100 kyr - for example:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.nCIE.BASE MS/vervoortetal.2019 CS05_CD100_CC06_11.warm 100000 RWLMA834.SPIN2gl

For nCIEs with a total duration of 300 kyr - for example:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.nCIE.BASE MS/vervoortetal.2019 CS05_CD300_CC06_11.warm 300000 RWLMA834.SPIN2gl

where 
CS05 is used for nCIE of size 0.5 permil. Other options are:
	CS10 (1.0 permil)
	CS30 (3.0 permil)
	CS60 (6.0 permil)
CD050 is used for nCIEs with total duration of 50 kyr. Other options are:
	CD100 (100 kyr) 
	CD300 (300 kyr)
CC06 is used to select carbon input with an isotopic signature of -6 permil. Other options are:
	CC12 (-12 permil)
	CC22 (-22 permil)
	CC60 (-60 permil)
11 is used to indicate the shape of a nCIE - rapid onset duration without d13C over- or undershoot. Other options are:
	12 (rapid onset - overshoot)
	13 (rapid onset - undershoot)
	21 (equal duation of onset and recovery phase - without over- or undershoot)
	22 (equal duation of onset and recovery phase - overshoot)
	23 (equal duation of onset and recovery phase - undershoot)
	31 (slow onset - without over- or undershoot)
	32 (slow onset - overshoot)
	33 (slow onset - undershoot)

### model experiments -- further ensembles ######################

...

################################################################
################################################################
################################################################

################################################################
### README.txt #################################################
################################################################
For:
'A distinctive Lithium isotope signature of extreme carbon cycling following snowball Earth'
Philip A.E. Pogge Von Strandmann, Andy Ridgwell, Simone A. Kasemann, Tim Elliott
################################################################
17/05/18 -- README.txt file creation (A.R.)
################################################################

Provided is the exact state of the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### compiling and testing the model ############################

For installing and running the ‘cGENIE’ Earth system model on a linux box (e.g. Ubuntu) or a Mac:
sets of instructions (‘Quick-Start Guides’) are available from:
http://www.seao2.info/mycgenie.html 
(in the ‘got muffin?’ box on the left).

Also refer to the  cGENIE User Manual -- cGENIE.muffin.User_manual.pdf
(http://www.seao2.info/mycgenie.html)

### model experiments ##########################################

All experiments are run from $HOME/cgenie.muffin/genie-main.
The commands to do this are listed as follows:

(1) INITIAL SPINUP
The initial, 1st-stage closed system spin-ups, for which there are 4 different configured varients of the marine Li cycle:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.worjh2.BASES TALK/USC.2017 EXAMPLE.worjh2.Archeretal2009.SPIN1 20000

(2) EXTENDED PRE-GLACIAL SPINUP




################################################################
################################################################
################################################################

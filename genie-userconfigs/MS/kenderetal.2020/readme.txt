################################################################
### readme.txt #################################################
################################################################

For:
xxx

################################################################
20/01/08 -- README.txt file creation (A.R.)
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

The model spin-up is conducted in 2 stages:


./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.SPIN1 20000

 



The commands to run the inversion experiments are listed as follows:

-------------------------------------------
./runmuffin.sh muffin.CB.umQ00p0a MS/crichtonetal.CP.2019 muffin.CB.umQ00p0a.SPIN 10000
./runmuffin.sh muffin.CB.umQ02p5a MS/crichtonetal.CP.2019 muffin.CB.umQ02p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ04p5a MS/crichtonetal.CP.2019 muffin.CB.umQ04p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ07p5a MS/crichtonetal.CP.2019 muffin.CB.umQ07p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ10p0a MS/crichtonetal.CP.2019 muffin.CB.umQ10p0a.SPIN 10000
./runmuffin.sh muffin.CB.umQ12p5a MS/crichtonetal.CP.2019 muffin.CB.umQ12p5a.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a MS/crichtonetal.CP.2019 muffin.CB.umQ15p0a.SPIN 10000
-------------------------------------------

### NOTES ######################################################


################################################################
################################################################
################################################################

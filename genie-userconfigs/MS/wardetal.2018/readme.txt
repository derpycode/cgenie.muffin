################################################################
### readme.txt #################################################
################################################################

For:
'EcoGEnIE 0.1: Plankton Ecology in the cGENIE Earth system model'
Ben A. Ward, Jamie D. Wilson, Ros M. Death, Fanny M. Monteiro, Andrew Yool and Andy Ridgwell

################################################################
06/05/2018 -- README.txt file creation (B.A.W.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

### code version ###############################################

The specific svn version of the code that runs the experiments, is:

r.????

(but the expectation is that the current (head) version will also always replicate the published results)

### model experiments -- spinups ################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the two model configurations are listed as follows:

(1) BIOGEM 10,000 year run

runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASESFeTDTL / BENW.worjh2.PO4FeTDTL_HYBRID_new.SPIN 10000

(2) ECOGEM 10,000 year run

runmuffin.sh cgenie.eb_go_gs_ac_bg_eg.worjh2.BASESFeTDTL / BSS.8P8Z_noresp.SPIN 10000

################################################################
################################################################
################################################################

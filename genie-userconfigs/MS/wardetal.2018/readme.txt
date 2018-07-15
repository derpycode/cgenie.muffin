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

### model experiments -- spinups ###############################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the two model configurations are listed as follows:

(1) BIOGEM 10,000 year run (with mixed layer depth scheme for direct comparison with ECOGEM experiment)

./runmuffin.sh muffin.CB.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.BIOGEM_MLD.SPIN 10000

(2) ECOGEM 10,000 year run (d13C is enaled in ECOGEM for completeness, but not described in this paper)

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN 10000

(3) standard (no mixed layer depth scheme) BIOGEM 10,000 year run (not shown in paper)

./runmuffin.sh muffin.CB.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.BIOGEM.SPIN 10000

################################################################
################################################################
################################################################

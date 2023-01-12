################################################################
### readme.txt #################################################
################################################################

For:
'A diatom extension of the EcoGEnIE Earth system model - DiatomEcoGEnIE.v0'
Aaron A. Naidoo-Bagwell, Fanny M. Monteiro, Katharine R. Hendry, Scott Burgan, Jamie D. Wilson, Ben A. Ward, Andy Ridgwell and Daniel J. Conley

################################################################
14/09/2022 -- README.txt file creation (A.A.N.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

### model experiments -- spinups ###############################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the two model configurations are listed as follows:

(1) DiatomEcoGEnIE 10,000 year run (with mixed layer depth scheme for direct comparison with ECOGEM experiment)

./runmuffin.sh muffin.CB.worlg4.BASESFeTDTLSi.Albani MS/wardetal.2018 2000 diat.worjh2.albani

(2) EcoGEnIE 10,000 year run (d13C is enaled in ECOGEM for completeness, but not described in this paper)

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN 10000

(3) NoDiatom BIOGEM 10,000 year run (not shown in paper)

./runmuffin.sh muffin.CB.worlg4.BASESFeTDTLSi.Albani MS/wardetal.2018 2000 diat.worjh2.albani

################################################################
################################################################
################################################################

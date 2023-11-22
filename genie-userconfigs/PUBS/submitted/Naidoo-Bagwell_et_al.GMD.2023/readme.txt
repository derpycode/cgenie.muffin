################################################################
### readme.txt #################################################
################################################################

For:
'A diatom extension of the EcoGEnIE Earth system model - EcoGEnIE 1.1'
Aaron A. Naidoo-Bagwell, Fanny M. Monteiro, Katharine R. Hendry, Scott Burgan, Jamie D. Wilson, Ben A. Ward, Andy Ridgwell and Daniel J. Conley

################################################################
15/11/2023 -- README.txt file creation (A.A.N.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

### model experiments -- spinups ###############################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the model configurations are listed as follows:

Ensure base config muffin.CBE.worjh2.BASESFeTDTLSi.Albani is in cgenie.muffin/genie-main/configs
Ensure config file 3Diat_4ZP_PiEu.eco is in cgenie.muffin/genie-ecogem/data

Initial new physics spin :

./runmuffin.sh muffin.CBE.worjh2.BASESFeTDTLSi.Albani PUBS/submitted/Naidoo-Bagwell_et_al.GMD.2023 diat.worjh2.Albani 20000

Once the spin has completed: 

(1) EcoGEnIE 1.1 2,000 year run 

./runmuffin.sh muffin.CBE.worjh2.BASESFeTDTLSi.Albani PUBS/submitted/Naidoo-Bagwell_et_al.GMD.2023 EcoGEnIE1.1 2000 diat.worjh2.Albani

(2) EcoGEnIE 1.0 from Ward et al. (2018)

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN 10000

(3) Additional runs

./runmuffin.sh muffin.CBE.worjh2.BASESFeTDTLSi.Albani PUBS/submitted/Naidoo-Bagwell_et_al.GMD.2023 NoDiatom 2000 diat.worjh2.Albani
./runmuffin.sh muffin.CBE.worjh2.BASESFeTDTLSi.Albani PUBS/submitted/Naidoo-Bagwell_et_al.GMD.2023 EcoGEnIE1.1_phys 2000 diat.worjh2.Albani
./runmuffin.sh muffin.CBE.worjh2.BASESFeTDTLSi.Albani PUBS/submitted/Naidoo-Bagwell_et_al.GMD.2023 EcoGEnIE1.1_phys_eco 2000 diat.worjh2.Albani
################################################################
################################################################
################################################################

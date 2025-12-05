################################################################
### readme.txt #################################################
################################################################

For:
'Long-term impacts of mixotrophy on ocean carbon storage: insights from a 10,000-year global model simulation'
Marco Puglia, Thomas Bibby, Jamie Wilson and Ben Ward

################################################################
27/11/2025 -- README.txt file creation 
################################################################

Provided is the code used to create the model experiments presented in the paper.

Also given are the configuration files necessary to run the model experiments.

### ecosystem configuration files ##############################

8M.eco and 8P8M8Z.eco need to be copied into ~/cgenie.muffin/genie-ecogem/data/input/.

### ecosystem user config files ################################

????, ???? and  ???? need to be copied to ????

################################################################
### model spinup and experiments ###############################
################################################################

All experiments are run from:

$HOME/cgenie.muffin/genie-main

(unless a different installation directory has been used)

The commands to run the model configurations are listed as follows:

################################################################
### Initial 10-year spin-up with 8P8Z config:

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_spinup 10000 > stdout.txt 2> stderr.txt & 

Once the spin has completed: 

################################################################
### Two-guild simulation

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_fromPZspinup 1000 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_spinup

################################################################
### Mixotrophic simulation

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN_M_LowFeQ_FLUX_fromPZspinup 1000 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_spinup

################################################################
### Additional sensitivity runs

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN_PMZ1.00_LowFeQ_FLUX_fromPZspinup 1000 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_spinup

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN_PMZ0.60_LowFeQ_FLUX_fromPZspinup 1000 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_spinup

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN_PMZ0.50_LowFeQ_FLUX_fromPZspinup 1000 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_spinup

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/wardetal.2018 wardetal.2018.ECOGEM.SPIN_PMZ0.40_LowFeQ_FLUX_fromPZspinup 1000 wardetal.2018.ECOGEM.SPIN_PZ_LowFeQ_FLUX_spinup

################################################################
################################################################

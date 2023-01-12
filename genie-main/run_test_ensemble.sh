#!/bin/bash
#
# *** SUBMITS ENSEMBLE TO CLUSTER***
# *** set up list of user-config files and submit to iwan ***
# *** Created 27/07/2015 by JDW ***

# ----------------------
# USER VARIABLES % Change accordingly 
# ----------------------
BASECONFIG='muffin.CBE.worjh2.BASESFeTDTLSi.Albani' 
RESTART='diat.worjh2.Albani' 
CONFIG_DIR='AARON'
YEARS='2000'
# ----------------------

# create list of user-config files from directory using wildcard (edit to find files in directory) (pipe to xargs removes directory listing)
config_list="$(ls ~/cgenie.muffin/genie-userconfigs/AARON/EXTRAPOCfixed_* | xargs -n 1 basename)"
#config_list="$(ls ~/test_scripts/EXTRAPOCfixed_* | xargs -n 1 basename)"
# loop over variables
while IFS= read
do
   #echo $REPLY
   qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh $BASECONFIG $CONFIG_DIR $REPLY $YEARS $RESTART
   sleep 15
   #qstat -f
done <<< $"$config_list"
	



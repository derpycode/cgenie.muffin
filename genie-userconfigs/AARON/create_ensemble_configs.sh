#!/bin/bash
#
# *** GENERATE USER-CONFIG FILES FOR Si ENSEMBLE ***
# *** read in latin hypercube sample parameters and create new user-config files ***
# *** Created 6/07/2015 by JDW ***

# --------------
# USER VARIABLES
# --------------
SPINFILE='diatom.SPIN'
PARAMSFILE='lhs_data'
n_samples=3
# --------------

# extra variables in script
DATEID=$(date +%y)$(date +%m)$(date +%d)
n_count=1


# loop to create user-config files
while IFS="," read param1 param2 param3
do

	echo $param1
	echo $param2
	echo $param3
	
	if [ $n_count -lt 10 ]; then
	ENSCONFIG='Si_ens_0'$n_count
	else
	ENSCONFIG='Si_ens_'$n_count
	fi
	
	cp $SPINFILE $ENSCONFIG
	
	echo "Ensemble created: $DATEID" >> $ENSCONFIG
        echo "bg_par_bio_remin_fun='KriestOschlies2008'" >> $ENSCONFIG
	echo "bg_par_bio_remin_POC_eL0=$param1" >> $ENSCONFIG
	echo "bg_par_bio_remin_POC_size0=$param2" >> $ENSCONFIG
	echo "bg_par_bio_remin_POC_eta=$param3" >> $ENSCONFIG
	
	let n_count=$n_count+1
	
done < $PARAMSFILE
#!/bin/bash
#
# *** GENERATE USER-CONFIG FILES FOR Si ENSEMBLE ***
# *** read in latin hypercube sample parameters and create new user-config files ***
# *** Created 6/07/2015 by JDW ***

# --------------
# USER VARIABLES
# --------------
SPINFILE='diat.worjh2.Albani'
PARAMSFILE='qmin_sensitivity'
n_samples=2
# --------------

# extra variables in script
DATEID=$(date +%y)$(date +%m)$(date +%d)
n_count=1


# loop to create user-config files
while IFS="," read param1 param2 
do

	echo $param1
	echo $param2
	


	
	if [ $n_count -lt 10 ]; then
	ENSCONFIG='qmin_sensitivity_0'$n_count
	else
	ENSCONFIG='qmin_sensitivity_'$n_count
	fi
	
	cp $SPINFILE $ENSCONFIG
	
	echo "Ensemble created: $DATEID" >> $ENSCONFIG
          echo "eg_qminP_a=$param1" >> $ENSCONFIG
	echo "eg_qmaxP_a="$(awk "BEGIN {print $param1*$param2}") >> $ENSCONFIG
		let n_count=$n_count+1
	
done < $PARAMSFILE
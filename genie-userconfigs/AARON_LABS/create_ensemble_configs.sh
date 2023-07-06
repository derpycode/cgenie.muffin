#!/bin/bash
#
# *** GENERATE USER-CONFIG FILES FOR Si ENSEMBLE ***
# *** read in latin hypercube sample parameters and create new user-config files ***
# *** Created 6/07/2015 by JDW ***

# --------------
# USER VARIABLES
# --------------
SPINFILE='dGEnIE.SPIN'
PARAMSFILE='size_EP'
n_samples=3
# --------------

# extra variables in script
DATEID=$(date +%y)$(date +%m)$(date +%d)
n_count=1


# loop to create user-config files
while IFS="," read param1 param2 param3 #param4 param5 param6 param7 param8 param9 param10 param11 param12 param13 param14 param15 param16 param17 param18
do

	echo $param1
	#echo $param2
	#echo $param3 
          #echo $param4
	#echo $param5
	#echo $param6
         # echo $param7
	#echo $param8
	#echo $param9
	#echo $param10
         # echo $param11
	#echo $param12
         # echo $param13
	#echo $param14
         # echo $param15
	#echo $param16
         #echo $param17
	#echo $param18



	
	if [ $n_count -lt 10 ]; then
	ENSCONFIG='dGEnIE_size_0'$n_count
	else
	ENSCONFIG='dGEnIE_size_'$n_count
	fi
	
	cp $SPINFILE $ENSCONFIG
	
	echo "Ensemble created: $DATEID" >> $ENSCONFIG
         
         echo "bg_par_bio_remin_fun='KriestOschlies2008'" >> $ENSCONFIG

         echo "bg_par_bio_remin_POC_eL0= $param2" >> $ENSCONFIG
         echo "bg_par_bio_remin_POC_size0=$param3"  >> $ENSCONFIG
         echo "bg_par_bio_remin_POC_eta=$param1"   >> $ENSCONFIG



       # echo "eg_qminP_a=$param1" >> $ENSCONFIG
	#echo "eg_qmaxP_a="$(awk "BEGIN {print $param1*$param2}") >> $ENSCONFIG
	##echo "eg_qminFe_a=$param3" >> $ENSCONFIG
	#echo "eg_qmaxFe_a="$(awk "BEGIN {print $param3*$param4}") >> $ENSCONFIG
	#echo "eg_qminSi_a=$param5" >> $ENSCONFIG
	#echo "eg_qmaxSi_a="$(awk "BEGIN {print $param5*$param6}") >> $ENSCONFIG
	#echo "eg_affinSiO2_a=$param7" >> $ENSCONFIG
	#echo "eg_affinSiO2_b=$param8" >> $ENSCONFIG
	##echo "eg_affinPO4_a=$param9" >> $ENSCONFIG
	#echo "eg_affinPO4_b=$param10" >> $ENSCONFIG
	#echo "eg_affinFe_a=$param11" >> $ENSCONFIG
	#echo "eg_affinFe_b=$param12" >> $ENSCONFIG
	#echo "eg_vmaxFe_a=$param13" >> $ENSCONFIG
         #echo "eg_vmaxFe_b=$param14" >> $ENSCONFIG
	##echo "eg_vmaxSiO2_a=$param1" >> $ENSCONFIG	
          #echo "eg_vmaxSiO2_b=$param2" >> $ENSCONFIG
          #echo "eg_par_diatom_palatability_mod=$param17" >> $ENSCONFIG
	#echo "bg_par_bio_remin_sinkingrate_reaction=$param18" >> $ENSCONFIG	
	let n_count=$n_count+1
	
done < $PARAMSFILE
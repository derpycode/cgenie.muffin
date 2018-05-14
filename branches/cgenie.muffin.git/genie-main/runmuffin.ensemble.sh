#!/bin/bash -e
#
#####################################################################
### SCIPT TO CREATE AND SUBMIT ENSEMBLES ############################
#####################################################################
#
# ALL NEEDS MUCH EDITING!

# set experiment paths etc.
CONFIGDIR='_MS/endK/'
CONFIGPATH=$HOME'/cgenie.muffin/genie-userconfigs/'$CONFIGDIR
TEMPLATEID=u067bc.x3CO2.0p30RR.SPINgl

# set date
DATEID=$(date +%y)$(date +%m)$(date +%d)
###DATEID='100606l'

# LOOP
memberi=10
while [ $memberi -lt 41 ]; do

  # set index and userconfig name
  MEMBERINDEXI=$memberi
  MEMBERID=$DATEID"."$TEMPLATEID".i"$MEMBERINDEXI

  # copy template user config file
  cp $CONFIGPATH/$TEMPLATEID $CONFIGPATH/$MEMBERID

  # append to userconfig
  ###memberid_alt=$memberi-5
  NAMELIST='rg_par_weather_CaCO3='$memberi'e+012'
  echo $NAMELIST >> $CONFIGPATH/$MEMBERID
  
  # submit
  qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.u067bc.BASESFe $CONFIGDIR $MEMBERID 200000 140330.u067bc.x3CO2.SPIN
  sleep 15
  qstat -f

  let memberi=$memberi+2

done

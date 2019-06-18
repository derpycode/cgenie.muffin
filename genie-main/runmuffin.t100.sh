#!/bin/bash -e
#
#####################################################################
### SCIPT TO META-CONFIGURE AND RUN CGENIE.MUFFIN ###################
#####################################################################
#
echo ""
#
# (0) USER OPTIONS
# ----------------
#####################################################################
# CHANGE THIS FOR INSTALLATIONS OTHER THAN IN $HOME
# SET THE SAME AS IN user.mak AND user.sh
# set home directory
HOMEDIR=$HOME
#####################################################################
# set output directory
OUTPUTDIR=$HOMEDIR/cgenie_output
# ensure rocks can find xsltproc
export PATH=$PATH:/opt/rocks/bin:/usr/bin
export PATH=$PATH:/share/apps/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/lib
# also ifort ...
export PATH=/state/partition1/apps/intel/bin:$PATH
# ensure stack size is adequate
ulimit -s 20480
#
# (1) GET PASSED PARAMETERS
# -------------------------
# [1] base configuration ID
if [ -z "$1" ]; then
    echo "Usage: '$1' 1st parameter must be the base configuration ID"
    exit 65
  else
    MODELID="$1"
fi
# [2] set user (experiment) configuration file directory
if [ -z "$2" ]; then
    echo "Usage: '$2' 2nd parameter must be the user (experiment) configuration file directory"
    exit 65
  else
    GOINDIR=$HOMEDIR/cgenie.muffin/genie-userconfigs/"$2"
fi
# [3] set run ID (input run ID (= user configuration file name))
if [ -z "$3" ]; then
    echo "Usage: '$3' 3rd parameter must be the run ID"
    exit 65
  else
    RUNID="$3"
fi
if [ $(expr length "$3") -gt 127 ] ; then
    echo "Usage: '$3' 3rd parameter must be less than 128 characters in length"
    exit 65
fi
GOIN=$GOINDIR/$RUNID
# [4] set run duration
if [ -z "$4" ]; then
    echo "Usage: '$4' 4th parameter must be the run length (years)"
    exit 65
  else
    RUNLENGTH="$4"
fi
# [5] restart path (optional)
if [ -n "$5" ]; then
  RESTARTPATH=$OUTPUTDIR/"$5"
    if [ $(expr length "$5") -gt 127 ] ; then
        echo "Usage: '$5' 5th parameter must be less than 128 characters in length"
        exit 65
    fi
fi
#
# (2) SET LOCAL FILE AND DIRECTORY NAMES
# --------------------------------------
#
OMP_NUM_THREADS=2
export OMP_NUM_THREADS
#
OUTPUTPATH=$OUTPUTDIR/$RUNID
CONFIGPATH=$HOMEDIR/cgenie.muffin/genie-main/configs
CONFIGNAME=$RUNID".config"
BINARYPATH=$HOMEDIR/cgenie.muffin/genie-main
RESTARTNAME="rst.1"
#
# (3) CHECK PARAMETERS
# --------------------------------------
#
echo ">> Checking parameters ..."
#
#
if test -e $CONFIGPATH/$MODELID".config"
then
    echo "   #1 Experiment base configuration: "
    echo $CONFIGPATH/$MODELID".config"
    echo " found :)"
else
    echo "   #1 Experiment base configuration: "
    echo $CONFIGPATH/$MODELID".config"
    echo " CANNOT be found :("
    exit 1
fi
if test -d $GOINDIR
then
    echo "   #2 Experiment user configuration file directory: "
    echo $GOINDIR
    echo " found :)"
else
    echo "   #2 Experiment user configuration file directory: "
    echo $GOINDIR
    echo " CANNOT be found :("
    exit 1
fi
if test -e $GOIN
then
    echo "   #3 User-config file: "
    echo $GOIN
    echo " found :)"
else
    echo "   #3 User-config file: "
    echo $GOIN
    echo " CANNOT be found :("
    exit 1
fi
if [ $(expr length "$3") -gt 127 ] ; then
    echo "Usage: '$3' 3rd parameter must be less than 128 characters in length"
    exit 65
fi
if [ -n "$5" ]; then
    if test -e $RESTARTPATH
    then
        echo "   #5 Restart file: "
        echo $RESTARTPATH
        echo " found :)"
    else
        echo "   #5 Restart file: "
        echo $RESTARTPATH
        echo " CANNOT be found :("
        exit 1
    fi
    if [ $(expr length "$5") -gt 127 ] ; then
        echo "Usage: '$5' 5th parameter must be less than 128 characters in length"
        exit 65
    fi
else
    echo "   #5 NO restart specified"
fi
#
# (3) CREATE RUN CONFIG FILE
# --------------------------
echo ""
echo ">> Configuring ..."
# Copy template config file
cp -f $CONFIGPATH/$MODELID".config" $CONFIGPATH/$CONFIGNAME
# Set the experiment run name
#echo EXPID=$MODELID.$RUNID >> $CONFIGPATH/$CONFIGNAME
echo EXPID=$RUNID >> $CONFIGPATH/$CONFIGNAME
#
# (4) SET MODEL TIME-STEPPING
# ---------------------------
# extract ocean (lon,lat,lev) dimensions
LONS=$(grep -o '$(DEFINE)GOLDSTEINNLONS=..\>' $CONFIGPATH/$MODELID".config" | sed -e s/.*=//)
LATS=$(grep -o '$(DEFINE)GOLDSTEINNLATS=..\>' $CONFIGPATH/$MODELID".config" | sed -e s/.*=//)
LEVS=$(grep -o '$(DEFINE)GOLDSTEINNLEVS=..\>' $CONFIGPATH/$MODELID".config" | sed -e s/.*=//)
IGRID=$(grep -o 'go_grid=.' $CONFIGPATH/$MODELID".config" | sed -e s/.*=//)
# filter single digit format level ('8' vs. '08')
###if [ "$LEVS" == "8\'" ] || [ "$LEVS" == "8\"" ]; then
if [ "$LEVS" != "32" ] && [ "$LEVS" != "24" ] && [ "$LEVS" != "16" ] && [ "$LEVS" != "08" ]; then
    let LEVS=8
fi
# define relative biogeochem time-stepping
if [ $LONS -eq 36 ] && [ $LEVS -eq 16 ]; then
    let N_TIMESTEPS=100
    let dbiostp=2
elif [ $LONS -eq 36 ] && [ $LEVS -eq 8 ]; then
    let N_TIMESTEPS=100
    let dbiostp=4
elif [ $LONS -eq 18 ] && [ $LEVS -eq 16 ]; then
    let N_TIMESTEPS=50
    let dbiostp=1
elif [ $LONS -eq 18 ] && [ $LEVS -eq 8 ]; then
    let N_TIMESTEPS=50
    let dbiostp=2
elif [ $LONS -eq 12 ] && [ $LEVS -eq 8 ]; then
    let N_TIMESTEPS=50
    let dbiostp=2
elif [ $LONS -eq 36 ] && [ $LEVS -eq 32 ]; then
    let N_TIMESTEPS=100
    let dbiostp=1
elif [ $LONS -eq 48 ] && [ $LEVS -eq 16 ]; then
    let N_TIMESTEPS=100
    let dbiostp=2
else
    let N_TIMESTEPS=100
    let dbiostp=1
fi
if [ $IGRID -eq 1 ]; then
    echo "   Making non-equal area grid modifications to time-stepping, igrid: " $IGRID
    N_TIMESTEPS="$(echo "4*$N_TIMESTEPS" | bc -l)"
    dbiostp="$(echo "4*$dbiostp" | bc -l)"
    let datmstp=5
else
    let datmstp=5
fi
echo "   Setting time-stepping [GOLDSTEIN, BIOGEM:GOLDSTEIN]: " $N_TIMESTEPS $dbiostp
# define primary model time step
# c-goldstein; e.g. ma_genie_timestep = 365.25*24.0/(5*96) * 3600.0 (GOLDSTEIN year length)
#                => ma_genie_timestep=65745.0
dstp="$(echo "3600.0*24.0*365.25/$datmstp/$N_TIMESTEPS" | bc -l)"
echo "   Setting primary model time step: " $dstp
# write primary model time step
echo ma_genie_timestep=$dstp >> $CONFIGPATH/$CONFIGNAME
# write relative time-stepping
echo ma_ksic_loop=$datmstp >> $CONFIGPATH/$CONFIGNAME
echo ma_kocn_loop=$datmstp >> $CONFIGPATH/$CONFIGNAME
echo ma_klnd_loop=$datmstp >> $CONFIGPATH/$CONFIGNAME
echo ma_conv_kocn_kecogem=$dbiostp >> $CONFIGPATH/$CONFIGNAME
echo ma_conv_kocn_katchem=$dbiostp >> $CONFIGPATH/$CONFIGNAME
echo ma_conv_kocn_kbiogem=$dbiostp >> $CONFIGPATH/$CONFIGNAME
echo ma_conv_kocn_ksedgem=$N_TIMESTEPS >> $CONFIGPATH/$CONFIGNAME
echo ma_conv_kocn_krokgem=$dbiostp >> $CONFIGPATH/$CONFIGNAME
echo ma_kgemlite=$N_TIMESTEPS >> $CONFIGPATH/$CONFIGNAME
# set BIOGEM run length
echo bg_par_misc_t_runtime=$RUNLENGTH >> $CONFIGPATH/$CONFIGNAME
# set SEDGEM sediment age
echo sg_par_misc_t_runtime=$RUNLENGTH >> $CONFIGPATH/$CONFIGNAME
# set overall GENIE run length
let stp=$RUNLENGTH*$datmstp*$N_TIMESTEPS
echo ma_koverall_total=$stp >> $CONFIGPATH/$CONFIGNAME
echo ma_dt_write=$stp >> $CONFIGPATH/$CONFIGNAME
# set 'health check' frequency [NOTE: a '+1' in effect disables this feature]
let stp=$RUNLENGTH*$N_TIMESTEPS
echo ea_3=$stp >> $CONFIGPATH/$CONFIGNAME
echo go_3=$stp >> $CONFIGPATH/$CONFIGNAME
echo gs_3=$stp >> $CONFIGPATH/$CONFIGNAME
echo el_3=$stp >> $CONFIGPATH/$CONFIGNAME
# set climate model component restart frequency
let stp=$RUNLENGTH*$N_TIMESTEPS
echo ea_4=$stp >> $CONFIGPATH/$CONFIGNAME
echo go_4=$stp >> $CONFIGPATH/$CONFIGNAME
echo gs_4=$stp >> $CONFIGPATH/$CONFIGNAME
echo el_4=$stp >> $CONFIGPATH/$CONFIGNAME
# set 'time series' frequency [NOTE: a '+1' in effect disables this feature]
let stp=$RUNLENGTH*$N_TIMESTEPS+1
echo ea_5=$stp >> $CONFIGPATH/$CONFIGNAME
echo go_5=$stp >> $CONFIGPATH/$CONFIGNAME
echo gs_5=$stp >> $CONFIGPATH/$CONFIGNAME
echo el_5=$stp >> $CONFIGPATH/$CONFIGNAME
# set 'an average' frequency [NOTE: a '+1' in effect disables this feature]
let stp=$RUNLENGTH*$N_TIMESTEPS+1
echo ea_6=$stp >> $CONFIGPATH/$CONFIGNAME
echo go_6=$stp >> $CONFIGPATH/$CONFIGNAME
echo gs_6=$stp >> $CONFIGPATH/$CONFIGNAME
echo el_6=$stp >> $CONFIGPATH/$CONFIGNAME
# SET CLIMATE COMPONENTS TIME-STEPS PER YEAR
echo ea_9=$N_TIMESTEPS >> $CONFIGPATH/$CONFIGNAME
echo go_9=$N_TIMESTEPS >> $CONFIGPATH/$CONFIGNAME
echo gs_9=$N_TIMESTEPS >> $CONFIGPATH/$CONFIGNAME
#
# (5) SET CLIMATE MODEL RE-START FILE DETAILS
# -------------------------------------------
# Set default flags
# Set netCDF restart saving flag
echo ea_31=n >> $CONFIGPATH/$CONFIGNAME
echo go_19=n >> $CONFIGPATH/$CONFIGNAME
echo gs_14=n >> $CONFIGPATH/$CONFIGNAME
# Set ASCII restart output flag
echo ea_32=y >> $CONFIGPATH/$CONFIGNAME
echo go_20=y >> $CONFIGPATH/$CONFIGNAME
echo gs_15=y >> $CONFIGPATH/$CONFIGNAME
# Set ASCII restart number (i.e., output file string)
echo ea_29=rst >> $CONFIGPATH/$CONFIGNAME
echo go_17=rst >> $CONFIGPATH/$CONFIGNAME
echo gs_12=rst >> $CONFIGPATH/$CONFIGNAME
echo el_17="rst" >> $CONFIGPATH/$CONFIGNAME
echo el_24="rst.sland" >> $CONFIGPATH/$CONFIGNAME
#
# (6) CONFIGURE USE OF RESTART
# -----------------------------
# Set continuing/new run flags
# => set restart input flags
# => disable netCDF restart input flag
# => set restart input number
# => copy restart files to data directory
if [ -n "$5" ]; then
  echo ">> Checking whether restart directory $RESTARTPATH exists ..."
  if test -d $RESTARTPATH
  then
    echo "   OK :)"
  else
      echo "   Restart directory $RESTARTPATH cannot be found"
    exit 1
  fi
  echo ea_7=c >> $CONFIGPATH/$CONFIGNAME
  echo go_7=c >> $CONFIGPATH/$CONFIGNAME
  echo gs_7=c >> $CONFIGPATH/$CONFIGNAME
  echo el_7=c >> $CONFIGPATH/$CONFIGNAME
  echo ac_ctrl_continuing=t >> $CONFIGPATH/$CONFIGNAME
  echo bg_ctrl_continuing=t >> $CONFIGPATH/$CONFIGNAME
  echo sg_ctrl_continuing=t >> $CONFIGPATH/$CONFIGNAME
  echo rg_ctrl_continuing=t >> $CONFIGPATH/$CONFIGNAME
  echo eg_ctrl_continuing=t >> $CONFIGPATH/$CONFIGNAME
  echo ea_30=n >> $CONFIGPATH/$CONFIGNAME
  echo go_18=n >> $CONFIGPATH/$CONFIGNAME
  echo gs_13=n >> $CONFIGPATH/$CONFIGNAME
  echo el_18=n >> $CONFIGPATH/$CONFIGNAME
  echo ea_35=$RESTARTNAME >> $CONFIGPATH/$CONFIGNAME
  echo go_23=$RESTARTNAME >> $CONFIGPATH/$CONFIGNAME
  echo gs_18=$RESTARTNAME >> $CONFIGPATH/$CONFIGNAME
  echo el_2=$OUTPUTPATH"/ents" >> $CONFIGPATH/$CONFIGNAME
  echo el_22=$RESTARTPATH"/ents" >> $CONFIGPATH/$CONFIGNAME
  echo ea_rstdir_name=$RESTARTPATH"/embm" >> $CONFIGPATH/$CONFIGNAME
  echo go_rstdir_name=$RESTARTPATH"/goldstein" >> $CONFIGPATH/$CONFIGNAME
  echo gs_rstdir_name=$RESTARTPATH"/goldsteinseaice" >> $CONFIGPATH/$CONFIGNAME
  echo el_rstdir_name=$RESTARTPATH"/ents" >> $CONFIGPATH/$CONFIGNAME
  echo ac_par_rstdir_name=$RESTARTPATH"/atchem" >> $CONFIGPATH/$CONFIGNAME
  echo bg_par_rstdir_name=$RESTARTPATH"/biogem" >> $CONFIGPATH/$CONFIGNAME
  echo sg_par_rstdir_name=$RESTARTPATH"/sedgem" >> $CONFIGPATH/$CONFIGNAME
  echo rg_par_rstdir_name=$RESTARTPATH"/rokgem" >> $CONFIGPATH/$CONFIGNAME
  echo eg_par_rstdir_name=$RESTARTPATH"/ecogem" >> $CONFIGPATH/$CONFIGNAME
else
  echo ea_7=n >> $CONFIGPATH/$CONFIGNAME
  echo go_7=n >> $CONFIGPATH/$CONFIGNAME
  echo gs_7=n >> $CONFIGPATH/$CONFIGNAME
  echo el_7=n >> $CONFIGPATH/$CONFIGNAME
  echo ac_ctrl_continuing=f >> $CONFIGPATH/$CONFIGNAME
  echo bg_ctrl_continuing=f >> $CONFIGPATH/$CONFIGNAME
  echo sg_ctrl_continuing=f >> $CONFIGPATH/$CONFIGNAME
  echo rg_ctrl_continuing=f >> $CONFIGPATH/$CONFIGNAME
  echo eg_ctrl_continuing=f >> $CONFIGPATH/$CONFIGNAME
fi
# Set netCDF format biogeochem restart files
echo ac_ctrl_ncrst=.true. >> $CONFIGPATH/$CONFIGNAME
echo bg_ctrl_ncrst=.true. >> $CONFIGPATH/$CONFIGNAME
echo sg_ctrl_ncrst=.true. >> $CONFIGPATH/$CONFIGNAME
echo eg_ctrl_ncrst=.true. >> $CONFIGPATH/$CONFIGNAME
#
# (7) OVER-RIDE DEFAULTS
# ----------------------
echo bg_ctrl_force_oldformat=.false. >> $CONFIGPATH/$CONFIGNAME
#
# (8) INCORPORATE RUN CONFIGURATION
# ---------------------------------
echo "   Make safe goin format"
#dos2unix $GOIN
#tr ‘\r’ ‘\n’ < $GOIN
cat $GOIN >> $CONFIGPATH/$CONFIGNAME
#
# (9) GO!
# -------
# Run model ...
echo ""
echo ">> Here we go ..."
echo ""
cd $BINARYPATH
# test for change of base-config
if test -e 'current_config.dat' 
then
    current_config=$(<'current_config.dat')
    if [ "$current_config" != "$MODELID" ]; then
        echo ">> Use of different base-config detected, so ... (make cleanall) ..."
        echo ""
        sleep 2
        make cleanall
        echo "$MODELID" > 'current_config.dat'
    fi
else
    echo ">> WARNING: No record of last base-config (file: current_config.dat): CONTINUING ..."
    echo ""
    sleep 4
    make cleanall
    echo "$MODELID" > 'current_config.dat'
fi
./genie_example.job -O -f $CONFIGPATH/$CONFIGNAME
# Clean up and archive
rm -f $CONFIGPATH/$CONFIGNAME
echo ""
echo ">> Archiving ..."
cd $OUTPUTDIR
tar cfz $RUNID.tar.gz $RUNID
mv $RUNID.tar.gz $HOMEDIR/cgenie_archive/$RUNID.tar.gz
cd $BINARYPATH
echo ">> All done - now go and play outside"
echo ""
#

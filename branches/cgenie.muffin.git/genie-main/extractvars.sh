#!/bin/bash
# $Id$

# Script to extract a subset of the variables (also not
# all dimensions) from files created for the short tests

INDIR=$HOME/genie_output
OUTDIR=$HOME/genie/genie-knowngood/generic
TAG=knowngood

# Usage information
usage () {
    echo "usage: $0 [-i INDIR] [-o OUTDIR]"
}

# Give a warning if called with no arguments
if [ $# -eq 0 ]
then
  echo $0 WARNING: no arguments provided, using default variables.
fi

# Parse the arguments for available options
while getopts ":i:o:t:" option
do
  case $option in 
    i     ) INDIR=$OPTARG;;
    o     ) OUTDIR=$OPTARG;;
    t     ) TAG=$OPTARG;;
    \?    ) echo ERROR: -$OPTARG is an invalid flag
            usage 
            exit 1 ;;
    *     ) echo ERROR: Use of -$OPTARG flag requires argument
            usage  
            exit 1 ;;
  esac
done

# routines to pull out the vars and create the reduced files
process-igcm () {
    if [ -f $TARGFILE ]; then
	\mv -f $TARGFILE $TARGFILE.old
    fi
    # extract the lowest model level (highest presure) temperature on the 3rd day
    ncks -O -q -v temp -d time,2,2 -d p,6,6 $SRCFILE $TMPFILE
    # extract the net longwave radiation on the 3rd day
    ncks -O -q -v netlong -d time,2,2 $SRCFILE $TARGFILE
    # merge the two variables into a single file
    ncks -A $TMPFILE $TARGFILE
    \rm -f $TMPFILE
}

process-embm () {
    if [ -f $TARGFILE ]; then
	\mv -f $TARGFILE $TARGFILE.old
    fi
    # extract the lowest model level (highest presure) temperature on the 3rd day
    ncks -O -q -v temp -d depth,0,0 $SRCFILE $TMPFILE
    # extract the net longwave radiation on the 3rd day
    ncks -O -q -v netlong $SRCFILE $TARGFILE
    # merge the two variables into a single file
    ncks -A $TMPFILE $TARGFILE
    \rm -f $TMPFILE
}

echo "**** CREATING 'KNOWN GOODS' ****"

echo "Default config:"
SRCFILE=$INDIR/genie_assumedgood/igcm/igcm_da_2000_01.nc
TMPFILE=$OUTDIR/temp.nc
TARGFILE=$OUTDIR/genie_vanilla_${TAG}.nc
process-igcm

echo "ig_go_sl:"
SRCFILE=$INDIR/genie_ig_go_sl_dyex_assumedgood/igcm/igcm_da_2000_01.nc
TMPFILE=$OUTDIR/temp.nc
TARGFILE=$OUTDIR/genie_ig_go_sl_dyex_${TAG}.nc
process-igcm

echo "ig_go_gs:"
SRCFILE=$INDIR/genie_ig_go_gs_assumedgood/igcm/igcm_da_2000_01.nc
TMPFILE=$OUTDIR/temp.nc
TARGFILE=$OUTDIR/genie_ig_go_gs_${TAG}.nc
process-igcm

echo "eb_go_gs:"
SRCFILE=$INDIR/genie_eb_go_gs_assumedgood/goldstein/gold_spn_av_0005_00.nc
TMPFILE=$OUTDIR/temp.nc
TARGFILE=$OUTDIR/genie_eb_go_gs_${TAG}.nc
process-embm

echo "ig_fi_fi_ml:"
SRCFILE=$INDIR/genie_ig_fi_fi_ml_assumedgood/igcm/igcm_da_2000_01.nc
TMPFILE=$OUTDIR/temp.nc
TARGFILE=$OUTDIR/genie_ig_fi_fi_ml_${TAG}.nc
process-igcm

echo "ig_go_gs_ml:"
SRCFILE=$INDIR/genie_ig_go_gs_ml_assumedgood/igcm/igcm_da_2000_01.nc
TMPFILE=$OUTDIR/temp.nc
TARGFILE=$OUTDIR/genie_ig_go_gs_ml_${TAG}.nc
process-igcm



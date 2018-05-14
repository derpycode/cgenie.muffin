#!/bin/bash
# $Id$

# Script to copy a subset of the variables in one
# NetCDF file and save them in another.

NCKS=ncks

# default
RULESET=igcm

# Usage information
usage () {
    echo "usage: $0 [-ig] <SRCFILE> <TARGFILE>"
    echo "-i: use rules to extract from an ICGM output file (default)"
    echo "-g: use rules to extract from a GOLDSTEIN output file"
}

# Parse the arguments for available options
while getopts "ig" option
do
  case $option in 
    i     ) RULESET=igcm; shift ;;
    g     ) RULESET=goldstein; shift ;;
    \?    ) echo ERROR: -$OPTARG is an invalid flag
            usage 
            exit 1 ;;
    *     ) echo ERROR: Use of -$OPTARG flag requires argument
            usage  
            exit 1 ;;
  esac
done

# Must have <SRCFILE> and <TARGFILE> remaining
if [ $# -ne 2 ] ; then
    usage
    exit 1
fi

SRCFILE=$1
TARGFILE=$2

# routines to pull out the vars and create the reduced files
process-igcm () {
    if [ -f $TARGFILE ]; then
	\mv -f $TARGFILE $TARGFILE.old
    fi
    # extract the lowest model level (highest presure) temperature on the 3rd day
#    $NCKS -O -q -v temp -d time,2,2 -d p,6,6 $SRCFILE $TMPFILE
    $NCKS -O -q -v temp $SRCFILE $TMPFILE
    # extract the net longwave radiation on the 3rd day
#    $NCKS -O -q -v netlong -d time,2,2 $SRCFILE $TARGFILE
    $NCKS -O -q -v netlong $SRCFILE $TARGFILE
    # merge the two variables into a single file
    $NCKS -A $TMPFILE $TARGFILE
}

process-goldstein () {
    if [ -f $TARGFILE ]; then
	\mv -f $TARGFILE $TARGFILE.old
    fi
    # extract the lowest model level (highest presure) temperature on the 3rd day
#    $NCKS -O -q -v temp -d depth,0,0 $SRCFILE $TMPFILE
    $NCKS -O -q -v temp $SRCFILE $TMPFILE
    # extract the net longwave radiation on the 3rd day
    $NCKS -O -q -v netlong $SRCFILE $TARGFILE
    # merge the two variables into a single file
    $NCKS -A $TMPFILE $TARGFILE
}

# call the appropriate extraction routine according to the option
TMPFILE=/tmp/extract-tmp.nc
if [ $RULESET = "igcm" ] ; then
    process-igcm
else
    process-goldstein
fi

\rm -f $TMPFILE

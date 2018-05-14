#!/bin/bash
# $Id$

# Simple script to compare two files
# takes options for text/netcdf comparisions
# Output as per tests

OS=`uname -s`
DIFFTOOL=src/c/nccompare.exe

# Usage information
usage () {
    echo "usage: $0 [-tn] <FILEA> <FILEB>"
    echo "-t: compare text files"
    echo "-n: compare NetCDF files (default)"
}

# Parse command line for valid options
# Coding Notes:
# - no associated args--no trailing ':'s
# - 'shift' to pop the option off the arglist $@
# - 'shift 2' if the option had an associated arg
while getopts "tn" option
do
  case $option in 
    t     ) DIFFTOOL="diff --strip-trailing-cr"
	    shift ;;
    n     ) DIFFTOOL="src/c/nccompare.exe"
	    shift ;;
    \?    ) echo ERROR: -$OPTARG is an invalid flag
            usage 
            exit 1 ;;
    *     ) echo ERROR: Use of -$OPTARG flag requires argument
            usage  
            exit 1 ;;
  esac
done

# Must have <FILEA> and <FILEB> remaining
if [ $# -ne 2 ] ; then
    usage
    exit 1
fi

FILEA=$1
FILEB=$2

# Deal with Windows paths if under Cygwin
if [ $(echo $OS | grep -i "cygwin") ]; then
    FILEA=$(cygpath -m $FILEA)
    FILEB=$(cygpath -m $FILEB)
fi

# Make the comparison and report accordingly
if $DIFFTOOL $FILEA $FILEB; then
    echo "**TEST OK**"
    exit 0
else
    echo "**TEST FAILED**"
    exit 1
fi

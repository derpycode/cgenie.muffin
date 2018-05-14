#!/bin/bash
# $Id$

OS=`uname -s`
COMPILER=ifort
OUTDIR=~/genie_output
PROC=`uname -p`
NCCOMPARE=src/c/nccompare.exe
TXTDIFF="diff --strip-trailing-cr"

# Usage information
usage () {
    echo "usage: $0 [-d OUTDIR] [-c COMPILER]"
}

# remove unrequired files
cleanup () {
    rm -rf $OUTDIR/$EXPDIR
}

cleanupboth () {
    rm -rf $OUTDIR/$EXPDIR
    rm -rf $OUTDIR/$DATUMDIR
}

# Give a warning if called with no arguments
if [ $# -eq 0 ]
then
  echo $0 WARNING: no arguments provided, using default variables.
fi

# Parse the arguments for available options
while getopts ":d:c:" option
do
  case $option in 
    d     ) OUTDIR=$OPTARG;;
    c     ) COMPILER=$OPTARG;;
    \?    ) echo ERROR: -$OPTARG is an invalid flag
            usage 
            exit 1 ;;
    *     ) echo ERROR: Use of -$OPTARG flag requires argument
            usage  
            exit 1 ;;
  esac
done

if [ $(echo $OS | grep -i "cygwin") ]; then
    OUTDIR=$(cygpath -m $OUTDIR)
fi

echo "**** TESTING - LONGRUN ****"

echo "GENIEPREC-R8, IGCMATMOSPREC-R8:"
EXPDIR=genie_r8r8_regtest
DATUMDIR=genie_r8r8_assumedgood
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_01.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_01.nc; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "GENIEPREC-R8, IGCMATMOSPREC-R4:"
EXPDIR=genie_r8r4_regtest
DATUMDIR=genie_r8r4_assumedgood
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_01.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_01.nc; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "GENIEPREC-R4, IGCMATMOSPREC-R8:"
EXPDIR=genie_r4r8_regtest
DATUMDIR=genie_r4r8_assumedgood
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_01.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_01.nc; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "GENIEPREC-R8, IGCMATMOSPREC-R8, GOLDSTEIN-72, moses, tiedtke:"
EXPDIR=genie_ig_go72_gs72_ml_regtest
DATUMDIR=genie_ig_go72_gs72_ml_assumedgood
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_da_2000_01.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_da_2000_01.nc; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "GENIEPREC-R8, IGCMATMOSPREC-R8, GOLDSTEIN-6432, gaalbedo"
EXPDIR=genie_ig_go6432_sl_gaalbedo_regtest
DATUMDIR=genie_ig_go6432_sl_gaalbedo_assumedgood
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_01.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_01.nc; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "Water conservation (R8-R8) ig_go_sl :"
EXPDIR=genie_ig_go_sl_dyex_checkfluxes_regtest
echo "T" > $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt
if $TXTDIFF $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt \
    $OUTDIR/$EXPDIR/main/check_fluxes.txt; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "Water conservation (R8-R8) ig_go_sl_ml :"
EXPDIR=genie_ig_go_sl_ml_checkfluxes_regtest
echo "T" > $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt
if $TXTDIFF $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt \
    $OUTDIR/$EXPDIR/main/check_fluxes.txt; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "Water conservation (R8-R8) ig_go_gs :"
EXPDIR=genie_ig_go_gs_checkfluxes_regtest
echo "T" > $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt
if $TXTDIFF $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt \
    $OUTDIR/$EXPDIR/main/check_fluxes.txt; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "Water conservation (R8-R8) ig_go_gs_ml :"
EXPDIR=genie_ig_go_gs_ml_checkfluxes_regtest
echo "T" > $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt
if $TXTDIFF $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt \
    $OUTDIR/$EXPDIR/main/check_fluxes.txt; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "Water conservation (R8-R8) ig_go72_gs72_ml :"
EXPDIR=genie_ig_go72_gs72_ml_checkfluxes_regtest
echo "T" > $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt
if $TXTDIFF $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt \
    $OUTDIR/$EXPDIR/main/check_fluxes.txt; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "Water conservation (R8-R8) ig_go6432_sl :"
EXPDIR=genie_ig_go6432_sl_checkfluxes_regtest
echo "T" > $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt
if $TXTDIFF $OUTDIR/$EXPDIR/main/check_fluxes_datum.txt \
    $OUTDIR/$EXPDIR/main/check_fluxes.txt; then
    echo "**TEST OK**"
    cleanup
else
    echo "**TEST FAILED**"
fi

echo "IG_GO_SL RESTARTS (R8-R8) :"
EXPDIR=genie_ig_go_sl_restartread_regtest
DATUMDIR=genie_ig_go_sl_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

echo "IG_GO_GS RESTARTS (R8-R8) :"
EXPDIR=genie_ig_go_gs_restartread_regtest
DATUMDIR=genie_ig_go_gs_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

echo "IG_GO_GS_ML RESTARTS (R8-R8) :"
EXPDIR=genie_ig_go_gs_ml_restartread_regtest
DATUMDIR=genie_ig_go_gs_ml_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

echo "IG22_GO_SL RESTARTS (R8-R8) :"
EXPDIR=genie_ig_go_sl_restartread_l22_regtest
DATUMDIR=genie_ig_go_sl_restartmake_l22_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

echo "IG_GO72_GS72_ML RESTARTS (R8-R8) :"
EXPDIR=genie_ig_go72_gs72_ml_restartread_regtest
DATUMDIR=genie_ig_go72_gs72_ml_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

echo "ig_fi_fi_restarts:"
EXPDIR=genie_ig_fi_fi_restartread_regtest
DATUMDIR=genie_ig_fi_fi_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_da_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_da_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

echo "ig_sl_sl_restarts:"
EXPDIR=genie_ig_sl_sl_restartread_regtest
DATUMDIR=genie_ig_sl_sl_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_da_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_da_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi


echo "ig_fi_fi_ml_restarts:"
EXPDIR=genie_ig_fi_fi_ml_restartread_regtest
DATUMDIR=genie_ig_fi_fi_ml_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_da_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_da_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

echo "IG_GO6432_SL RESTARTS (R8-R8) :"
EXPDIR=genie_ig_go6432_sl_restartread_regtest
DATUMDIR=genie_ig_go6432_sl_restartmake_regtest
if $NCCOMPARE $OUTDIR/$DATUMDIR/igcm/igcm_cl_2000_02.nc \
    $OUTDIR/$EXPDIR/igcm/igcm_cl_2000_02.nc; then
    echo "**TEST OK**"
    cleanupboth
else
    echo "**TEST FAILED**"
fi

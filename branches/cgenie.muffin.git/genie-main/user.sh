# ---------------------------------------------------------------------
# PLEASE MAKE YOUR INSTALLATION IN THIS FILE
# ---------------------------------------------------------------------
#
# CODEDIR is the top-level code directory.  This is set to "~/genie" by
# defualt.  All GENIE modules sit below this, e.g. $CODEDIR/genie-main,
# $CODEDIR/genie-goldstein etc.
#
# OUTROOT is the path to where the output directories for the
# various experiments will be put; again defaults to a dir in your home
# directory: "~/genie_output"  NOTE that the GENIE executable (i.e. the
# experiment) is run from a directory under $OUTROOT,
# e.g. $OUTROOT/genie-test.
#
# RUNTIME_ROOT is a RELATIVE path from the output directory in which an
# experiment is run to $CODEDIR.  For example, if you have installed GENIE
# in ~/genie and your experiment is run from ~/genie_output/genie-test,
# then $RUNTIME_ROOT should be "../../genie"
#
# RUNTIME_OUTDIR should always be ".".  This is because experiments are
# run from the output directory. 
#
# ---------------------------------------------------------------------
#
# NOTE that all these variables may also be set on the command-line
# using switches to genie_example.job and further that options set on
# the command-line take precedence over changes made in this file.
#
# ---------------------------------------------------------------------

CODEDIR=~/cgenie.muffin
OUTROOT=~/cgenie_output
ARCHIVEDIR=~/cgenie_archive
LOGDIR=~/cgenie_log

RUNTIME_ROOT=${RUNTIME_ROOT:=../../cgenie.muffin}
RUNTIME_OUTDIR=${RUNTIME_OUTDIR:=.}


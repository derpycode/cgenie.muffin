#!/bin/bash -e
#
#####################################################################
### SCIPT TO COMPLETE INSTALLATION OF CGENIE.MUFFIN #################
#####################################################################
#
echo ""
#
# unpack lookup tables
echo ">> Unpacking lookup tables ..."
tar xzf ../genie-sedgem/data/input/lookup_calcite_4.tar.gz -C ../genie-sedgem/data/input/
tar xzf ../genie-sedgem/data/input/lookup_opal_5.tar.gz -C ../genie-sedgem/data/input/
# 
echo "   *** muffin installation complete ***"
echo ""
#

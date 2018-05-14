#!/bin/bash
#
#########################################
### SCIPT TO MAKE User_manual PS FILE ###
#########################################
#

latex cGENIE.User_manual.tex
dvips -o cGENIE.User_manual.ps cGENIE.User_manual.dvi
evince cGENIE.User_manual.ps &
rm cGENIE.User_manual.dvi
rm cGENIE.User_manual.out
rm cGENIE.User_manual.aux
rm cGENIE.User_manual.toc

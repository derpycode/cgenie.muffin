================================================================
=== readme.txt =================================================
================================================================

Provided are as part of the code release the configuration files necessary to run the key model experiments presented in the paper.
The intention is to provide an oppertunity to question the paper assumptions and interpretation through re-analysis,
as well as the creation of new and different experiments. (Plus, to provide a means to replicate published results.)
This readme file details how the experiments can be run.
Refer to the muffin manual:
https://github.com/derpycode/muffindoc
for details on model code installation and configuration, locating and visualizing model results, etc.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PUBLICATION DETAILS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

For: 'Instability in the geological regulation of Earth’s climate'

Dominik Hülse1,2, Andy Ridgwell1
1Department of Earth & Planetary Sciences, University of California, Riverside;
Riverside, CA 92521
2Max-Planck-Institute for Meteorology, Hamburg, Germany

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/06/20 -- README.txt file created by [AR]

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The individual experiments used in the main paper are:

ALL		-- all feedbacks enabled
SiONLY	-- silicate (and carbonate) weathering only feedback
lowCO2	-- as per ALL, but pre-industrial pCO2 plus modern major cation and anion concentions (and see SI)
hiDIC	-- as per ALL, but modern major cation and anion concentions (and see SI)

The results of these are shown in the following figures:

ALL		-- 2, 3
SiONLY	-- 2
lowCO2	-- 3
hiDIC	-- 3

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The experiment configuraiton files for the model ensemble shown in Figure 3 and provided in the ENSEMBLE folder.
The ensemble includes all feedbacks and follows the configuration of experiment ALL (differing only in intiial pO2 and PO4, and C and P fluxes).

The 10,000 PgC CO2 release experiments have a filename of the form:

231230.BASESw.EXPT.ENS.pO2PO4.5x5.yz

where yz is a 2-digit code corresponding to the specific combinations of values of pO2 and ocean [PO4]
and in which each digit starts at zero (0).

y corresponds to the initial pO2 value: x0.2, x0.4, x0.6, x0.8, x1.0 modern
z corresponds to the initial PO4 value: x0.4, x0.6, x0.8, x1.0, x1.2 modern

e.g. 
.00 ==  x0.2 pO2, x0.4 PO4
.01 ==  x0.2 pO2, x0.6 PO4
.34 ==  x0.8 pO2, x1.2 PO4

A single ensemble member is used for illustration in Figure 2.
This has x0.6 modern pO2 and x1.0 modern PO4, so it is experiment:

231230.BASESw.EXPT.ENS.pO2PO4.5x5.23

(remembering that each digit starts counting at 0).

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

All experiments consist of:

(1) A 20 kyr 'closed system' spin-up starting from 'cold'.
(2) A 50 kyr 'open system' spin-up starting from the end of the 20 kyr one.
(3) A 400 kyr long experiment with a 10,000 PgC CO2 release over the first 10 kyr, starting from the end of the 50 kyr spinup.
(4) A 400 kyr long control experiment with noC CO2 release, starting from the end of the 50 kyr spinup.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

For the individual experiments, the sequence of 4 parts for each is:

ALL
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231227.muffin.CBSR.fkl_pp51.BASESw.SPIN1 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231228.muffin.CBSR.fkl_pp51.BASESw.SPIN2 50000 231227.muffin.CBSR.fkl_pp51.BASESw.SPIN1
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231229.muffin.CBSR.fkl_pp51.BASESw.NONE.EXPT 400000 231228.muffin.CBSR.fkl_pp51.BASESw.SPIN2
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231229.muffin.CBSR.fkl_pp51.BASESw.NONE.CTRL 400000 231228.muffin.CBSR.fkl_pp51.BASESw.SPIN2

SiONLY
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231227.muffin.CBSR.fkl_pp51.BASESw.SPIN1 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231228.muffin.CBSR.fkl_pp51.BASESw.SPIN2 50000 231227.muffin.CBSR.fkl_pp51.BASESw.SPIN1
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 240111.muffin.CBSR.fkl_pp51.BASESw.ORG.EXPT 400000 231228.muffin.CBSR.fkl_pp51.BASESw.SPIN2
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 240111.muffin.CBSR.fkl_pp51.BASESw.ORG.CTRL 400000 231228.muffin.CBSR.fkl_pp51.BASESw.SPIN2

lowCO2
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231227.muffin.CBSR.fkl_pp51.BASESw.MODERN.SPIN1 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231228.muffin.CBSR.fkl_pp51.BASESw.MODERN.SPIN2 50000 231227.muffin.CBSR.fkl_pp51.BASESw.MODERN.SPIN1
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231229.muffin.CBSR.fkl_pp51.BASESw.MODERN.NONE.EXPT 400000 231228.muffin.CBSR.fkl_pp51.BASESw.MODERN.SPIN2
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231229.muffin.CBSR.fkl_pp51.BASESw.MODERN.NONE.CTRL 400000 231228.muffin.CBSR.fkl_pp51.BASESw.MODERN.SPIN2

hiDIC
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231227.muffin.CBSR.fkl_pp51.BASESw.HIGHDIC.SPIN1 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231228.muffin.CBSR.fkl_pp51.BASESw.HIGHDIC.SPIN2 50000 231227.muffin.CBSR.fkl_pp51.BASESw.HIGHDIC.SPIN1
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231229.muffin.CBSR.fkl_pp51.BASESw.NONE.EXPT 400000 231228.muffin.CBSR.fkl_pp51.BASESw.HIGHDIC.SPIN2
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024 231229.muffin.CBSR.fkl_pp51.BASESw.NONE.CTRL 400000 231228.muffin.CBSR.fkl_pp51.BASESw.HIGHDIC.SPIN2

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

For the ensemble, the sequence of 4 parts is:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 231228.BASESw.SPIN1.ENS.pO2PO4.5x5.yz 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 231229.BASESw.SPIN2.ENS.pO2PO4.5x5.yz 50000 231228.BASESw.SPIN1.ENS.pO2PO4.5x5.yz
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 231230.BASESw.EXPT.ENS.pO2PO4.5x5.yz 400000 231229.BASESw.SPIN2.ENS.pO2PO4.5x5.yz
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 240112.BASESw.CTRL.ENS.pO2PO4.5x5.yz 400000 231229.BASESw.SPIN2.ENS.pO2PO4.5x5.yz

where yz is a 2-digit code corresponding to the specific combinations of values of pO2 and ocean [PO4] as described above.

For the specific ensemble member shown in Figure 2, the sequence is:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 231228.BASESw.SPIN1.ENS.pO2PO4.5x5.23 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 231229.BASESw.SPIN2.ENS.pO2PO4.5x5.23 50000 231228.BASESw.SPIN1.ENS.pO2PO4.5x5.23
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 231230.BASESw.EXPT.ENS.pO2PO4.5x5.23 400000 231229.BASESw.SPIN2.ENS.pO2PO4.5x5.23
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2024/ENSEMBLE 240112.BASESw.CTRL.ENS.pO2PO4.5x5.23 400000 231229.BASESw.SPIN2.ENS.pO2PO4.5x5.23

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

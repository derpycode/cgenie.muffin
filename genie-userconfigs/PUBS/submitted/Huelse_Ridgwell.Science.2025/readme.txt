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

2025/03/23 -- README.txt file created by [AR]

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The individual experiments used in the main paper (and the figuresthat  they appear in) are:

[]				-- all feedbacks enabled											[figure 2 + 3]
fixedCorg		-- silicate (and carbonate) weathering only feedback				[figure 2 + 3]
CPhigh			-- all feedbacks enabled + high plankton Corg C/P					[figure 3]
CPlow			-- all feedbacks enabled + low plankton Corg C/P					[figure 3]
modernpCO2MgCa	-- all feedbacks enabled + pre-industrial pCO2 plus modern Mg, Ca	[figure 3]
modernMgCa		-- all feedbacks enabled + modern Mg, Ca							[figure 3]
seasonal		-- all feedbacks enabled + seasonal insolation forcing				[figure 3]

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The experiment configuraiton files for the model ensemble shown in Figure 3 and provided in the ENSEMBLE folder. The ensemble includes all feedbacks.

The 10,000 PgC CO2 release experiments have a filename of the form:

yymmdd.BASESw.EXPT.ENS.pO2PO4.5x5.yz

where yymmdd is a date code, and yz is a 2-digit code corresponding to the specific combinations of values of pO2 and ocean [PO4]
and in which each digit starts at zero (0).

y corresponds to the initial pO2 value: x0.2, x0.4, x0.6, x0.8, x1.0 modern
z corresponds to the initial PO4 value: x0.4, x0.6, x0.8, x1.0, x1.2 modern

e.g. 
.00 ==  x0.2 pO2, x0.4 PO4
.01 ==  x0.2 pO2, x0.6 PO4
.34 ==  x0.8 pO2, x1.2 PO4

A single ensemble member is used for illustration in Figure 2.
This has x0.6 modern pO2 and x1.0 modern PO4, so it is experiment:

240912.BASESw.EXPT.ENS.pO2PO4.5x5.23

(remembering that each digit starts counting at 0).

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

All experiments consist of:

(1) A 20 kyr 'closed system' spin-up starting from 'cold'.
(2) A 50 kyr 'open system' spin-up starting from the end of the 20 kyr one.
(3) A 500 kyr long experiment with a 10,000 PgC CO2 release over the first 10 kyr, starting from the end of the 50 kyr spinup.
(4) A 500 kyr long control experiment with noC CO2 release, starting from the end of the 50 kyr spinup.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

For the individual experiments, the sequence of 4 parts for each is:

ALL feedbacks:
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.SPIN1 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.SPIN2 50000 muffin.CBSR.fkl_pp51.BASESw.SPIN1
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.NONE.EXPT 500000 muffin.CBSR.fkl_pp51.BASESw.SPIN2
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.NONE.CTRL 500000 muffin.CBSR.fkl_pp51.BASESw.SPIN2

(and similarly for: CPhigh. CPlow, modernpCO2MgCa, modernMgCa, seasonal)

silicate weathering only uses the same SPIN1 and SPIN2 as for ALL feedbacks:
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.SPIN1 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.SPIN2 50000 muffin.CBSR.fkl_pp51.BASESw.SPIN1
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.fixedCorg.EXPT 500000 muffin.CBSR.fkl_pp51.BASESw.SPIN2
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025 muffin.CBSR.fkl_pp51.BASESw.fixedCorg.CTRL 500000 muffin.CBSR.fkl_pp51.BASESw.SPIN2

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

For the ensemble, the sequence of 4 parts is:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240910.BASESw.SPIN1.ENS.pO2PO4.5x5.yz 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240910.BASESw.SPIN2.ENS.pO2PO4.5x5.yz 50000 240910.BASESw.SPIN1.ENS.pO2PO4.5x5.yz
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240912.BASESw.EXPT.ENS.pO2PO4.5x5.yz 500000 240910.BASESw.SPIN2.ENS.pO2PO4.5x5.yz
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240912.BASESw.CTRL.ENS.pO2PO4.5x5.yz 500000 240910.BASESw.SPIN2.ENS.pO2PO4.5x5.yz

where yz is a 2-digit code corresponding to the specific combinations of values of pO2 and ocean [PO4] as described above.

For the specific ensemble member shown in Figure 2, the sequence is:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240910.BASESw.SPIN1.ENS.pO2PO4.5x5.23 20000
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240910.BASESw.SPIN2.ENS.pO2PO4.5x5.23 50000 240910.BASESw.SPIN1.ENS.pO2PO4.5x5.23
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240912.BASESw.EXPT.ENS.pO2PO4.5x5.23 500000 240910.BASESw.SPIN2.ENS.pO2PO4.5x5.23
./runmuffin.sh muffin.CBSR.fkl_pp51.BASESw /PUBS/submitted/Huelse_Ridgwell.Science.2025/ENSEMBLE 240912.BASESw.CTRL.ENS.pO2PO4.5x5.23 500000 240910.BASESw.SPIN2.ENS.pO2PO4.5x5.23

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

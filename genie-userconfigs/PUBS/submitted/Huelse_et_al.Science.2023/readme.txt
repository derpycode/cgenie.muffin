################################################################
### readme.txt #################################################
################################################################

For: 'Instability in the geological regulation of Earth’s climate'

Dominik Hülse1,2, Andy Ridgwell1
1Department of Earth & Planetary Sciences, University of California, Riverside;
Riverside, CA 92521
2Max-Planck-Institute for Meteorology, Hamburg, Germany

################################################################
2023/03/09 -- README.txt file creation (AR)
################################################################

Note that if the code is obtained from the DOI, the experiments can be run directly from the obtained code.
If the code repository is cloned, then it is necessary to switch branch. From cgenie.muffin:
$ git checkout _DEV_FeNIP2

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################ Series 1 (feedback combinations) ##############

A 2-stage spin-up is required before a perturbation (or contrl) can be run.
First a 20 kyr 'closed-system' (see: Ridgwell and Hargreaves [2007]) experiment:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023 muffin.CBSR.fkl_pp51.BASES.mud2.SPIN1 20000

and then a 2nd stage, 50kyr 'open system' experiment using weathering fluxes diagnosed form the closed system run:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023 muffin.CBSR.fkl_pp51.BASES.mud2.SPIN2 50000 muffin.CBSR.fkl_pp51.BASES.mud2.SPIN1

All the 10,000 PgC CO2 release perturbation expeirments are run form the same restart (muffin.CBSR.fkl_pp51.BASES.mud2.SPIN2).

These are run for 400 kyr with a command of the form:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023 muffin.CBSR.fkl_pp51.BASES.mud2.xxx.EXPT 400000 muffin.CBSR.fkl_pp51.BASES.mud2.SPIN2

where xxx is a code corresponding to the combination of feedbacks that is fixed. The full list of 14 different codes (see SI) is:

fixed Porg burial                                       :: xxx ==   fixedPb
fixed Porg weathering                                   :: xxx ==   fixedPw
fixed Porg weathering AND burial                        :: xxx ==   fixedPwPb
fixed Corg burial                                       :: xxx ==   fixedCb
fixed Corg weathering                                   :: xxx ==   fixedCw
fixed Corg weathering AND burial                        :: xxx ==   fixedCwCb
fixed Corg,Porg weathering AND burial (classic config)  :: xxx ==   fixedCwCbPwPb
fixed Porg burial Redfield Ratio                        :: xxx ==   fixedPbrr
fixed Porg weathering AND burial Redfield Ratio         :: xxx ==   fixedPwPbrr
fixed Corg,Porg weathering AND burial Redfield Ratio    :: xxx ==   fixedCwCbPwPbrr
fixed silicate weathering (only)                        :: xxx ==   fixedSIw
fixed carbonate weathering (only)                       :: xxx ==   fixedCARBw
fixed silicate AND carbonate weathering                 :: xxx ==   fixedSIwCARBw
NONE (all feedbacks duplicate as a 'control')           :: xxx ==   fixedNONE

e.g. for all feedbacks combined (none fixed), the perturbation experiment would be run:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023 muffin.CBSR.fkl_pp51.BASES.mud2.fixedNONE.EXPT 400000 muffin.CBSR.fkl_pp51.BASES.mud2.SPIN2

The corresponding control experiments take the same code, but with a user-config ending in 'CTRL', i.e.:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023 muffin.CBSR.fkl_pp51.BASES.mud2.xxx.CTRL 400000 muffin.CBSR.fkl_pp51.BASES.mud2.SPIN2

################ Series 2 (pO2 vs. PO4 parameter ensemble) #####

The ensemble experiments utilize an identical methodology of a 2-stage spinup, 
and with both 400 kyr perturbation and control experiments run on form the end of the 2nd stage spin-up experiment.

However, each ensemble member has a 2-digit code corresponding to the specific combinations of values of pO2 and ocean [PO4].
This is represented by .yz at the end of the filename.
y corresponds to the initial pO2 value: x0.4, x0.6, x0.4, x1.0 modern
z corresponds to the initial PO4 value: x0.6, x0.8, x1.0, x1.2 modern
e.g. 
.00 ==  x0.4 pO2, x0.6 PO4
.01 ==  x0.4 pO2, x0.8 PO4
.32 ==  x1.0 pO2, x1.0 PO4

The first stage spin-up for each ensemble member is:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023/ENSEMBLE 220526.ENS.pO2PO4.4x4a.yz 20000

The 2nd stage:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023/ENSEMBLE 220530.ENS.pO2PO4.4x4a.yz 50000 220526.ENS.pO2PO4.4x4a.yz 

The perturbation experiment:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023/ENSEMBLE 220701.ENS.pO2PO4.4x4a.yz 400000 220530.ENS.pO2PO4.4x4a.yz 

And the corresponding control:

./runmuffin.sh muffin.CBSR.fkl_pp51.BASES PUBS/submitted/Huelse_et_al.Science.2023/ENSEMBLE 220706.ENS.pO2PO4.4x4a.yz 400000 220530.ENS.pO2PO4.4x4a.yz 

################################################################
################################################################
################################################################
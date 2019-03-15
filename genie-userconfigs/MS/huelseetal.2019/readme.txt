################################################################
### readme.txt #################################################
################################################################

For: ‘Mitigation of extreme Ocean Anoxic Event conditions by organic matter sulfurization’ 
Dominik Hülse, Sandra Arndt and Andy Ridgwell

################################################################
19/02/2019 -- README.txt file creation (D.H.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################## Model Experiments -- Steady-state simulations ###########################

The commands to run (a selection of) the steady-state simulations as listed in the Methods. (For the simulations not listed below, PO4 and ksulf has to be changed 
appropriately in the userr-config.:

(1) 1.0xPO4, without sulfurization 

./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 SPIN1_huelseetal.p0093k_1.0P_4pCO2_ksulf_0 20000

(2) 1.0xPO4, with sulfurization, ksulf = E+5
./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 SPIN2_huelseetal.p0093k_1.0P_4pCO2_ksulf_E5 20000

(3) 2.0xPO4, without sulfurization 

./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 SPIN3_huelseetal.p0093k_2.0P_4pCO2_ksulf_0 20000

(4) 2.0xPO4, with sulfurization, ksulf = E+5
./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 SPIN4_huelseetal.p0093k_2.0P_4pCO2_ksulf_E5 20000

(5) 2.0xPO4, with sulfurization, ksulf = E+6
./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 SPIN5_huelseetal.p0093k_2.0P_4pCO2_ksulf_E6 20000

(6) 2.0xPO4, with sulfurization, ksulf = E+7
./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 SPIN6_huelseetal.p0093k_2.0P_4pCO2_ksulf_E7 20000


################## Model Experiments -- Transient simulations ##############

NB. named as per Table 2 

These are run on from the end of the steady-state simulation 2.0xPO4, without sulfurization:

(1) Experiment #1

./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 Exp1_huelseetal.p0093k_2.0P_4pCO2_ksulf_E5 30000 SPIN3_huelseetal.p0093k_2.0P_4pCO2_ksulf_0

(2) Experiment #2

./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 Exp2_huelseetal.p0093k_2.0P_4pCO2_ksulf_E5 30000 SPIN3_huelseetal.p0093k_2.0P_4pCO2_ksulf_0

(3) Experiment #3

./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 Exp3_huelseetal.p0093k_2.0P_4pCO2_ksulf_E5 30000 SPIN3_huelseetal.p0093k_2.0P_4pCO2_ksulf_0

(4) Experiment #4

./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 Exp4_huelseetal.p0093k_2.0P_4pCO2_ksulf_E5 30000 SPIN3_huelseetal.p0093k_2.0P_4pCO2_ksulf_0

(5) Experiment #5

./runmuffin.sh  cgenie.eb_go_gs_ac_bg_sg_rg.p0093k.BASESFePOM-S MS/huelseetal.2019 Exp5_ctrl_huelseetal.p0093k_2.0P_4pCO2_ksulf_0 30000 SPIN3_huelseetal.p0093k_2.0P_4pCO2_ksulf_0

################################################################
################################################################
################################################################


################################################################
### readme.txt #################################################
################################################################

For: 'Spatially contrasted response of Devonian anoxia to astronomical forcing '

Justin GERARD1, Alexandre POHL2, Loic SABLON1, Jarno J.C. HUYGH3, Anne-Christine DA SILVA3, Michel CRUCIFIX1

1Université catholique de Louvain (UCLouvain), Earth and Life Institute ELI), 1348 Louvain-la-Neuve, Belgium
2Biogéosciences, UMR 6282 CNRS, Université de Bourgogne, 21000 Dijon, France
3University of Liège, Department of Geology, Sart Tilman, 4000 Liège, Belgium

################################################################
16/12/2025 -- readme.txt file creation (J.G.)
16/12/2025 -- added files (J.G.)
18/12/2025 -- finalized (J.G.)
################################################################

Provided are the configuration files necessary to run:
	A] Open setup system at equilibrium
	B] Transient simulations 

####### A] Open setup system at equilibrium ####################

#
# SPIN 1: closed setup (similar to Hulse and Ridgwell 2025)
#
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN1 20000

#
# SPIN 2: open setup (similar to Hulse and Ridgwell 2025)
#
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_50k 50000 JG.S370Mgcm.main_eq.SPIN1
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_100k 50000 JG.S370Mgcm.main_eq.SPIN2_50k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_150k 50000 JG.S370Mgcm.main_eq.SPIN2_100k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_200k 50000 JG.S370Mgcm.main_eq.SPIN2_150k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_250k 50000 JG.S370Mgcm.main_eq.SPIN2_200k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_300k 50000 JG.S370Mgcm.main_eq.SPIN2_250k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_350k 50000 JG.S370Mgcm.main_eq.SPIN2_300k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_400k 50000 JG.S370Mgcm.main_eq.SPIN2_350k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_450k 50000 JG.S370Mgcm.main_eq.SPIN2_400k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN2_500k 50000 JG.S370Mgcm.main_eq.SPIN2_450k

#
# SPIN 3: the emulator is used
#
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN3_50k 50000 JG.S370Mgcm.main_eq.SPIN2_500k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025 JG.S370Mgcm.main_eq.SPIN3_100k 50000 JG.S370Mgcm.main_eq.SPIN3_50k

####### B] Transient simulations ###############################

#
# v1_exp_full: all 3 astronomical parameters are evolving (1100 simulations of 1000 years each)
#
REMARK: In order for these simulations to work out properly, the "biogem_force_flux_ocn_PO4_SUR.dat" file in the "genie-forcings/JG.v1_exp_full" folder has to be modified !!!
        All the forcing files are in "genie-forcings/JG.v1_exp_full/OLD_SUR_files.zip" and have the name "bffo_PO4_[xxxx]k" where [xxxx] gives the simulation year (e.g. 0001 or 1100). 
        For the first simulation, the "bffo_PO4_0001k" files must be copied to "biogem_force_flux_ocn_PO4_SUR.dat". Also, each user config file which has to be uncompressed 
        (from "compressed_v1_exp_full.tar.gz"). THIS HAS TO BE DONE FOR EACH SIMULATION !!!

./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_full JG.S370Mgcm.v1_exp_full.TRANS_0001k 1000 JG.S370Mgcm.main_eq.SPIN3_100k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_full JG.S370Mgcm.v1_exp_full.TRANS_0002k 1000 JG.S370Mgcm.v1_exp_full.TRANS_0001k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_full JG.S370Mgcm.v1_exp_full.TRANS_0003k 1000 JG.S370Mgcm.v1_exp_full.TRANS_0002k
...
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_full JG.S370Mgcm.v1_exp_full.TRANS_1099k 1000 JG.S370Mgcm.v1_exp_full.TRANS_1098k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_full JG.S370Mgcm.v1_exp_full.TRANS_1100k 1000 JG.S370Mgcm.v1_exp_full.TRANS_1099k

#
# v1_exp_obli: Only obliquity evolving (1100 simulations of 1000 years each)
#
REMARK: In order for these simulations to work out properly, the "biogem_force_flux_ocn_PO4_SUR.dat" file in the "genie-forcings/JG.v1_exp_obli" folder has to be modified !!!
        All the forcing files are in "genie-forcings/JG.v1_exp_obli/OLD_SUR_files.zip" and have the name "bffo_PO4_[xxxx]k" where [xxxx] gives the simulation year (e.g. 0001 or 1100).
        For the first simulation, the "bffo_PO4_0001k" files must be copied to "biogem_force_flux_ocn_PO4_SUR.dat". Also, each user config file which has to be uncompressed            
        (from "compressed_v1_exp_obli.tar.gz"). THIS HAS TO BE DONE FOR EACH SIMULATION !!!

./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_obli JG.S370Mgcm.v1_exp_obli.TRANS_0001k 1000 JG.S370Mgcm.main_eq.SPIN3_100k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_obli JG.S370Mgcm.v1_exp_obli.TRANS_0002k 1000 JG.S370Mgcm.v1_exp_obli.TRANS_0001k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_obli JG.S370Mgcm.v1_exp_obli.TRANS_0003k 1000 JG.S370Mgcm.v1_exp_obli.TRANS_0002k
...
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_obli JG.S370Mgcm.v1_exp_obli.TRANS_1099k 1000 JG.S370Mgcm.v1_exp_obli.TRANS_1098k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_obli JG.S370Mgcm.v1_exp_obli.TRANS_1100k 1000 JG.S370Mgcm.v1_exp_obli.TRANS_1099k

#
# v1_exp_ecc_prec: Only eccentricity and precession evolving (1100 simulations of 1000 years each)
#
REMARK: In order for these simulations to work out properly, the "biogem_force_flux_ocn_PO4_SUR.dat" file in the "genie-forcings/JG.v1_exp_ecc_prec" folder has to be modified !!!
        All the forcing files are in "genie-forcings/JG.v1_exp_ecc_prec/OLD_SUR_files.zip" and have the name "bffo_PO4_[xxxx]k" where [xxxx] gives the simulation year (e.g. 0001 or 1100).
        For the first simulation, the "bffo_PO4_0001k" files must be copied to "biogem_force_flux_ocn_PO4_SUR.dat". Also, each user config file which has to be uncompressed            
        (from "compressed_v1_exp_ecc_prec.tar.gz"). THIS HAS TO BE DONE FOR EACH SIMULATION !!!

./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_ecc_prec JG.S370Mgcm.v1_exp_ecc_prec.TRANS_0001k 1000 JG.S370Mgcm.main_eq.SPIN3_100k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_ecc_prec JG.S370Mgcm.v1_exp_ecc_prec.TRANS_0002k 1000 JG.S370Mgcm.v1_exp_ecc_prec.TRANS_0001k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_ecc_prec JG.S370Mgcm.v1_exp_ecc_prec.TRANS_0003k 1000 JG.S370Mgcm.v1_exp_ecc_prec.TRANS_0002k
...
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_ecc_prec JG.S370Mgcm.v1_exp_ecc_prec.TRANS_1099k 1000 JG.S370Mgcm.v1_exp_ecc_prec.TRANS_1098k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_ecc_prec JG.S370Mgcm.v1_exp_ecc_prec.TRANS_1100k 1000 JG.S370Mgcm.v1_exp_ecc_prec.TRANS_1099k

#
# v1_exp_CTRL: constant average astronomical configuration 
#
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_CTRL JG.S370Mgcm.v1_exp_CTRL.TRANS_0050k 50000 JG.S370Mgcm.main_eq.SPIN3_100k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_CTRL JG.S370Mgcm.v1_exp_CTRL.TRANS_0100k 50000 JG.S370Mgcm.v1_exp_CTRL.TRANS_0050k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_CTRL JG.S370Mgcm.v1_exp_CTRL.TRANS_0150k 50000 JG.S370Mgcm.v1_exp_CTRL.TRANS_0100k
...
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_CTRL JG.S370Mgcm.v1_exp_CTRL.TRANS_1050k 50000 JG.S370Mgcm.v1_exp_CTRL.TRANS_1000k
./runmuffin.sh muffin.JG.S370Mgcm.eb_go_gs_ac_bg_sg_rg PUBS/published/Gerard_et_al_2025/v1_exp_CTRL JG.S370Mgcm.v1_exp_CTRL.TRANS_1100k 50000 JG.S370Mgcm.v1_exp_CTRL.TRANS_1050k

################################################################
################################################################
################################################################

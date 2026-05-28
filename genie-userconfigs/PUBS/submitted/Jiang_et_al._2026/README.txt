#############################################
##########readme.txt###########################
#############################################

For: "Precession-dominated Orbital Control on North Pacific Deep Water Formation in Early Eocene Simulations"
Q. Jiang, M. Li, N. M. Papadomanolaki, Y. Liu and D. De Vleeschouwer

#############################################
03/02/2026 -- README.txt file creation
20/05/2026 -- update
#############################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################# Model Experiments ##############

The commands to run the model configurations as listed in the Methods are listed here.

(1) endmember experiments
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES genie-userconfigs/PUBS/submitted 3pal_pre_max 20000
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES genie-userconfigs/PUBS/submitted 3pal_pre_min 20000
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES genie-userconfigs/PUBS/submitted 3pal_obl_max 20000
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES genie-userconfigs/PUBS/submitted 3pal_obl_min 20000


(2) 100-member ensemble experiments
e.g., to run ensemble member QQ.petm01
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES genie-userconfigs/PUBS/submitted QQ.petm01.ID.1 20000

Note: QQ.petm01.ID.* is 100-member emsemble with state-dependent climate sensitivity
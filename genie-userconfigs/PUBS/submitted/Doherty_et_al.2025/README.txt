################################################################
### README.txt #################################################
################################################################

For:
'Quantifying net carbon cycle feedbacks across the Paleocene-Eocene Thermal Maximum'
D. Doherty, P. Vervoort, S. M. Jones, T. Dunkley Jones, D. E. Gaskell, A. Ridgwell & S. E. Greene

################################################################
10/11/2025 -- README.txt add spin-up details. Edit run commands (PV)
07/11/2025 -- README.txt file creation (DD)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an opportunity to question the assumptions and interpretation through 
re-analysis and the creation of new and different experiments. (Plus, to provide a means to replicate results.)

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

##### 1. Model experiments -- SPIN-UPS #####

	# 1.1. INITIAL SPINUP - closed system (see Ridgwell and Hargreaves [2007]):
	#                       following Gutjahr et al., 2017

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c.SPIN1 20000

	# 1.2 SECOND STAGE SPINUP - open system (see Ridgwell and Hargreaves [2007] and Lord et al. [2015]):
	#                           following Gutjahr et al., 2017

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c.SPIN2gl 200000 p0055c.SPIN1

##### 2. Model experiments -- MAIN ENSEMBLE ######

Below are the commands to run the experiments, following naming conventions in the manuscript:

(**_Low) --> Low NAIP emissions scenario
(**_Med) --> Median NAIP emissions scenario (focus of the main text)
(**_High) --> High NAIP emissions scenario

	# 1. For the NAIP Forcing Experiments

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_NAIP_Only_Low.config 150000 p0055c.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_NAIP_Only_Med.config 150000 p0055c.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_NAIP_Only_High.config 150000 p0055c.SPIN2gl

	# 2. For the Inversion Experiments

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_Inversion_Exp_Low.config 150000 p0055c.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_Inversion_Exp_Med.config 150000 p0055c.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_Inversion_Exp_High.config 150000 p0055c.SPIN2gl

	# 3. For the Burial Inversion Experiments

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_Burial_Inv_Exp_Low.config 150000 p0055c.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_Burial_Inv_Exp_Med.config 150000 p0055c.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASESr PUBS/submitted/Doherty_et_al.2025 p0055c_Burial_Inv_Exp_High.config 150000 p0055c.SPIN2gl

################################################################
################################################################
################################################################
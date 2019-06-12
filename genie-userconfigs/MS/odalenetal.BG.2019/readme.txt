################################################################
### readme.txt #################################################
################################################################

For:
'Variable C/P composition of organic production and its effect onocean carbon storage in glacial model simulations'
M. Ödalen,  J. Nycander, A. Ridgwell, K. I. C. Oliver, C. D. Peterson, andJ. Nilsson

################################################################
19/05/29 -- README.txt file creation (M.Ö.)
################################################################

Provided are the configuration files and forcing files necessary to run the model experiments presented in the paper.

The intention is to provide an opportunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### base configuration ##########################################

All spinups and experiments are run from the base config
malod.worjh2.pref.BASEFe

This is an adaptation of the general GENIE base config
cgenie.eb_go_gs_ac_bg.worjh2.BASEFe.config
to allow running with and outputting preformed tracers


### model versions ##############################################

In the paper, three model versions are compared

RED : The default, using Redfield fixed P:C stoichiometry
121 : Fixed stoichiometry, with P:C = 1:121
GAM : Flexible P:C stoichiometry as in Galbraith and Martiny, 2015

### model experiments -- spinups ################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the spinups are listed as follows:

./runmuffin.sh malod.worjh2.pref.BASEFe /MS/odalenetal.BG.2019 malod.worjh2.PO4Fe.pref.(VERSION).SPIN 10000

In this line, (VERSION) should be replaced by one of the model version abbreviations RED, GAM or 121.
The spinups use modern/pre-industrial forcings to achieve a modern/pre-industrial model state. The spinups are run for 10000 years, to allow equilibration of the carbon system.


### model experiments - main ensemble ############################

The model experiments of each model version are run as a restart from the respective version spinup, and are run for 10000 years to allow equilibration of the new model state.

./runmuffin.sh malod.worjh2.pref.BASEFe /MS/odalenetal.BG.2019 EXPERIMENT_CONFIG.(VERSION).EXP 10000 malod.worjh2.PO4Fe.pref.(VERSION).SPIN

Here, EXPERIMENT_CONFIG.(VERSION).EXP should be replaced by one of the experiments listed below.


### list of ensemble experiment configurations ###################

Experiment					Forcings

malod.LGMalb.PO4Fe.pref.(VERSION).EXP  		LGM albedo
malod.LGMrf.PO4Fe.pref.(VERSION).EXP		LGM radiative forcing (corresponding to pCO2atm = 185) 
malod.LGMphy.PO4Fe.pref.(VERSION).EXP		LGM albedo + radiative forcing
malod.WNSx05.PO4Fe.pref.(VERSION).EXP		Wind stress at +-50 °N reduced
malod.LGMdust.PO4Fe.pref.(VERSION).EXP		LGM dust flux
malod.RLSx0_75.PO4Fe.pref.(VERSION).EXP		Remineralisation length scale reduced by 25%
malod.RLSx1_25.PO4Fe.pref.(VERSION).EXP		Remineralisation length scale increased by 25%
malod.RLSx1_75.PO4Fe.pref.(VERSION).EXP		Remineralisation length scale increased by 75%
malod.Acomb.PO4Fe.pref.(VERSION).EXP		LGMrf + LGMalb + LGMdust + RLSx1.25
malod.GLcomb.PO4Fe.pref.(VERSION).EXP		LGMrf + LGMalb + LGMdust + RLSx1.25 + WNSx05



### Forcing files in addition to standard GENIE selection #########


* Albedo

malod_albs_LGM.dat			2D LGM albedo profile,
					regridded from GCM simulation (Davies-Barnard et al., 2017)


* Wind stress

GL_taux_u_NA050.interp			Wind stress components rescaled at +-50°N as described in manuscript
GL_tauy_u_NA050.interp
GL_taux_v_NA050.interp
GL_tauy_v_NA050.interp


* Iron/Dust

worjh2.FeMahowald2006_RpCO2_Rp13CO2 	Iron forcing from Mahowald et al. (2006), restores pCO2atm
					—> used in spinup
malod.worjh2.FeMahowald2006.no_rest	Iron forcing from Mahowald et al. (2006), no restoring of pCO2atm
					—> used in experiments with no change in dust forcing
malod.worjh2.FeMahowald2006_LGMdust	Iron forcing from Mahowald et al. (2006) but with LGM dust flux,
					no restoring of pCO2atm
					—> used in experiments with LGM dust forcing




 
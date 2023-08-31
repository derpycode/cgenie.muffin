################################################################
### readme.txt #################################################
################################################################

For: 'Diagnosing the MS/gerard.crucifix.2023 slowdown in a coupled model: a cautionary tale'

Justin GERARD1, Michel CRUCIFIX1

1Université catholique de Louvain (UCLouvain), Earth and Life Institute (ELI), Louvain-la-Neuve, Belgium 

################################################################
31/08/2023 -- readme.txt file creation (J.G.)
31/08/2023 -- added files
################################################################

Provided are the configuration files necessary to run:
	A] RCP8.5 simulation set
	B] Hysteresis simulation set

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################ A] RCP8.5 ###################

# Main initial condition (preindustrial climate)
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 ULTIME_config 30000

# RCP8.5 runs
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 RCP8.5_year_2000 235 ULTIME_config
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 RCP8.5_1000yr_from_2000 1000 RCP8.5_year_2000

################ B] Hysteresis ###############

# CO2 hysteresis
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 bifurcation_CO2 40000 ULTIME_config
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 bifurcation_CO2_verif 40000 ULTIME_config

# Freshwater hysteresis
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 freshwater_flux_0.2Sv 10000 ULTIME_config
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 freshwater_flux_0.2Sv_sal_cst 10000 ULTIME_config
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 bifurcation_freshwater_flux_0.0Sv 20000 ULTIME_config
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 bifurcation_freshwater_flux_0.0Sv_sal_cst 20000 ULTIME_config
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 bifurcation_freshwater_flux_0.2Sv 20000 freshwater_flux_0.2Sv
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 bifurcation_freshwater_flux_0.2Sv_sal_cst 20000 freshwater_flux_0.2Sv_sal_cst
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/gerard.crucifix.2023 bifurcation_freshwater_flux_0.2Sv_sal_cst_verif 20000 freshwater_flux_0.2Sv_sal_cst

################################################################
################################################################
################################################################

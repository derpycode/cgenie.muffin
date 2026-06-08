################################################################
### readme.txt #################################################
################################################################

For: 'Exploring the mechanisms of Devonian oceanic anoxia: impact of ocean dynamics, palaeogeography and orbital forcing'

Justin GERARD1, Lo�c SABLON1, Jarno J.C. HUYGH2, Anne-Christine DA SILVA2, Alexandre POHL3, Christian VERARD4, Michel CRUCIFIX1

1Université catholique de Louvain (UCLouvain), Earth and Life Institute ELI), 1348 Louvain-la-Neuve, Belgium
2University of Liège, Department of Geology, Sart Tilman, 4000 Liège, Belgium
3Biogéosciences, UMR 6282 CNRS, Université de Bourgogne, 21000 Dijon, France
4Department of Earth Sciences, University of Geneva, 13 Rue des Maraîchers, 1205 Geneva, Switzerland

################################################################
06/08/2024 -- readme.txt file creation (J.G.)
12/08/2024 -- added files (J.G.)
19/11/2024 -- finalized (J.G.)
################################################################

Provided are the configuration files necessary to run:
        A] Set 1: Continental configuration
        B] Set 2: Geochemical state
        C] Set 3: Astronomical forcing

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

####### A] Set 1: Continental configuration ####################

# All Devonian continental configuration from Scotese and Wright (2018)
./runmuffin.sh muffin.JG.Scot360M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot360M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot365M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot365M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot370M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot375M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot375M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot380M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot380M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot385M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot385M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot390M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot395M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot395M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot400M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot400M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot405M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot405M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot410M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot410M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot415M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot415M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Scot420M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot420M.main_config.SPIN 10000

# All Devonian continental configuration from Verard (2019)
./runmuffin.sh muffin.JG.Vera370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera_main_config JG.Vera370M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Vera383M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera_main_config JG.Vera383M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera_main_config JG.Vera393M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Vera408M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera_main_config JG.Vera408M.main_config.SPIN 10000
./runmuffin.sh muffin.JG.Vera420M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera_main_config JG.Vera420M.main_config.SPIN 10000

# All Devonian continental configuration from Scotese with the solar constant from 390Ma
./runmuffin.sh muffin.JG.Scot360M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot360M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot365M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot365M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot370M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot375M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot375M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot380M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot380M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot385M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot385M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot390M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot395M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot395M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot400M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot400M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot405M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot405M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot410M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot410M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot415M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot415M.main_config.sol_cst.SPIN 10000
./runmuffin.sh muffin.JG.Scot420M.eb_go_gs_ac_bg.sol_cst PUBS/published/Gerard_et_al_2024/Scot_main_config JG.Scot420M.main_config.sol_cst.SPIN 10000

# Variation of the Vera393M continental configuration to alter a seaway
./runmuffin.sh muffin.JG.Vera3932.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera_main_config JG.Vera393_2M.main_config.SPIN 10000

####### B] Set 2: Geochemical state ############################

### Geochemical state with different continental configurations

# Parameter naming convention 
[pCO2] : atmospheric partial pressure of CO2
01      280 ppm
02      560 ppm
04      840 ppm
08      1120 ppm
16      1400 ppm

[PO4] : global mean oceanic phosphate concentration
1.00    2.159E-06 mol/kg
1.25    2.69875E-06 mol/kg
1.50    3.2385E-06 mol/kg
1.75    3.77825E-06 mol/kg
2.00    4.318E-06 mol/kg

[pO2] : atmospheric partial pressure of O2
0.4     0.0838
0.7     0.14665
1.0     0.2095

./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M JG.Scot370M.[pCO2]X_[PO4]PO4_[pO2]O2.SPIN 10000
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M JG.Scot390M.[pCO2]X_[PO4]PO4_[pO2]O2.SPIN 10000
./runmuffin.sh muffin.JG.Scot420M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot420M JG.Scot420M.[pCO2]X_[PO4]PO4_[pO2]O2.SPIN 10000
./runmuffin.sh muffin.JG.Vera370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera370M JG.Vera370M.[pCO2]X_[PO4]PO4_[pO2]O2.SPIN 10000
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M JG.Vera393M.[pCO2]X_[PO4]PO4_[pO2]O2.SPIN 10000
./runmuffin.sh muffin.JG.Vera420M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera420M JG.Vera420M.[pCO2]X_[PO4]PO4_[pO2]O2.SPIN 10000

### Hysteresis experiment

# Ramp up
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.01_to_02X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.01X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.02_to_03X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.01_to_02X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.03_to_04X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.02_to_03X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_05X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.03_to_04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.05_to_06X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.04_to_05X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.06_to_07X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.05_to_06X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.07_to_08X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.06_to_07X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.08_to_09X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.07_to_08X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.09_to_10X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.08_to_09X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.10_to_11X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.09_to_10X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.11_to_12X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.10_to_11X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.12_to_13X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.11_to_12X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.13_to_14X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.12_to_13X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.14_to_15X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.13_to_14X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.15_to_16X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.14_to_15X_1.25PO4_0.7O2.SPIN

# Ramp down
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.16_to_15X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.15_to_16X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.15_to_14X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.16_to_15X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.14_to_13X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.15_to_14X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.13_to_12X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.14_to_13X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.12_to_11X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.13_to_12X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.11_to_10X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.12_to_11X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.10_to_09X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.11_to_10X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.09_to_08X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.10_to_09X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.08_to_07X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.09_to_08X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.07_to_06X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.08_to_07X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.06_to_05X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.07_to_06X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.05_to_04X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.06_to_05X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_03X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.05_to_04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.03_to_02X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.04_to_03X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.02_to_01X_1.25PO4_0.7O2.SPIN 5000 JG.Scot390M.03_to_02X_1.25PO4_0.7O2.SPIN

# Stable states
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.01_to_01X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.01X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.02_to_02X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.01_to_02X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.03_to_03X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.02_to_03X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_04X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.03_to_04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.05_to_05X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.04_to_05X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.06_to_06X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.05_to_06X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.07_to_07X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.06_to_07X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.08_to_08X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.07_to_08X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.09_to_09X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.08_to_09X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.10_to_10X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.09_to_10X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.11_to_11X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.10_to_11X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.12_to_12X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.11_to_12X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.13_to_13X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.12_to_13X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.14_to_14X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.13_to_14X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.15_to_15X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.14_to_15X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.16_to_16X_1.25PO4_0.7O2_go.SPIN 10000 JG.Scot390M.15_to_16X_1.25PO4_0.7O2.SPIN

./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.15_to_15X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.16_to_15X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.14_to_14X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.15_to_14X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.13_to_13X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.14_to_13X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.12_to_12X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.13_to_12X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.11_to_11X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.12_to_11X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.10_to_10X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.11_to_10X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.09_to_09X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.10_to_09X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.08_to_08X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.09_to_08X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.07_to_07X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.08_to_07X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.06_to_06X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.07_to_06X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.05_to_05X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.06_to_05X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_04X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.05_to_04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.03_to_03X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.04_to_03X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.02_to_02X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.03_to_02X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.01_to_01X_1.25PO4_0.7O2_back.SPIN 10000 JG.Scot390M.02_to_01X_1.25PO4_0.7O2.SPIN

# 'JG.Scot390M.16_to_16X_1.25PO4_0.7O2_back.SPIN' doesn't exists as it corresponds exactly to 'JG.Scot390M.16_to_16X_1.25PO4_0.7O2_go.SPIN' in this experimental design.

# Additional simulations to explore the boudaries of the oscillation
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.1.8X_1.25PO4_0.7O2.SPIN 20000
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.1.9X_1.25PO4_0.7O2.SPIN 20000
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.2.0X_1.25PO4_0.7O2.SPIN 20000
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.2.1X_1.25PO4_0.7O2.SPIN 20000
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.2.2X_1.25PO4_0.7O2.SPIN 20000

./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_1.8X_1.25PO4_0.7O2.SPIN 20000 JG.Scot390M.04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_1.9X_1.25PO4_0.7O2.SPIN 20000 JG.Scot390M.04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_2.0X_1.25PO4_0.7O2.SPIN 20000 JG.Scot390M.04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_2.1X_1.25PO4_0.7O2.SPIN 20000 JG.Scot390M.04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_HYST JG.Scot390M.04_to_2.2X_1.25PO4_0.7O2.SPIN 20000 JG.Scot390M.04X_1.25PO4_0.7O2.SPIN

####### C] Set 3: Astronomical forcing #########################

### Scot370M and Vera393M continental configurations
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0_21_0.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_21_0.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_21_90.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_21_180.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_21_270.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0_25_0.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_25_0.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_25_90.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_25_180.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Scot370M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot370M_AF JG.Scot370M.02X_1.25PO4_0.7O2.0.06_25_270.SPIN 10000 JG.Scot370M.02X_1.25PO4_0.4O2.SPIN

./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0_21_0.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_21_0.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_21_90.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_21_180.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_21_270.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0_25_0.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_25_0.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_25_90.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_25_180.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN
./runmuffin.sh muffin.JG.Vera393M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Vera393M_AF JG.Vera393M.02X_1.25PO4_0.7O2.0.06_25_270.SPIN 10000 JG.Vera393M.02X_1.25PO4_0.4O2.SPIN

### Scot390M continental configuration

# Parameter naming convention
[ecc] : eccentricity 
0 (for this value of eccentricity, the precession is systematically put to 0)
0.03
0.06

[obli] : obliquity
21      
22
23
24
25

[prec] : precession
0
45
90
135
180
225
270
315

./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_AF JG.Scot390M.04_to_2.0X_1.25PO4_0.7O2.SPIN 20000 JG.Scot390M.04X_1.25PO4_0.7O2.SPIN
./runmuffin.sh muffin.JG.Scot390M.eb_go_gs_ac_bg PUBS/published/Gerard_et_al_2024/Scot390M_AF JG.Scot390M.02X_1.25PO4_0.7O2.[ecc]_[obli]_[prec].SPIN 10000 JG.Scot390M.04_to_2.0X_1.25PO4_0.7O2.SPIN

################################################################
################################################################
################################################################

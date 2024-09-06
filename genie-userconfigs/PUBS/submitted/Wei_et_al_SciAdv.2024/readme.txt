================================================================
=== readme.txt =================================================
================================================================

Provided are as part of the code release the configuration files necessary to run the key model experiments presented in the paper.
The intention is to provide an opportunity to question the paper assumptions and interpretation through re-analysis,
as well as the creation of new and different experiments. (Plus, to provide a means to replicate published results.)
This readme file details how the experiments can be run.
Refer to the muffin manual:
https://github.com/derpycode/muffindoc
for details on model code installation and configuration, locating and visualizing model results, etc.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PUBLICATION DETAILS [summary of manuscript/publication]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Depressed response of chemical weathering to global warming inhibited global
marine deoxygenation during the Paleocene-Eocene Thermal Maximum

Guang-Yi Wei1, Alexandre Pohl2 [...]  Shu-Zhong Shen1, Feifei
Zhang1*

1 State Key Laboratory for Mineral Deposits Research, School of Earth Sciences
and Engineering, and Frontiers Science Center for Critical Earth Material
Cycling, Nanjing University, 163 Xianlin Avenue, Nanjing 210023, China
2 Biogéosciences, UMR 6282 CNRS, Université de Bourgogne, 6 Boulevard Gabriel,
21000 Dijon, France
* Corresponding author: Feifei Zhang (fzhang@nju.edu.cn)


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/09/06 -- readme.txt file created by AP

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Experiments used in Figs. 3, S6 and S7

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#################### Experiments for Fig. 3 ####################

# pre-PETM
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.Fehybrid.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4Fe.x1.2pO2.x1.0PO4.834ppm.Tdep.radfor.SPIN 20000

# peak PETM with respectively, +30 % [PO4], + 0 % [PO4] and –20 % [PO4]
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.Fehybrid.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4Fe.x1.2pO2.x1.3PO4.2176ppm.Tdep.radfor.SPIN 20000
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.Fehybrid.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4Fe.x1.2pO2.x1.0PO4.2176ppm.Tdep.radfor.SPIN 20000
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.Fehybrid.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4Fe.x1.2pO2.x0.8PO4.2176ppm.Tdep.radfor.SPIN 20000

#################### Experiments for Fig. S6 ####################

# pre-PETM
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.Fehybrid.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4Fe.x1.2pO2.x1.0PO4.834ppm.radfor.SPIN 20000

# peak PETM with -20 % [PO4]
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.Fehybrid.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4Fe.x1.2pO2.x1.0PO4.2176ppm.radfor.SPIN 20000

#################### Experiments for Fig. S7 ####################

# pre-PETM
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4.x1.2pO2.x1.0PO4.834ppm.Tdep.radfor.SPIN 20000

# peak PETM with -20 % [PO4]
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/submitted/Wei_et_al_SciAdv.2024 AP.p0055c.PO4.x1.2pO2.x0.8PO4.2176ppm.Tdep.radfor.SPIN 20000
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

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
PUBLICATION DETAILS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Enhanced marine biological carbon pump as a trigger for the onset of the late Paleozoic ice age

Feifei Zhang, Alexandre Pohl, Maya Elrick, Guang-yi Wei, Keyi Cheng, Peter Crockford, Mojtaba Fakhraee, Yi-bo Lin, Mengchun Cao, Na Li, Gaojun Li, Xiangdong Wang, Shu-zhong Shen

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2023/09/12 -- README.txt file created by AP

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Ensemble of experiments shown in Figs. 4 and 5

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PUBS/submitted/Zhang_et_al.PNAS.2023

./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.2PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.4PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.6PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.8PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.0PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.2PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.4PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.6PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.8PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x2.0PO4.280ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.2PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.4PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.6PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.8PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.0PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.2PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.4PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.6PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.8PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x2.0PO4.560ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.2PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.4PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.6PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.8PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.0PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.2PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.4PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.6PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.8PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x2.0PO4.1120ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.2PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.4PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.6PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x0.8PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.0PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.2PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.4PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.6PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x1.8PO4.2240ppm.SPIN 25000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN PUBS/submitted/Zhang_et_al.PNAS.2023 AP.360ebP2_.PO4.pO2PD.x2.0PO4.2240ppm.SPIN 25000

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

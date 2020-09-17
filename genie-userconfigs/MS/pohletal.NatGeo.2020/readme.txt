################################################################
### readme.txt #################################################
################################################################

For: 'Reorganization of ocean circulation and oxygenation during Late Ordovician glaciation'

Alexandre POHL1,2*, Zunli LU3*, Wanyi LU3, Richard G. STOCKEY4, Maya ELRICK5, Menghan LI6, André DESROCHERS7, Yanan SHEN6, Ruliang HE3, Seth FINNEGAN8, Andy RIDGWELL1

1Department of Earth Sciences, University of California, Riverside, CA, USA
2Biogéosciences, UMR 6282, UBFC/CNRS, Université Bourgogne Franche-Comté, 6 boulevard Gabriel, F-21000 Dijon, France
3Department of Earth Sciences, Syracuse University, Syracuse, NY, USA
4Department of Geological Sciences, Stanford University, Stanford, CA 94305, USA
5Earth and Planetary Sciences, University of New Mexico, Albuquerque, NM 87131
6School of Earth and Space Sciences, University of Science and Technology of China, Hefei 230026, China
7Earth and Environmental Sciences, University of Ottawa, Ottawa ON K1N 6N5, Canada
8Department of Integrative Biology, University of California, Berkeley, Berkeley, CA, USA
*Corresponding authors

################################################################
23/06/2020 -- README.txt file creation (A.P.)
23/06/2020 -- added files
################################################################

Provided are the configuration files necessary to run the ensemble at 0.4 PAL O2 shown in Fig. 2b

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################ model ensemble at 0.4 PAL O2 ################

# Experiments at 5 PAL CO2
./runmuffin.sh muffin.AP.445eb05X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb5X.PO4.0.4O2.TdepJohn2014.0.2PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb05X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb5X.PO4.0.4O2.TdepJohn2014.0.4PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb05X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb5X.PO4.0.4O2.TdepJohn2014.0.6PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb05X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb5X.PO4.0.4O2.TdepJohn2014.0.8PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb05X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb5X.PO4.0.4O2.TdepJohn2014.1.0PO4.SPIN 10000
# Experiments at 6 PAL CO2
./runmuffin.sh muffin.AP.445eb06X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb6X.PO4.0.4O2.TdepJohn2014.0.2PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb06X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb6X.PO4.0.4O2.TdepJohn2014.0.4PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb06X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb6X.PO4.0.4O2.TdepJohn2014.0.6PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb06X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb6X.PO4.0.4O2.TdepJohn2014.0.8PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb06X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb6X.PO4.0.4O2.TdepJohn2014.1.0PO4.SPIN 10000
# Experiments at 7 PAL CO2
./runmuffin.sh muffin.AP.445eb07X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb7X.PO4.0.4O2.TdepJohn2014.0.2PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb07X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb7X.PO4.0.4O2.TdepJohn2014.0.4PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb07X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb7X.PO4.0.4O2.TdepJohn2014.0.6PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb07X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb7X.PO4.0.4O2.TdepJohn2014.0.8PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb07X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb7X.PO4.0.4O2.TdepJohn2014.1.0PO4.SPIN 10000
# Experiments at 8.5 PAL CO2
./runmuffin.sh muffin.AP.445eb8.5X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb8.5X.PO4.0.4O2.TdepJohn2014.0.2PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb8.5X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb8.5X.PO4.0.4O2.TdepJohn2014.0.4PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb8.5X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb8.5X.PO4.0.4O2.TdepJohn2014.0.6PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb8.5X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb8.5X.PO4.0.4O2.TdepJohn2014.0.8PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb8.5X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb8.5X.PO4.0.4O2.TdepJohn2014.1.0PO4.SPIN 10000
# Experiments at 10 PAL CO2
./runmuffin.sh muffin.AP.445eb10X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb10X.PO4.0.4O2.TdepJohn2014.0.2PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb10X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb10X.PO4.0.4O2.TdepJohn2014.0.4PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb10X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb10X.PO4.0.4O2.TdepJohn2014.0.6PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb10X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb10X.PO4.0.4O2.TdepJohn2014.0.8PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb10X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb10X.PO4.0.4O2.TdepJohn2014.1.0PO4.SPIN 10000
# Experiments at 24 PAL CO2
./runmuffin.sh muffin.AP.445eb24X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb24X.PO4.0.4O2.TdepJohn2014.0.2PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb24X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb24X.PO4.0.4O2.TdepJohn2014.0.4PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb24X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb24X.PO4.0.4O2.TdepJohn2014.0.6PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb24X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb24X.PO4.0.4O2.TdepJohn2014.0.8PO4.SPIN 10000
./runmuffin.sh muffin.AP.445eb24X.eb_go_gs_ac_bg.PO4.SPIN MS/pohletal.NatGeo.2020 AP.445eb24X.PO4.0.4O2.TdepJohn2014.1.0PO4.SPIN 10000

################################################################
################################################################
################################################################

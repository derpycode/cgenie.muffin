################################################################
### readme.txt #################################################
################################################################

For: 'Future Pacific deoxygenation predicted by microfossil records'

Authors
Martin Tetard1,2,3†, Yusuke Okazaki3†, Alexandre Pohl4†, Andy Ridgwell5†, Ekaterina Ovsepyan6†, Luc Beaufort1†

Affiliations
1 Aix Marseille Univ, CNRS, IRD, Coll France, INRAE, CEREGE, Aix en Provence, France.
2 GNS Science, Lower Hutt, New Zealand.
3 Department of Earth and Planetary Sciences, Graduate School of Science, Kyushu University, Fukuoka, Japan. 
4 Biogeosciences, UMR 6282 CNRS, Universite de Bourgogne, 6 boulevard Gabriel, 21000 Dijon, France.
5 Department of Earth and Planetary Sciences, University of California, Riverside, CA, USA.
6 Shirshov Institute of Oceanology, Russian Academy of Sciences, Moscow, Russia.

################################################################
30/05/2023 -- README.txt file creation (A.P.)
30/05/2023 -- added files
2023/05/31 -- adjusted file paths; checked experiments run (AR)
################################################################

Provided are the configuration files necessary to run the main experiments used in Fig. 4 and Extended Data Fig. 1, plus WOA [O2] data regridded to the cGENIE resolution and used in Fig. 4a.

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

############### WOA data  ################
worhj2.WOA13_O2_molkg-1.151207.nc (in this directory)

############### Preindustrial cGENIE simulation ################
(Rae et al.'s spinup + modern pO2 forcing)
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx PUBS/submitted/Tetard_et_al.Nature.2023 SPIN.worjh2.Fe14C.preAge.Dye.pO2 20000
[plotted at year 19999.5]

############### non-tuned LGM cGENIE simulation ################
(Rae et al.'s spinup + modern pO2 forcing)
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx PUBS/submitted/Tetard_et_al.Nature.2023 SPIN.worjh2.Fe14C.preAge.Dye.LGM.pO2 20000
[plotted at year 19999.5]

############### tuned LGM cGENIE simulation ################
(Rae et al.'s simulation + modern pO2 forcing)
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx PUBS/submitted/Tetard_et_al.Nature.2023 worjh2.Fe14C.preAge.Dye.LGM.PA15.pO2 5000 SPIN.worjh2.Fe14C.preAge.Dye.LGM.pO2
[plotted at year 4999.5]

############### D-O cGENIE simulation ################
/runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx PUBS/submitted/Tetard_et_al.Nature.2023 worjh2.Fe14C.preAge.Dye.LGM.PA15.pO2.sc1.pCO2x1.A0.5 2000 worjh2.Fe14C.preAge.Dye.LGM.PA15.pO2
[plotted at year 1449.5]

############### Heinrich cGENIE simulation ################
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFePRE14Crbcolx PUBS/submitted/Tetard_et_al.Nature.2023 worjh2.Fe14C.preAge.Dye.LGM.PA15.pO2.sc2.pCO2x1.A-0.5 2000 worjh2.Fe14C.preAge.Dye.LGM.PA15.pO2
[plotted at year 1199.5]

################################################################
################################################################
################################################################

################################################################
### readme.txt #################################################
################################################################

For: 'Spatiotemporal analysis of local redox proxy data: what do we really know about Phanerozoic oceanic oxygen evolution?'

Authors: Ruliang He1*, Alexandre Pohl2, Zunli Lu3, Rosalind E. M. Rickaby4, Richard G. Stockey5, Chao Chang1, Xingliang Zhang1

1 State Key Laboratory of Continental Evolution and Early Life, Shaanxi Key Laboratory of Early Life and Environments, Department of Geology, Northwest University, Xi’an, China.
2 Université Bourgogne Europe, CNRS, Biogéosciences UMR 6282, 21 000 Dijon, France.
3 Department of Earth and Environmental Sciences, Syracuse University, Syracuse, NY, USA.
4 Department of Earth Sciences, University of Oxford, Oxford OX1 3AN, UK. 
5 School of Ocean and Earth Science, National Oceanography Centre Southampton, University of Southampton, Southampton, UK.

Email: rulianghe@nwu.edu.cn 


################################################################
10/09/2026 -- README.txt file creation (A.P.)
10/09/2026 -- added files
################################################################

Provided are the configuration files necessary to run all simulations of:
A] series #1 ('best-guess' climatic trend, modern atmospheric pO2 > showing role of continental configuration and long-term climate)
B] series #2 ('constant' climate, modern atmospheric pO2 > showing role of continental configuration)
C] series #3 ('best-guess' climatic trend, carbon-cycle-model-derived atmospheric pO2 > showing role of continental configuration, long-term climate and changing atmospheric pO2)

ALl simulations include the iodine cycle.

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)


# =========== (A) series #1  =========== #

./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.540ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.520ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.500ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.480ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.460ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.440ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.420ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.400ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.380ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.360ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.340ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.320ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.300ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.280ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.260ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.240ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.220ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.200ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.180ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.160ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.140ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.120ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.100ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.80_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.60_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.40_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.20_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.0__ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2PD_radfor.SPIN 20000

# =========== (B) series #2  =========== #

./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.540ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.520ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.500ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.480ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.460ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.440ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.420ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.400ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.380ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.360ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.340ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.320ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.300ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.280ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.260ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.240ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.220ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.200ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.180ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.160ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.140ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.120ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.100ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.80_ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.60_ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.40_ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.20_ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.0__ebP2_.fenThresWOA.pCO2FKr_detreq.PO4PD.pO2PD_radfor.SPIN 20000

# =========== (C) series #3  =========== #

./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.540ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.520ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.500ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.480ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.460ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.440ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.420ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.400ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.380ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.360ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.340ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.320ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.300ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.280ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.260ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.240ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.220ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.200ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.180ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.160ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.140ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.120ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.100ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.80_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.60_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.40_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.20_ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN.I PUBS/submitted/He_et_al.2027 AP.0__ebP2_.fenThresWOA.pCO2FKr.PO4PD.pO2Kr_radfor.SPIN 20000

################################################################
################################################################
################################################################

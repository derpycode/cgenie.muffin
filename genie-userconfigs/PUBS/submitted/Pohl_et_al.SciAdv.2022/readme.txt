################################################################
### readme.txt #################################################
################################################################

For: 'The early Paleozoic was intrinsically prone to metazoan extinction'

Authors
Alexandre Pohl1*, Richard G. Stockey2,3, Xu Dai1, Ryan Yohler4, G. Le Hir5, D. Hülse6, Arnaud Brayard1, Seth Finnegan4, Andy Ridgwell7.

Affiliations 
1Biogéosciences, UMR 6282 CNRS, Université Bourgogne Franche-Comté, 6 boulevard Gabriel, 21000 Dijon, France
2Department of Geological Sciences, Stanford University, Stanford, CA 94305, USA.
3School of Ocean and Earth Science, National Oceanography Centre Southampton, University of Southampton, Southampton, UK
4Department of Integrative Biology, University of California, Berkeley, Berkeley, CA, USA.
5Université de Paris, Institut de Physique du Globe de Paris, CNRS, 1 rue Jussieu, 75005 Paris, France.
6Max-Planck-Institute for Meteorology, Hamburg, Germany.
7Department of Earth and Planetary Sciences, University of California, Riverside, CA, USA.
* Corresponding author. Email: alexandre.pohl@u-bourgogne.fr

################################################################
14/10/2022 -- README.txt file creation (A.P.)
14/10/2022 -- added files
################################################################

Provided are the configuration files necessary to run the main experiments used in Fig. 1.

For each series of experiments/panel of Fig. 1, 2 series of simulations are run (cold and warm), each one consisting in 28 experiments/time slices.

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

# =========== (A) 'Baseline' experiments of Fig. 1C =========== #

# In these experiments, pCO2 is varied (actually we use an equivalent radiative forcing), ocean [PO4] is Modern, atmospheric pO2 is Modern

[cold]
./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.540ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.520ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.500ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.480ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.460ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.440ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.420ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.400ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.380ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.360ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.340ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.320ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.300ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.280ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.260ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.240ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.220ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.200ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.180ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.160ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.140ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.120ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.100ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.80_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.60_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.40_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.20_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.0__ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2PD_radfor.SPIN 8000

[warm]
./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.540ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.520ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.500ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.480ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.460ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.440ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.420ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.400ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.380ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.360ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.340ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.320ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.300ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.280ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.260ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.240ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.220ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.200ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.180ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.160ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.140ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.120ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.100ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.80_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.60_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.40_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.20_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.0__ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2PD_radfor.SPIN 8000

# =========== (B) 'Detrended' experiments of Fig. 1D =========== #

# Same as (A) but pCO2 is varied in order to detrend low-latitude SSTs.

[cold]
./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.540ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.520ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.500ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.480ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.460ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.440ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.420ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.400ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.380ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.360ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.340ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.320ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.300ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.280ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.260ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.240ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.220ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.200ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.180ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.160ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.140ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.120ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.100ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.80_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.60_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.40_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.20_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.0__ebP2_.bg.PO4.Tdep.pCO2FKr_detreqC.PO4PD.pO2PD_radfor.SPIN 8000

[warm]
./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.540ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.520ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.500ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.480ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.460ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.440ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.420ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.400ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.380ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.360ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.340ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.320ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.300ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.280ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.260ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.240ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.220ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.200ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.180ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.160ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.140ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.120ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.100ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.80_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.60_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.40_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.20_ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.0__ebP2_.bg.PO4.Tdep.pCO2FKr_detreqW.PO4PD.pO2PD_radfor.SPIN 8000

# =========== (C) 'pO2' experiments of Fig. 1E =========== #

# Same as (A) using a time-varying pO2 

[cold]
./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.540ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.520ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.500ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.480ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.460ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.440ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.420ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.400ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.380ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.360ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.340ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.320ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.300ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.280ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.260ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.240ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.220ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.200ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.180ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.160ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.140ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.120ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.100ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.80_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.60_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.40_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.20_ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.0__ebP2_.bg.PO4.Tdep.pCO2FKr_x0.5.PO4PD.pO2Kr_radfor.SPIN 8000

[warm]
./runmuffin.sh muffin.AP.540ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.540ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.520ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.520ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.500ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.500ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.480ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.480ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.460ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.460ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.440ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.440ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.420ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.420ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.400ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.400ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.380ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.380ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.360ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.360ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.340ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.340ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.320ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.320ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.300ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.300ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.280ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.280ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.260ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.260ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.240ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.240ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.220ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.220ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.200ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.200ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.180ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.180ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.160ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.160ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.140ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.140ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.120ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.120ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.100ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.100ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.80_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.80_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.60_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.60_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.40_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.40_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.20_ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.20_ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000
./runmuffin.sh muffin.AP.0__ebP2_.eb_go_gs_ac_bg.PO4.SPIN pohletal.SciAdv.2022 AP.0__ebP2_.bg.PO4.Tdep.pCO2FKr_x2.0.PO4PD.pO2Kr_radfor.SPIN 8000

################################################################
################################################################
################################################################

################################################################
### readme.txt #################################################
################################################################

For: 'Continental configuration drives variability in ocean oxygenation through the Phanerozoic'

Alexandre POHL1,2*, Andy RIDGWELL1*, Richard G. STOCKEY3, Christophe THOMAZO2, Andrew KEANE4, Emmanuelle VENNIN2, Christopher R. SCOTESE5
1Department of Earth and Planetary Sciences, University of California, Riverside, CA, USA
2Biogéosciences, UMR 6282, UBFC/CNRS, Université Bourgogne Franche-Comté, 6 boulevard Gabriel, F-21000 Dijon, France
3Department of Geological Sciences, Stanford University, Stanford, CA 94305, USA
4School of Mathematical Sciences and Environmental Research Institute, University College Cork, College Road, Cork, Ireland
5Department of Earth and Planetary Sciences, Northwestern University, Evanston, Illinois 60208, USA
*These authors contributed equally to this work


################################################################
18/06/2020 -- README.txt file creation (A.P.)
18/06/2020 -- added files
15/12/2021 -- updated readme.txt and added files for series E to H
19/01/2022 -- edited to reflect new genie-userconfigs directory
20/01/2022 -- added modern oxygenation experiments
12/07/2022 -- copied to new directory
15/07/2022 -- corrected/updated user-config path
################################################################

Provided are the configuration files necessary to run: 
    A] series #1 (i.e., all runs at 2240 ppm)
    B] series #2 (i.e., like series #1 but pCO2 varied so as to approximately 'correct' for the tectonic impacts on climate)
    C] like series #1 but with no dependence of remineralization on temperature
    D] like series #1 but with a flat bottom, [0-140] Ma only
    E] like series #1 but at 1120 ppm
    F] 460 Ma and 440 Ma like series #1 but using Torsvik's BugPlates continental reconstructions
    G] Drake world simulations shown in Extended Data Fig. 10
    H] Ridge world simulations shown in Extended Data Fig. 10
    I] present-day oxygenation experiment shown in Extended Data Figure 1:
       1st experiment -- present-day ocean circulation and marine productivity of Cao et al. [2009], as shown in panel (b)
       2nd experiment -- present-day ocean circulation of Cao et al. [2009] but with ecological model used in the Phanerozoic experiment, as shown in panel (c)
    J] present-day ocean circulation (Cao et al. [2009]) ventillation age tracer experiment (not shown in paper)

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

Remark: Phanerozoic continental configurations are not intended, and should not be used to conduct 'realistic' simulations.
        Indeed, boundary conditions used in FOAM, including solar luminosity and pCO2, were designed for a simple sensitivity test to the continental configuration.
        The resulting, inadapted albedo values and latitudinal temperature gradients prevent the use of these GENIE base configurations for paleo investigations beyond this very specific application.

Remark 2: Drake- and ridge- world configurations are just the ones provided in the standard model distribution, plus age tracers

################ A] series #1 ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.0__rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.20_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.40_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.60_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.80_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.100rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.120rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.140rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.160rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.160rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.180rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.180rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.200rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.200rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.220rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.220rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.240rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.240rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.260rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.260rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.280rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.280rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.300rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.300rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.320rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.320rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.340rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.340rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.360rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.360rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.380rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.380rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.400rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.400rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.420rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.420rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.440rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.440rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.460rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.460rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.480rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.480rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.500rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.500rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.520rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.520rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.540rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.540rdP1_.PO4.8P8Z.Tdep.SPIN 20000

################ B] series #2 ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.0__rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.20_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.40_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.60_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.80_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.100rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.120rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.140rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.160rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.160rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.180rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.180rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.200rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.200rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.220rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.220rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.240rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.240rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.260rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.260rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.280rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.280rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.300rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.300rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.320rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.320rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.340rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.340rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.360rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.360rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.380rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.380rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.400rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.400rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.420rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.420rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.440rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.440rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.460rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.460rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.480rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.480rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.500rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.500rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.520rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.520rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.540rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.540rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000

################ C] no T-dep ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.0__rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.20_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.40_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.60_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.80_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.100rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.120rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.140rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.160rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.160rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.180rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.180rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.200rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.200rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.220rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.220rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.240rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.240rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.260rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.260rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.280rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.280rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.300rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.300rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.320rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.320rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.340rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.340rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.360rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.360rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.380rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.380rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.400rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.400rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.420rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.420rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.440rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.440rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.460rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.460rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.480rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.480rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.500rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.500rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.520rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.520rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.540rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.540rdP1_.PO4.8P8Z.SPIN 20000

################ D] flat bottom ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.0__rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.20_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.40_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.60_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.80_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.100rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.120rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.140rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000

################ E] 1120 ppm ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.0__rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.20_rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.40_rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.60_rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.80_rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.100rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.120rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.140rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.160rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.160rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.180rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.180rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.200rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.200rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.220rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.220rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.240rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.240rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.260rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.260rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.280rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.280rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.300rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.300rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.320rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.320rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.340rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.340rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.360rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.360rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.380rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.380rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.400rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.400rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.420rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.420rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.440rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.440rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.460rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.460rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.480rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.480rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.500rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.500rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.520rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.520rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000
./runmuffin.sh muffin.AP.540rdP1_.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.540rdP1_.PO4.8P8Z.Tdep.1120ppm.SPIN 20000

################ F] BugPlates ################

./runmuffin.sh muffin.AP.440bug__.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.440bug__.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.460bug__.eb_go_gs_ac_eg.PO4.SPIN PUBS/published/Pohl_et_al.2022 AP.460bug__.PO4.8P8Z.Tdep.SPIN 20000

################ G] Drake world ################

./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.4.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.4.5X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.5.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.5.5X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.6.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.7.5X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.8.25X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.8.5X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.9.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.9.75X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.10.75X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.11.25X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.12.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_dw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_dw_hi.BASES.16.0X.SPIN 60000

################ H] Ridge world ################

./runmuffin.sh muffin.CB.eg_rw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_rw_hi.BASES.4.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_rw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_rw_hi.BASES.5.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_rw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_rw_hi.BASES.6.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_rw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_rw_hi.BASES.8.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_rw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_rw_hi.BASES.10.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_rw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_rw_hi.BASES.12.0X.SPIN 60000
./runmuffin.sh muffin.CB.eg_rw_hi.BASES_agetr PUBS/published/Pohl_et_al.2022 muffin.CB.eg_rw_hi.BASES.16.0X.SPIN 60000

################ I] modern oxygenation #########

./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Pohl_et_al.2022 muffin.CB.p_worjh2.BASES.caoetal.SPIN 10000
./runmuffin.sh muffin.CBE.p_worjh2.BASES PUBS/published/Pohl_et_al.2022 muffin.CBE.p_worjh2.BASES.wardetal.SPIN 10000

################ J] modern ventillation age ####

./runmuffin.sh muffin.C.p_worjh2.r PUBS/published/Pohl_et_al.2022 muffin.C.p_worjh2.r.age.SPIN 10000

################################################################
################################################################
################################################################

################################################################
### readme.txt #################################################
################################################################

For: 'Plate motion drives variability in ocean oxygenation through the Phanerozoic'

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
################################################################

Provided are the configuration files necessary to run: 
    A] series #1 (i.e., all runs at 2240 ppm)
    B] series #2 (i.e., like series #1 but pCO2 varied so as to approximately 'correct' for the tectonic impacts on climate)
    C] like series #1 but with no dependence of remineralization on temperature
    D] like series #1 but with a flat bottom, [0-140] Ma only

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

Remark: These continental configurations are not intended, and should not be used to conduct 'realistic' simulations.
        Indeed, boundary conditions used in FOAM, including solar luminosity and pCO2, were designed for a simple sensitivity test to the continental configuration.
        The resulting, inadapted albedo values and latitudinal temperature gradients prevent the use of these GENIE base configurations for paleo investigations beyond this very specific application.

################ A] series #1 ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.0__rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.20_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.40_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.60_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.80_rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.100rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.120rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.140rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.160rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.160rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.180rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.180rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.200rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.200rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.220rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.220rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.240rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.240rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.260rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.260rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.280rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.280rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.300rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.300rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.320rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.320rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.340rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.340rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.360rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.360rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.380rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.380rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.400rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.400rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.420rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.420rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.440rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.440rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.460rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.460rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.480rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.480rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.500rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.500rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.520rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.520rdP1_.PO4.8P8Z.Tdep.SPIN 20000
./runmuffin.sh muffin.AP.540rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.540rdP1_.PO4.8P8Z.Tdep.SPIN 20000

################ B] series #2 ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.0__rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.20_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.40_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.60_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.80_rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.100rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.120rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.140rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.160rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.160rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.180rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.180rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.200rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.200rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.220rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.220rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.240rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.240rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.260rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.260rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.280rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.280rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.300rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.300rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.320rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.320rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.340rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.340rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.360rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.360rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.380rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.380rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.400rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.400rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.420rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.420rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.440rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.440rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.460rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.460rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.480rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.480rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.500rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.500rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.520rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.520rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000
./runmuffin.sh muffin.AP.540rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.540rdP1_.PO4.8P8Z.Tdep.detr.SPIN 20000

################ C] no T-dep ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.0__rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.20_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.40_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.60_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.80_rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.100rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.120rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.140rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.160rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.160rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.180rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.180rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.200rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.200rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.220rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.220rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.240rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.240rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.260rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.260rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.280rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.280rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.300rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.300rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.320rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.320rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.340rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.340rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.360rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.360rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.380rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.380rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.400rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.400rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.420rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.420rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.440rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.440rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.460rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.460rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.480rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.480rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.500rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.500rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.520rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.520rdP1_.PO4.8P8Z.SPIN 20000
./runmuffin.sh muffin.AP.540rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.540rdP1_.PO4.8P8Z.SPIN 20000

################ D] flat bottom ################

./runmuffin.sh muffin.AP.0__rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.0__rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.20_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.20_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.40_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.40_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.60_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.60_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.80_rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.80_rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.100rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.100rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.120rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.120rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000
./runmuffin.sh muffin.AP.140rdP1_.eb_go_gs_ac_eg.PO4.SPIN MS/pohletal.Nature.2021 AP.140rdP1_.PO4.8P8Z.Tdep_fb.SPIN 20000

################################################################
################################################################
################################################################

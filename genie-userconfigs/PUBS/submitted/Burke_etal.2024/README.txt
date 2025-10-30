################################################################
### README.txt #################################################
################################################################

For:
'Miocene ocean circulation shifted expansive oxygen deficient zones to the Atlantic'
Janet E. Burke, Keyi Cheng, Andy Ridgwell, Donald E. Penman, Dalton S. Hardisty.

################################################################
2024/12/04 -- README.txt file created
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments (including those in SI).

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################################################################
### model experiments ##########################
################################################################

Used Temperature-dependent export productivity (TDEP) from Crichton et al., (2021) GMD.

Used paleo-bathymetry from 15Ma to 2.5Ma from Crichton et al., (2021) CP and the modern bathymetry from Cheng et al., (2024). 

Each experiment was run for 10000 years from the initial state without a spin-up. Iodine cycle parameter used the 'Fennel-threshold' combination calibrated in Cheng et al., (2024).
Detailed iodine cycle parameters are below: 
### IODINE CYCLE CONTROLS ###
# set biological IO3 uptake
bg_par_bio_red_POC_POI = 3.5e-4
# select option for watercolumn reduction
bg_opt_bio_remin_reduce_IO3toI='threshold'
# select basic oxidation option
bg_opt_bio_remin_oxidize_ItoIO3='Fennel'
# set Fennel parameters
bg_par_bio_remin_cO2_ItoIO3=20.0E-6
bg_par_bio_remin_kItoIO3=0.1
# set [O2] threshold (mol kg-1) for (complete) IO3 reduction to I
bg_par_bio_remin_cO2_IO3toI=10e-6
#

Sensitivity analysis:

1) Preindustrial CO2 (280ppm) scenario: 

./runmuffin.sh muffin.CB.p_worjh2.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.00p0a.BASESI.280_0p0.SPIN 10000

./runmuffin.sh muffin.CB.umQ02p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.02p5a.BASESI.CASopen.280_0p3.SPIN 10000 

./runmuffin.sh muffin.CB.umQ02p5b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.02p5b.BASESI.CASclosed.280_0p1.SPIN 10000

./runmuffin.sh muffin.CB.umQ04p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.04p5a.BASESI.CASopen.280_0p5.SPIN 10000 

./runmuffin.sh muffin.CB.umQ04p5b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.04p5b.BASESI.CASclosed.280_0p2.SPIN 10000

./runmuffin.sh muffin.CB.umQ07p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.07p5a.BASESI.CASopen.280_0p4.SPIN 10000 

./runmuffin.sh muffin.CB.umQ07p5b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.07p5b.BASESI.CASclosed.280_0p3.SPIN 10000

./runmuffin.sh muffin.CB.umQ10p0a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.10p0a.BASESI.CASopen.280_0p4.SPIN 10000

./runmuffin.sh muffin.CB.umQ10p0b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.10p0b.BASESI.CASclosed.280_0p3.SPIN 10000

./runmuffin.sh muffin.CB.umQ12p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.12p5a.BASESI.CASopen.280_0p2.SPIN 10000

./runmuffin.sh muffin.CB.umQ15p0a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.15p0a.BASESI.CASopen.280_0p1.SPIN 10000 

2) High CO2 (560ppm) scenario: 

./runmuffin.sh muffin.CB.p_worjh2.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.00p0a.BASESI.560_0p0.SPIN 10000

./runmuffin.sh muffin.CB.umQ02p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.02p5a.BASESI.CASopen.560_0p3.SPIN 10000 

./runmuffin.sh muffin.CB.umQ02p5b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.02p5b.BASESI.CASclosed.560_0p1.SPIN 10000

./runmuffin.sh muffin.CB.umQ04p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.04p5a.BASESI.CASopen.560_0p5.SPIN 10000 

./runmuffin.sh muffin.CB.umQ04p5b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.04p5b.BASESI.CASclosed.560_0p2.SPIN 10000

./runmuffin.sh muffin.CB.umQ07p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.07p5a.BASESI.CASopen.560_0p4.SPIN 10000 

./runmuffin.sh muffin.CB.umQ07p5b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.07p5b.BASESI.CASclosed.560_0p3.SPIN 10000

./runmuffin.sh muffin.CB.umQ10p0a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.10p0a.BASESI.CASopen.560_0p4.SPIN 10000

./runmuffin.sh muffin.CB.umQ10p0b.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.10p0b.BASESI.CASclosed.560_0p3.SPIN 10000

./runmuffin.sh muffin.CB.umQ12p5a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.12p5a.BASESI.CASopen.560_0p2.SPIN 10000

./runmuffin.sh muffin.CB.umQ15p0a.BASESI PUBS/submitted/Burke_etal.2024 JB.TDEP.15p0a.BASESI.CASopen.560_0p1.SPIN 10000 

################################################################
################################################################
################################################################
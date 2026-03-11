################################################################
### readme.txt #################################################
################################################################

For: 'The “warm, sluggish ocean” - a critical reevaluation'

Authors
Or M. Bialik1,2*, Anta-Clarisse Sarr3,4, Yannick Donnadieu4, Alexandre Pohl5*

Affiliations 
1. Institute of Geology and Palaeontology, University of Münster, Corrensstr. 24, 48149 Münster, Germany
2. Israel Oceanographic and Limnological Research, National Institute of Oceanography, 310800, Haifa, Israel
3. Department of Earth Science, University of Oregon, Eugene, OR, USA
4. Aix-Marseille Université, CNRS, IRD, INRAE, Collège de France, CEREGE, Aix-en-Provence, France
5. Université Bourgogne Europe, CNRS, Biogéosciences UMR 6282, 21000 Dijon,
France.

Corresponding authors: OMB (obialik@ocean.org.il); AP (alexandre.pohl@cnrs.fr)


################################################################
09/02/2026 -- README.txt file creation (A.P.)
09/02/2026 -- added files
################################################################

Provided are the configuration files necessary to run simulations with median
orbit only.
A] with climate after Judd et al. (2024)
B] with x0.5 pCO2 
C] with x2.0 pCO2 

Also provided is the modern, calibrated simulation (D) and the Devonian simulation with instantaneous warming (E).

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

# =========== (A) with climate after Judd et al. (2024)  =========== #

./runmuffin.sh muffin.AP.M20Sc0__.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc0__.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc15_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc15_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc26_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc26_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc36_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc36_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc52_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc52_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc61_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc61_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc69_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc69_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc75_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc75_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc87_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc87_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc97_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc97_.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc107.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc107.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc116.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc116.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc131.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc131.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc149.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc149.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc168.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc168.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc178.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc178.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc196.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc196.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc234.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc234.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc265.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc265.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc301.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc301.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc327.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc327.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc366.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc366.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc400.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc400.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc415.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc415.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc436.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc436.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc496.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc496.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc510.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc510.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc520.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc520.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc530.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc530.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc541.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc541.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN 40000

# =========== (B) with x0.5 pCO2  =========== #

./runmuffin.sh muffin.AP.M20Sc0__.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc0__.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc15_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc15_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc26_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc26_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc36_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc36_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc52_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc52_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc61_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc61_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc69_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc69_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc75_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc75_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc87_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc87_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc97_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc97_.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc107.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc107.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc116.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc116.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc131.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc131.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc149.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc149.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc168.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc168.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc178.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc178.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc196.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc196.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc234.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc234.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc265.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc265.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc301.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc301.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc327.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc327.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc366.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc366.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc400.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc400.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc415.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc415.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc436.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc436.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc496.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc496.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc510.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc510.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc520.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc520.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc530.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc530.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc541.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc541.C.pCO2Judd24x0.5..0.0_23.5_Sep.._radfor_age.SPIN 40000

# =========== (C) with x2.0 pCO2  =========== #

./runmuffin.sh muffin.AP.M20Sc0__.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc0__.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc15_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc15_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc26_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc26_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc36_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc36_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc52_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc52_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc61_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc61_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc69_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc69_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc75_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc75_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc87_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc87_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc97_.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc97_.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc107.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc107.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc116.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc116.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc131.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc131.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc149.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc149.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc168.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc168.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc178.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc178.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc196.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc196.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc234.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc234.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc265.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc265.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc301.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc301.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc327.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc327.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc366.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc366.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc400.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc400.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc415.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc415.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc436.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc436.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc496.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc496.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc510.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc510.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc520.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc520.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc530.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc530.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000
./runmuffin.sh muffin.AP.M20Sc541.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc541.C.pCO2Judd24x2.0..0.0_23.5_Sep.._radfor_age.SPIN 40000

# =========== (D) modern, calibrated simulation  =========== #

./runmuffin.sh muffin.C.p_worjh2.r PUBS/published/Pohl_et_al.2022 muffin.C.p_worjh2.r.age.SPIN 40000

# =========== (E) Devonian simulation with instantaneous warming =========== #

(requires that simulation AP.M20Sc400.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN was previously completed, since used here as a restart):

./runmuffin.sh muffin.AP.M20Sc400.C.agetr.SPIN PUBS/submitted/Bialik_et_al.SciAdv.2026 AP.M20Sc400.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age_db.SPIN 40000 AP.M20Sc400.C.pCO2Judd24..0.0_23.5_Sep.._radfor_age.SPIN

################################################################
################################################################
################################################################

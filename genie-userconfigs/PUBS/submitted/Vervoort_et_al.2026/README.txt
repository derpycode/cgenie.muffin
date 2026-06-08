################################################################
### README.txt #################################################
################################################################

For:
'100,000-year cycles in an ice-free world'
P. Vervoort, A. Ridgwell, D. Hülse, S. E. Greene, S. Kirtland Turner

################################################################
8 June, 2026 -- README.txt file creation (PV)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an opportunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)


### model experiments -- spinups ################################

The commands to run the spinups are listed as follows:

fkl_PV01 => hemispherically symmetric continental configuration with one continent from pole-to-pole
         => same as in Vervoort et al.(2026, Pal&Pal)
fkl_PV03 => hemispherically asymmetric continental configuration with one continent in the NH
         => same as in Vervoort et al.(2026, Pal&Pal)
fkl_PV05 => hemispherically symmetric continental configuration with semirestricted basin configuration
         => new in this study
fkl_PV30 => hemispherically symmetric continental configuration with narrow equatorial corridor
         => new in this study
fkl_PV31 => hemispherically symmetric continental configuration with wide equatorial corridor
         => new in this study	 

(1a) INITIAL SPINUP
The initial, 1st-stage closed system spin-up needs 20 kyr to reach equilibrium:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV01_D3x_ALK.SPIN1 20000
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV03_D3x_ALK.SPIN1 20000
./runmuffin.sh muffin.CBSRG.fkl_PV05.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV05_D3x_ALK.SPIN1 20000
./runmuffin.sh muffin.CBSRG.fkl_PV30.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV30_D3x_ALK.SPIN1 20000
./runmuffin.sh muffin.CBSRG.fkl_PV31.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV31_D3x_ALK.SPIN1 20000

(1b) Second Stage SPINUP
The follow-on, 2nd-stage (accelerated) open system spin-up requires a long spinup time of ~900 kyr to reach equilibrium

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV01_D3x_ALK.SPIN2 880000 fkl_PV01_D3x_ALK.SPIN1
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV03_D3x_ALK.SPIN2 880000 fkl_PV03_D3x_ALK.SPIN1
./runmuffin.sh muffin.CBSRG.fkl_PV05.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV05_D3x_ALK.SPIN2 880000 fkl_PV05_D3x_ALK.SPIN1


### model experiments -- main ensemble ##########################

Below the commands to run the experiments with astronomical forcing for 4 Myr
NOTE: Experiment 7 (EXP7) corresponds to that described in Vervoort et al. (2026, Pal&Pal)
NOTE: PV30 and PV31 use SPIN1 as restart instead of SPIN2

(2a) For EXPERIMENT 7 (Variable Corg burial, PO4 weathering, PO4 regeneration, fixed kerogen weathering)

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV01_D3x_ALK_EXP7.4Ma 4000000 fkl_PV01_D3x_ALK.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV03_D3x_ALK_EXP7.4Ma 4000000 fkl_PV03_D3x_ALK.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV05.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV05_D3x_ALK_EXP7.4Ma 4000000 fkl_PV05_D3x_ALK.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV30.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV30_D3x_ALK_EXP7.4Ma 4000000 fkl_PV30_D3x_ALK.SPIN1
./runmuffin.sh muffin.CBSRG.fkl_PV31.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV31_D3x_ALK_EXP7.4Ma 4000000 fkl_PV31_D3x_ALK.SPIN1

(2b) EXPERIMENT 7 without orbital forcing (restricted basin only)

./runmuffin.sh muffin.CBSRG.fkl_PV05.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV05_D3x_ALK_EXP7_fixORB.1Ma 1000000 fkl_PV05_D3x_ALK.SPIN2

(2c) EXPERIMENT with PO4 feedback fix or PO4 regeneration feedback fixed (restricted basin only)

./runmuffin.sh muffin.CBSRG.fkl_PV05.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV05_D3x_ALK_fixPO4w.4Ma 4000000 fkl_PV05_D3x_ALK.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV05.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV05_D3x_ALK_fixPreg.4Ma 4000000 fkl_PV05_D3x_ALK.SPIN2

(2d) EXPERIMENT 7 with orbital forcing, enhanced CO2 outgassing

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV01_D3x_ALK_EXP7_add_0.15_CO2.4Ma 4000000 fkl_PV01_D3x_ALK.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV03_D3x_ALK_EXP7_add_0.15_CO2.4Ma 4000000 fkl_PV03_D3x_ALK.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV05.BASES PUBS/submitted/Vervoort_et_al.2026 fkl_PV05_D3x_ALK_EXP7_add_0.15_CO2.4Ma 4000000 fkl_PV05_D3x_ALK.SPIN2

################################################################
################################################################
################################################################

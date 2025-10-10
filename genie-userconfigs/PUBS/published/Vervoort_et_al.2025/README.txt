################################################################
### README.txt #################################################
################################################################

For:
'Earth System Model Analysis of how Astronomical Forcing is Imprinted onto the Marine Geological Record:
The Role of the Marine Organic Carbon Cycle and Feedbacks'
P. Vervoort, S. Kirtland Turner, D. Hülse, Sarah E. Greene, and A. Ridgwell

################################################################
02/21/2025 -- README.txt file creation (PV)
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
         => differs from fkl_pp30 in Vervoort et al.(2024): added shelf area
fkl_PV03 => hemispherically asymmetric continental configuration with one continent in the NH
         => differs from fkl_np30 in Vervoort et al.(2024): added shelf area and continent extends 
		    further to the equator at the expense of latitudinal converage

(1a) INITIAL SPINUP

The initial, 1st-stage closed system spin-up needs 20 kyr to reach equilibrium:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x.SPIN1 20000
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x.SPIN1 20000

Separate spin-up is needed to add temperature-dependent export and remineralization:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_Tdep.SPIN1 20000
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_Tdep.SPIN1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage (accelerated) open system spin-up requires a long spinup time of ~1 Myr to reach equilibrium

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x.SPIN2 980000 fkl_PV01_D3x.SPIN1
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x.SPIN2 980000 fkl_PV03_D3x.SPIN1

Separate spin-up is needed to add temperature-dependent export and remineralization:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_Tdep.SPIN2 980000 fkl_PV01_D3x_Tdep.SPIN1
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_Tdep.SPIN2 980000 fkl_PV03_D3x_Tdep.SPIN1

**Because carbonate and silicate weathering are temperature-dependent, a reference temperature (T0) controls the baseline 
  weathering rates (Colbourn et al., 2013). Without seasonality of insolation, T0 is equal to the global mean surface land 
  temperature diagnosed from the first spin-up. However, the exponential relation between temperature and silicate 
  weathering produces higher annual mean weathering rates when seasonality of insolation is represented. To correct for the 
  impact of enabling seasonality and to avoid drift in CO2 and temperature, we apply a two-step modification in the second 
  stage spin-up. 
		1.	Iteratively adjust T0 to obtain modelled annual mean silicate weathering rates in a seasonally-forced climate 
		    equal to those diagnosed in the first spin-up.
		2.	Adjust the baseline CaCO3 weathering rate to correct for the new T0.
  After these adjustments, the second stage spin-up is run for 1-Myr. The long simulation time is necessary because small 
  initial imbalances to the carbon cycle that arise from switching from a ‘closed’ to an ‘open’ system are amplified by the 
  temperature dependence of silicate weathering (and by extension kerogen and PO4 weathering), which prolongs the 
  disequilibrium in the carbon cycle. 

### model experiments -- main ensemble ##########################

Below the commands to run the experiments with astronomical forcing for 4 Myr
Experiment 4-8 correspond to the numbering as described in the manuscript

(2a) For EXPERIMENT 4, all inorganic carbon cycle feedbacks (as in Exp.4 in Vervoort et al.(2024)):

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_EXP4.4Ma 4000000 fkl_PV01_D3x.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_EXP4.4Ma 4000000 fkl_PV03_D3x.SPIN2

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_Tdep_EXP4.4Ma 4000000 fkl_PV01_D3x.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_Tdep_EXP4.4Ma 4000000 fkl_PV03_D3x.SPIN2

(2b) For EXPERIMENT 5, EXPERIMENT 4 + Corg burial feedback:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_EXP5.4Ma 4000000 fkl_PV01_D3x.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_EXP5.4Ma 4000000 fkl_PV03_D3x.SPIN2

(2c) For EXPERIMENT 6, EXPERIMENT 5 + Porg burial feedback:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_EXP6.4Ma 4000000 fkl_PV01_D3x.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_EXP6.4Ma 4000000 fkl_PV03_D3x.SPIN2

(2d) For EXPERIMENT 7, EXPERIMENT 6 + PO4 weathering feedback:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_EXP7.4Ma 4000000 fkl_PV01_D3x.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_EXP7.4Ma 4000000 fkl_PV03_D3x.SPIN2

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_EXP7_Tdep.4Ma 4000000 fkl_PV01_D3x.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_EXP7_Tdep.4Ma 4000000 fkl_PV03_D3x.SPIN2

(2e) For EXPERIMENT 8, EXPERIMENT 7 + kerogen weathering feedback:

./runmuffin.sh muffin.CBSRG.fkl_PV01.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV01_D3x_EXP8.4Ma 4000000 fkl_PV01_D3x.SPIN2
./runmuffin.sh muffin.CBSRG.fkl_PV03.BASES PUBS/published/Vervoort_et_al.2025 fkl_PV03_D3x_EXP8.4Ma 4000000 fkl_PV03_D3x.SPIN2

################################################################
################################################################
################################################################

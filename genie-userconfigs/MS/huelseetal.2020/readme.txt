################################################################
### readme.txt #################################################
################################################################

For:
Temperature-driven nutrient recycling and subsurface euxinia as an end-Permian kill mechanism
Dominik Hülse, Kimberly V. Lau, Sebastiaan J. van de Velde, Sandra Arndt, Katja Meyer, Andy Ridgwell

################################################################
20/08/27 -- README.txt file creation (D.H.)
################################################################

Provided is the code used to create the model experiments for the three phases (end-Permian background, Start Warming, Main Extinction) as presented in Fig. 3 of the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an opportunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### model experiments ##########################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The three experiments are run from cold for 10kyrs to steady-state:

1) end-Permian background (2.5 x modern pCO2, 1.0 x modern PO4)
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0251b.BASESFePOM-S MS/huelseetal.2020 01_endPermian_background_02.5CO2_1.00PO4_Tmp_98sink_sulfE6 10000

2) Start Warming (7.5 x modern pCO2, 1.5 x modern PO4)
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0251b.BASESFePOM-S MS/huelseetal.2020 02_StartWarming_07.5CO2_1.50PO4_Tmp_98sink_sulfE6 10000

3) Main Extinction (20.0 x modern pCO2, 3.0 x modern PO4)
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0251b.BASESFePOM-S MS/huelseetal.2020 03_MainExtinction_20.0CO2_2.00PO4_Tmp_98sink_sulfE6 10000

### NOTES ######################################################


################################################################
################################################################
################################################################

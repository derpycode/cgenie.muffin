################################################################
### readme.txt #################################################
################################################################

For: 'Decreasing Phanerozoic extinction intensity as a consequence of Earth surface oxygenation and metazoan ecophysiology'

Richard G. Stockey1*, Alexandre Pohl2,3, Andy Ridgwell2, Seth Finnegan4, and Erik A. Sperling1

1 Department of Geological Sciences, Stanford University, Stanford, CA 94305, USA
2 Department of Earth and Planetary Sciences, University of California, Riverside, CA, USA
3 Biogéosciences, UMR 6282, UBFC/CNRS, Université Bourgogne Franche-Comté, 6 boulevard Gabriel, F-21000 Dijon, France
4 Department of Integrative Biology, University of California, Berkeley, Berkeley, CA, USA
* Corresponding author: rstockey@stanford.edu

################################################################
11/15/2020 -- README.txt file creation (R.G.S)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an opportunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

All experiments are run from: $HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

All five O2-CO2 ensembles are described below using x-y placeholders for atmospheric O2 and CO2 
concentrations as a function of preindustrial atmospheric levels (PAL)

For each ensemble, all possible combinations of x and y are used, resulting in 42 simulations per ensemble
O2 forcings (x) are the same across all ensembles
CO2 forcings (y) are described separately for each ensemble

x --> 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1 [for all ensembles described below]

######## model ensembles in main text ##########################

# Ordovician continental configuration
y --> 4, 8, 16, 32, 64, 128
./runmuffin.sh muffin.CB.fm0450dc.BASES MS/stockeyetal.PNAS.2020/fm0450.CO2.O2 muffin.CB.fm0450dc.BASES.yCO2.xO2.1EFD.config 10000

# Permian continental configuration
y --> 2, 4, 8, 16, 32, 64
runmuffin.sh cgenie.eb_go_gs_ac_bg.p0251b.BASES MS/stockeyetal.PNAS.2020//p0251.CO2.O2.ramp cgenie.eb_go_gs_ac_bg.p0251b.BASES.yCO2.xO2.1EFD.ramp.config 10000

# Paleocene continental configuration
y --> 1, 2, 4, 8, 16, 32
runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASES MS/stockeyetal.PNAS.2020/p0055.CO2.O2.ramp cgenie.eb_go_gs_ac_bg.p0055c.BASES.yCO2.xO2.1EFD.ramp.config 20000

####### model ensembles in supplemental material ################

# Ordovician continental configuration - shallow remineralization depth
y --> 4, 8, 16, 32, 64, 128
./runmuffin.sh muffin.CB.fm0450dc.BASES MS/stockeyetal.PNAS.2020/fm0450.CO2.O2.shallowremin muffin.CB.fm0450dc.BASES.yCO2.xO2.0.34EFD.config 10000

# Ordovician continental configuration - modern solar luminosity
y --> 1, 2, 4, 8, 16, 32
./runmuffin.sh muffin.CB.fm0450dc.BASES.modern.solar.constant MS/stockeyetal.PNAS.2020/fm0450.CO2.O2.modern.solar.constant muffin.CB.fm0450dc.BASES.modern.solar.constant.yCO2.xO2.1EFD.config 10000

################################################################
################################################################
################################################################
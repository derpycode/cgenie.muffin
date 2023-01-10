################################################################
### readme.txt #################################################
################################################################

For:
'Trace-metal isotope ‘fingerprints’ of past solid Earth-ocean interactions'
Markus Adloff, Andy Ridgwell, Fanny M. Monteiro, Adina Paytan, and Sarah E. Greene

################################################################
02/12/2022 -- README.txt file creation (M.A.)
2023/01/10 -- edits by A.R.
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

### model experiments -- spinups ###############################

We used the spinups produced by Adloff et al. 2020. Please follow the spin-up procedure provided there.

### model experiments -- step changes in solid Earth-ocean interactions ###

The user configurations are named as follows:
TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+P+'_Andy'
N being the factor of change (05 = halving, 2 = doubling) and P the process perturbed

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa / TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+P+'_Andy' 20000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES

### model experiments -- transient hydrothermal events ###################

The user configurations are named as follows:
TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+C+'HT-HYD_Andy_'+T+E
N being the factor of change, C being the C flux change ('noCO2' = extra metal but no extra CO2 emissions, 'RCO2' = extra CO2 emissions but no climate response, '' = extra CO2 at same rate increase as metals, 'moreCO2' = CO2 emissions are increased by twice as much as metal emissions), T is the duration of the hydrothermal event (in years) and E is the recovery option.

Each experiment consists of four stages:

1) Hydrothermal event
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa / TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+C+'HT-HYD_Andy_'+T T spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES

2) Start of recovery (without model acceleration)

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa / TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+C+'HT-HYD_Andy_'+T+'_rec' 10000 TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+C+'HT-HYD_Andy_'+T

3) continued recovery (with model acceleration)

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa / TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+C+'HT-HYD_Andy_'+T+'_rec2' 15000000 TM.worbe2.RidgwellHargreaves1997_S36x36.'+N+C+'HT-HYD_Andy_'+T+'_rec'

Please contact the authors for questions about the model parameter file configurations.

################################################################
################################################################
################################################################

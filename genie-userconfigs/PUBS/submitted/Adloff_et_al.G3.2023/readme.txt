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

### model experiments series A -- step changes in solid Earth-ocean interactions ###

The user configurations are named as follows:
TM.worbe2.RidgwellHargreaves1997_S36x36.A_'+N+P
N being the factor of change (05 = halving, 2 = doubling, etc) and P the
process perturbed (identifiers as in table 1 of the manuscript)

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa PUBS/submitted/Adloff_et_al.G3.2023 TM.worbe2.RidgwellHargreaves1997_S36x36.A_'+N+P 20000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES
997_S36x36.'+N+P+'_Andy' 20000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES

### model experiments series B -- transient hydrothermal events ###################

The user configurations are named as follows:
TM.worbe2.RidgwellHargreaves1997_S36x36.B_'+N+C+'HT-HYD_'+T+E
N being the factor of change, C being the C flux change ('RCO2' = restored
(i.e. constant) atmospheric CO2, 'noClim' = variable CO2 concentration but
no radiative effect, '' = full feedbacks, pre-industrial CO2:metal ratio
of hydrothermal emissions, 'moreCO2' = full feedbacks, 2xpre-industrial
CO2:metal ratio of hydrothermal emissions, T is the duration of the
hydrothermal event (in years) and E is the recovery option.

Each experiment consists of four stages:

1) Hydrothermal event
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa PUBS/submitted/Adloff_et_al.G3.2023 TM.worbe2.RidgwellHargreaves1997_S36x36.B_'+N+C+'HT-HYD_'+T T spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES

2) Start of recovery (without model acceleration)

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa PUBS/submitted/Adloff_et_al.G3.2023 TM.worbe2.RidgwellHargreaves1997_S36x36.B_'+N+C+'HT-HYD_'+T+'_rec' 10000 TM.worbe2.RidgwellHargreaves1997_S36x36.B_'+N+C+'HT-HYD_'+T

3) continued recovery (with model acceleration)

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa PUBS/submitted/Adloff_et_al.G3.2023 TM.worbe2.RidgwellHargreaves1997_S36x36.B_'+N+C+'HT-HYD_'+T+'_rec2' 15000000
TM.worbe2.RidgwellHargreaves1997_S36x36.B_'+N+C+'HT-HYD_'+T+'_rec'

################################################################
################################################################
################################################################

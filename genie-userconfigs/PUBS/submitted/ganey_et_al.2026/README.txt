################################################################
### README.txt #################################################
################################################################

For:
'Additionality of climate benefits from emission reduction and carbon dioxide removal'
T.M. Ganey, M.P. Hain, A. Ridgwell, & A. Paytan

################################################################
05/25/2025 -- README.txt file creation (TMG)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

### model experiments - spinup #################################

(1) Preindustrial-like spinup

The initial spinup experiment needs 10 kyr to reach equilibrium. The command to run the spinup is:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL 10000

This model experiment also serves as the 'preindustrial' (SSPO) control scenario.


### model experiments - SSP controls ###########################

(2) Emissions-driven controls

Two emissions-driven control experiments (SSP2-4.5, SSP3–7.0) are initialized from the preindustrial spinup and run for 2500 years, starting in year 1750:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.CTRL 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.CTRL 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL


### model experiments - main ensemble ##########################

Below are the commands to run the CDR experiments that comprise the main model ensemble. Each CDR experiment is initialized from the same preindustrial spinup and run for 2500 years beginning in year 1750.

The name scheme for CDR experiments is:
[base config].[emission scenario].[intervention type].pulse[intervention size]

The four intervention types are:
DAC - direct air capture
DOC - direct ocean capture/removal
OAE - ocean alkalinity enhancement
CMB - combined (all 3 interventions)

CDR interventions are implemented as 'pulses' of either 10 or 100 Pg C equivalent.

(3a) Preindustrial (SSP0) CDR

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.DAC.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.DAC.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.DOC.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.DOC.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.OAE.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.OAE.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CMB.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CMB.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL

(3b) SSP2–4.5 CDR

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.DAC.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.DAC.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.DOC.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.DOC.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.OAE.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.OAE.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.CMB.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP2x.CMB.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL

(3c) SSP3–7.0 CDR

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.DAC.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.DAC.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.DOC.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.DOC.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.OAE.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.OAE.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.CMB.pulse10 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL
./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASE PUBS/submitted/ganey_et_al.2026 cgenie.eb_go_gs_ac_bg.worjh2.BASE.SSP3x.CMB.pulse100 2500 cgenie.eb_go_gs_ac_bg.worjh2.BASE.PI.CTRL


################################################################
################################################################
################################################################

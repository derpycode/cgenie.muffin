################################################################
### README.txt #################################################
################################################################

For:
'Earth System Model Analysis of how Astronomical Forcing is Imprinted onto the Marine Geological Record'
P. Vervoort, S. Kirtland Turner, F. Rochholz, A. Ridgwell

################################################################
06/15/2020 -- README.txt file creation (PV)
07/01/2021 -- update (AR)
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

RWLMA => hemispherically symmetric continental configuration with one continent from pole-to-pole
POLE => hemispherically asymmetric continental configuration with one continent in the NH

(1a) INITIAL SPINUP

The initial, 1st-stage closed system spin-up needs 20 kyr to reach equilibrium:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.orbits MS/vervoortetal.2020 RWLMA834_clim_bio.SPIN1 20000
./runmuffin.sh muffin.eb_go_ac_bg_sg_rg_gl._poleland.orbits MS/vervoortetal.2020 POLE834_clim_bio.SPIN1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage (accelerated) open system spin-up requires a long spinup time of 1.75 Myr to reach equilibrium

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.orbits MS/vervoortetal.2020 RWLMA834_clim_bio_sed_we.SPIN2gl 1750000 RWLMA834_clim_bio.SPIN1
./runmuffin.sh muffin.eb_go_ac_bg_sg_rg_gl._poleland.orbits MS/vervoortetal.2020 POLE834_clim_bio_sed_we.SPIN2gl 1750000 POLE834_clim_bio.SPIN1


### model experiments -- main ensemble ##########################

Below the commands to run the experiments with astronomical forcing for 4 Myr
Experiment 0-5 correspond to the numbering as described in the manuscript

(2a) For EXPERIMENT 0, no carbon cycle feedbacks:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.orbits MS/vervoortetal.2020 RWLMA834_noclim.4Ma 4000000 RWLMA834_clim_bio_sed_we.SPIN2gl
./runmuffin.sh muffin.eb_go_ac_bg_sg_rg_gl._poleland.orbits MS/vervoortetal.2020 POLE834_noclim.4Ma 4000000 POLE834_clim_bio_sed_we.SPIN2gl

(2b) For EXPERIMENT 1, climate + ocean circulation + CO2-ocean chemistry feedbacks only:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.orbits MS/vervoortetal.2020 RWLMA834_clim.4Ma 4000000 RWLMA834_clim_bio_sed_we.SPIN2gl
./runmuffin.sh muffin.eb_go_ac_bg_sg_rg_gl._poleland.orbits MS/vervoortetal.2020 POLE834_clim.4Ma 4000000 POLE834_clim_bio_sed_we.SPIN2gl

(2c) For EXPERIMENT 2, EXPERIMENT 1 + primary productivity feedbacks :

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.orbits MS/vervoortetal.2020 RWLMA834_clim_bio.4Ma 4000000 RWLMA834_clim_bio_sed_we.SPIN2gl
./runmuffin.sh muffin.eb_go_ac_bg_sg_rg_gl._poleland.orbits MS/vervoortetal.2020 POLE834_clim_bio.4Ma 4000000 POLE834_clim_bio_sed_we.SPIN2gl

(2d) For EXPERIMENT 3, EXPERIMENT 2 + CaCO3 sedimentation feedbacks :

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.orbits MS/vervoortetal.2020 RWLMA834_clim_bio_sed.4Ma 4000000 RWLMA834_clim_bio_sed_we.SPIN2gl
./runmuffin.sh muffin.eb_go_ac_bg_sg_rg_gl._poleland.orbits MS/vervoortetal.2020 POLE834_clim_bio_sed.4Ma 4000000 POLE834_clim_bio_sed_we.SPIN2gl

(2e) For EXPERIMENT 4, EXPERIMENT 3 + weathering feedbacks :

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl._rwlma.orbits MS/vervoortetal.2020 RWLMA834_clim_bio_sed_we.4Ma 4000000 RWLMA834_clim_bio_sed_we.SPIN2gl
./runmuffin.sh muffin.eb_go_ac_bg_sg_rg_gl._poleland.orbits MS/vervoortetal.2020 POLE834_clim_bio_sed_we.4Ma 4000000 POLE834_clim_bio_sed_we.SPIN2gl


################################################################
################################################################
################################################################

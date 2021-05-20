################################################################
### readme.txt #################################################
################################################################

For:
'Inclusion of a suite of weathering tracers in the cGENIE Earth System Model - muffin release v.0.9.23'
Markus Adloff, Andy Ridgwell, Fanny M. Monteiro, Ian J. Parkinson, Alexander Dickson, PhilipA. E. Pogge von Strandmann, Matthew Fantle, and Sarah E. Greene

################################################################
10/07/2020 -- README.txt file creation (M.A.)
30/03/2021 -- Added details for sensitivity studies (M.A.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### model experiments -- spinups ###############################

All experiments by default are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the spinups are listed as follows:

(1a) INITIAL SPINUP

The initial, 1st-stage closed system spin-up as detailed in the Methods section of the paper:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage open system and accelerated spin-up:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_noRCO2 500000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN1

(1c) Third Stage SPINUPs

The follow-on, 3rd-stage open system and accelerated spin-ups with metal isotopes:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES 15000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_noRCO2

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_TUNED 15000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_noRCO2

### model experiments -- pulsed C injection ####################

For transient simulations of the response to a sudden C injection:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 Transient_1000PgC 15000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 Transient_5000PgC 15000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_FLUXES

### model experiments -- sensitivity studies ###################

We ran the following sensitivity experiments for indivitual metal cycles:

(2a) Osmium -- 2D weathering

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_GEMCO2 500000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN1

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_GEMCO2 5000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_GEMCO2

(2b) Osmium -- watercolumn scavenging

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_Osscav 5000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_noRCO2

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 Transient_1000PgC_Osscav 200000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_Osscav

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 Transient_5000PgC_Osscav 200000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_Osscav

(2c) Lithium -- climate effect on terrestrial clay formation

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_WDLi 5000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_noRCO2

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 Transient_1000PgC_WDLi 10000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_WDLi

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASEOsSrLiCa MS/adloffetal.GMD.2021 Transient_5000PgC_WDLi 10000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_WDLi

(2d) Calcium -- different fractionation schemes for biogenic carbonate production

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_CaA 20000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_noRCO2

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.worbe2.BASE MS/adloffetal.GMD.2021 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN3gl_CaB 20000000 spinup.worbe2.RidgwellHargreaves1997_S36x36.SPIN2gl_noRCO2

### addition of an updated reef mask for the present-day #######

An updated reef mask for the present-day ocean is provided, based on ETOPO5 bathymetry dataset. A detailed description and test configurations are described in GRID.txt.

################################################################

Please contact the authors for questions about the model parameter file configurations.

################################################################
################################################################
################################################################

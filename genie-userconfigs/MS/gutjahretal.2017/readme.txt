################################################################
### readme.txt #################################################
################################################################

For:
'Massive release of mostly volcanic carbon during the Paleocene-Eocene Thermal Maximum'
Marcus Gutjahr, Andy Ridgwell, Philip F. Sexton, Eleni Anagnostou, Paul N. Pearson, Heiko Pälike, Richard D. Norris, Ellen Thomas and Gavin L. Foster

################################################################
17/06/26 -- README.txt file creation (A.R.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### code version ###############################################

The specific svn version of the code that runs the experiments, is:

r.9984

(but the expectation is that the current (head) version will also always replicate the published results)

### model experiments -- spinups ################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The commands to run the spinups are listed as follows:

(1a) INITIAL SPINUP

The initial, 1st-stage closed system spin-up (e.g. see Ridgwell and Hargreaves [2007]):

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES MS/gutjahretal.2017 SPIN1 20000

(1b) Second Stage SPINUP

The follow-on, 2nd-stage open system and accelerated spin-up (e.g. see Ridgwell and Hargreaves [2007] and Lord et al. [2015]):

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 SPIN2gl 200000 SPIN1

### model experiments -- main (no Corg burial) #################

NB. named as per Extended Data Figure 10 (Table a) and in the order that they appear in the Table.

These are run on from the end of the (2nd stage) spinup as follows:

-------------------------------------------
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_HI 500000 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm 500000 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_LO 500000 SPIN2gl
-------------------------------------------
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.FEsm_HI 500000 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.FEsm 500000 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.FEsm_LO 500000 SPIN2gl
-------------------------------------------
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07rw 500000 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.FErw 500000 SPIN2gl
-------------------------------------------
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_noW 500000 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.FEsm_noW 500000 SPIN2gl
-------------------------------------------

### model experiments -- main (Corg burial) ####################

The Corg burial experiments listed in the Table (EXP.R07sm_Corg_HI, EXP.R07sm_Corg, EXP.R07sm_Corg_LO), 
differ from the non Corg burial experiments in that they consist of 2 segments:
(1) the onset, with no Corg burial
(2) the plateau and recovery, with Corg burial prescribed (to track the d13C recovery)
The first segment is hence the same as per EXP.R07sm_HI, EXP.R07sm, EXP.R07sm_LO.

To create the first segment:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_HI_Corg1 72600 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_Corg1 72600 SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_LO_Corg1 72600 SPIN2gl

and the 2nd segment:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_HI_Corg2 227400 EXP.R07sm_HI_Corg1
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_Corg2 227400 EXP.R07sm_Corg1
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 EXP.R07sm_LO_Corg2 227400 EXP.R07sm_LO_Corg1

The 2 segments of each HI/mean/LO configuration are stitched together to create a continue 300,000 year long experiment and then processed.

### model experiments -- sensitivity tests #####################

NB. as per Extended Data Figure 10 (Table b).

Experiment ID   |   duratrion of onset (yr)
-------------------------------------------
SENS.000100yr   |       100
SENS.000200yr   |       200
SENS.000500yr   |       500
SENS.001000yr   |     1,000
SENS.002000yr   |     2,000
SENS.005000yr   |     5,000
SENS.010000yr   |    10,000
SENS.020000yr   |    20,000

These are run on from the end of the (2nd stage) spinup as follows:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/gutjahretal.2017 SENS.??????yr 200000 SPIN2gl

where SENS.??????yr is one of the experiment IDs listed above.

################################################################
################################################################
################################################################

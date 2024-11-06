================================================================
=== readme.txt =================================================
================================================================

Provided are as part of the code release the configuration files necessary to run the key model experiments presented in the paper.
The intention is to provide an oppertunity to question the paper assumptions and interpretation through re-analysis,
as well as the creation of new and different experiments. (Plus, to provide a means to replicate published results.)
This readme file details how the experiments can be run.
Refer to the muffin manual:
https://github.com/derpycode/muffindoc
for details on model code installation and configuration, locating and visualizing model results, etc.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PUBLICATION DETAILS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Si, W., Herbert, T., Wu, M., & Rosenthal, Y. (2023). 
Increased biogenic calcification and burial under elevated pCO2 during the Miocene: A model-data comparison. 
Global Biogeochemical Cycles, 37, e2022GB007541. https://doi.org/10.1029/2022GB007541

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/03/06 -- README.txt file created by [AR]

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The different model experiments are summerized in Table 1 of Si et al. [2022].
The experiment user-configs are:

A control experiment, which is a pre-industrial configuration of the model, with 1.0x weathering fluxes and 1.0x carbonate production:
muffin.CB.umQ00p0a_sg_rg_gl_P

At 600 ppm CO2 in the atmosphere, there are experiments with permutations of weathering fluxes and carbonate production:

							1.0x carbonate production			2.0x carbonate production
1.0x weathering flux  ::	muffin.CB.umQ12p5a_sg_rg_gl_Ma		muffin.CB.umQ12p5a_sg_rg_gl_Mc
1.3x weathering flux  ::	muffin.CB.umQ12p5a_sg_rg_gl_Mb		muffin.CB.umQ12p5a_sg_rg_gl_Md
1.45x weathering flux ::	---									muffin.CB.umQ12p5a_sg_rg_gl_Me

And at 1120 ppm CO2 in the atmosphere, a set of comparable parameter permutations:

							1.0x carbonate production			2.0x carbonate production
1.0x weathering flux  ::	muffin.CB.umQ12p5a_sg_rg_gl_Ma+		muffin.CB.umQ12p5a_sg_rg_gl_Mc+
1.3x weathering flux  ::	muffin.CB.umQ12p5a_sg_rg_gl_Mb+		muffin.CB.umQ12p5a_sg_rg_gl_Md+
1.45x weathering flux ::	---									muffin.CB.umQ12p5a_sg_rg_gl_Me+

Each experiment is run in 2 stages:
1. An initial 500 kyr long spin-up under an accelerated geochemical mass balance.
2. A final 80 kyr long experiment to fully bring the geological carbon cycle to steady state.

The initial spin-up configurations use the same naming convention but ending in 'SPIN'.
The .SPIN configurations differ only in the specification of the use of an accelerated geochemical mass balance:
Under '# --- GEOCHEM ACCELERATION', the following lines are uncommented to achieve this:

gl_ctrl_update_pCO2=.true.
ma_gem_notyr_min=10
ma_gem_notyr_max=10
ma_gem_yr_min=990
ma_gem_yr_max=990
ma_gem_dyr=0
ma_gem_adapt_auto=.false. 

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The initial, 500 kyr spin-ups are run as follows.

For the preindustrial control:

./runmuffin.sh muffin.CB.umQ00p0a_sg_rg_gl.BASES PUBS/published/Si_et_al.2022 muffin.CB.umQ00p0a_sg_rg_gl_P.SPIN 500000

and for the Miocene configurations at 600 ppm:

./runmuffin.sh muffin.CB.umQ12p5a_sg_rg_gl.BASES PUBS/published/Si_et_al.2022 muffin.CB.umQ12p5a_sg_rg_gl_Mxxx.SPIN 500000

where xxx is one of the letters 'a' through 'e', with (meaning 1120 ppm CO2) or without (600 ppm) a '+' -- see above.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The 80 kyr long continuing experiments are then run:

./runmuffin.sh muffin.CB.umQ00p0a_sg_rg_gl.BASES PUBS/published/Si_et_al.2022 muffin.CB.umQ00p0a_sg_rg_gl_P 80000 muffin.CB.umQ00p0a_sg_rg_gl_P.SPIN 

./runmuffin.sh muffin.CB.umQ12p5a_sg_rg_gl.BASES PUBS/published/Si_et_al.2022 muffin.CB.umQ12p5a_sg_rg_gl_Mxxx 80000 muffin.CB.umQ12p5a_sg_rg_gl_Mxxx.SPIN 

where again, xxx is one of the letters 'a' through 'e', with (meaning 1120 ppm CO2) or without (600 ppm) a '+'.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

################################################################
### README.txt #################################################
################################################################

For:
'The Atmospheric Bridge Communicated the δ13C Decline during the Last Deglaciation to the Global Upper Ocean'
Jun Shao, Lowell D. Stott, Laurie Menviel, Andy Ridgwell, Malin Ödalen, Mayhar Mohtadi

################################################################
2021/06/04 -- README.txt file created
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments (including those in SI).

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################################################################
### model experiments -- baseline run ##########################
################################################################

For the initial 10,000 year spinup:

./runmuffin.sh muffin.CBSR.p_worjh2.BASES_nosed_tracers MS/shaoetal.2021 SPIN.worjh2.Fe.preAge.LGM_Csoft 10000

The deglacial transient (shown in Figure 4,5 and S8) is then simulated in 4 different deglacial experiment stages (i-iv):

--- (i) ---

./runmuffin.sh muffin.CBSR.p_worjh2.BASES_nosed_tracers MS/shaoetal.2021 LGM.SOsalt0NAbluesalt-0.05_184_174 1000 SPIN.worjh2.Fe.preAge.LGM_Csoft

which spans 18.4-17.4 ka of the deglacial transient simulation, with -0.05 Sv of salt flux into the North Atlantic.
This uses the initial spin-up, SPIN.worjh2.Fe.preAge.LGM_Csoft, as a restart.

--- (ii) ---

./runmuffin.sh muffin.CBSR.p_worjh2.BASES_nosed_SO10_tracers MS/shaoetal.2021 LGM.SOsalt0.05wind10NAbluesalt-0.1_174_164 1000 LGM.SOsalt0NAbluesalt-0.05_184_174

which spans 17.4-16.4 ka of the deglacial transient simulation, with -0.1 Sv of salt flux into the North Atlantic, 0.05 Sv of salt flux into the Southern Ocean, 
10% enhanced wind stress over the Southern Ocean. 
This uses LGM.SOsalt0NAbluesalt-0.05_184_174 as a restart.

--- (iii) ---

./runmuffin.sh muffin.CBSR.p_worjh2.BASES_nosed_SO30_tracers MS/shaoetal.2021 LGM.SOsalt0.1wind30NAbluesalt-0.1_164_161 300 LGM.SOsalt0.05wind10NAbluesalt-0.1_174_164

16.4-16.1 ka of the deglacial transient simulation, with -0.1 Sv of salt flux into the North Atlantic, 0.1 Sv of salt flux into the Southern Ocean,
30% enhanced wind stress over the Southern Ocean.
This uses LGM.SOsalt0.05wind10NAbluesalt-0.1_174_164 as a restart.

--- (iv) ---

./runmuffin.sh muffin.CBSR.p_worjh2.BASES_nosed_SO10_tracers MS/shaoetal.2021 LGM.SOsalt0.1wind10NAbluesalt-0.1_161_15 1100 LGM.SOsalt0.1wind30NAbluesalt-0.1_164_161

16.1-15 ka of the deglacial transient simulation, with -0.1 Sv of salt flux into the North Atlantic, 0.1 Sv of salt flux into the Southern Ocean,
10% enhanced wind stress over the Southern Ocean.
This uses LGM.SOsalt0.1wind30NAbluesalt-0.1_164_161 as a restart.

################################################################
### model experiments -- sensitivity tests #####################
################################################################

Two different 10,000 year spin-ups are used for the tracer sensitivity tests:

(1) standard configuration

./runmuffin.sh muffin.CB.p_worjh2.BASEScol023789 MS/shaoetal.2021 muffin.CB.p_worjh2.BASEScol023789.SPIN 10000

(2) no CaCO3 configuration (to remove the contribution to d13C(DIC) of d13C(CaCO3))

./runmuffin.sh muffin.CB.p_worjh2.BASEScol023789 MS/shaoetal.2021 muffin.CB.p_worjh2.BASEScol023789.noCaCO3.SPIN 10000

Each includes 6 different diagnostic tracers (indicated by the 'col023789' bit):

#0 -- preformed DIC
#2 -- preformed O2
#3 -- preformed PO4
#7 -- preformed DIC(d13C)
#8 -- Csoft d13C
#9 -- Csoft

For each of the 2 configurations, a control experiment, and a climate perturbation experiment are run.
The control (CTRL) experiemnt is used in the analysis of the steady-state AOU-based d13C evaluation.
The perturbation experiment is identical, except radiative forcing (TOA) equivalent to a doubling of pCO2, is applied.
All experiments are run for 2,000 years and follow on from their corresponding restart.
 
./runmuffin.sh muffin.CB.p_worjh2.BASEScol023789 MS/shaoetal.2021 muffin.CB.p_worjh2.BASEScol023789.CTRL 2000 muffin.CB.p_worjh2.BASEScol023789.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASEScol023789 MS/shaoetal.2021 muffin.CB.p_worjh2.BASEScol023789.noCaCO3.CTRL 2000 muffin.CB.p_worjh2.BASEScol023789.noCaCO3.SPIN

(controls)

./runmuffin.sh muffin.CB.p_worjh2.BASEScol023789 MS/shaoetal.2021 muffin.CB.p_worjh2.BASEScol023789.EXPT 2000 muffin.CB.p_worjh2.BASEScol023789.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASEScol023789 MS/shaoetal.2021 muffin.CB.p_worjh2.BASEScol023789.noCaCO3.EXPT 2000 muffin.CB.p_worjh2.BASEScol023789.noCaCO3.SPIN

(perturbations)

################################################################
################################################################
################################################################

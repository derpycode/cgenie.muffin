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
PUBLICATION DETAILS [summary of manuscript/publication]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[PAPER TITLE]
Past foraminiferal acclimation capacity is limited during future warming

[AUTHOR LIST]
Rui Ying1*, Fanny M. Monteiro2, Jamie D. Wilson3, Malin Ödalen4,5, Daniela N. Schmidt1

1 School of Earth Sciences, University of Bristol, Bristol, UK
2 School of Geographical Sciences, University of Bristol, Bristol, UK
3 Department of Earth, Ocean and Ecological Sciences, University of Liverpool, Liverpool, UK
4 GEOMAR Helmholtz Centre for Ocean Research Kiel, Kiel, Germany
5 Department of Meteorology, Stockholm University

*Correspondence: rui.ying@bristol.ac.uk

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/04/01 -- README.txt file created by RY

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
1. Last Glacial Maximum spin up (Fig. 1), adapted from Odalen et al. 2022 with ECOGEM enabled
2. Pre-industrial spin up (Fig. 1), same physical configuration as Ward et al. 2018 (ECOGEM 1.0)
3. Historical transient simulation (until 2022) (Fig. 3), running based on preindustrial spinup using historical CO2 forcing
4. Idealised future simulation (2100) with 1.5, 2, 3, 4 degree C warming (Fig. 3)

Other important files:
8P7Z4F.zoo: plankton functional type configuration
FORAMECOGEM.zoo: the explicit trait parameters of foraminifera

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Main experiments
1. pre-industrial spinup (0-10000)
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.SPIN 10000


2. Historical (1765-2022)
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.historical 257 muffin.CBE.worlg4.BASESFeTDTL.SPIN `

3. Future (2022-2100)
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.2100.[XXX]deg 78 muffin.CBE.worlg4.BASESFeTDTL.historical


4. LGM spin up
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.GIteiiva.BASESFeTDTL_rb FORAMECOGEM muffin.CBE.GIteiiva.BASESFeTDTL_rb.SPIN 10000


# SI model runs
1. LGM spinup with seasonal output: muffin.CBE.GIteiiva.BASESFeTDTL_rb.seasonal (same command of running as above)
2. Future warming under different rates: muffin.CBE.worlg4.BASESFeTDTL.{2100, 3000, 4000, 5000}.4deg
3. one might also reproduce the carbon uptake flux and calculate turnover time by adding 'eg_eco_uptake_fluxes=.true.'

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================



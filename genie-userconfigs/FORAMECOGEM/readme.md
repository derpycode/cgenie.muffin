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
Marine zooplankton acclimated to warming in the geological record face limits by the next century

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
1. Last Glacial Maximum spin up
2. Pre-industrial spin up
3. Historical simulation (until 2022)
4. Idealised future simulation (2100) with 1.5, 2, 3, 4 degree C warming


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Model Runs
## pre-industrial spinup (0-10000)

```
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.SPIN 10000
```

## Historical (1765-2022) 

```sh
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.historical 257 muffin.CBE.worlg4.BASESFeTDTL.SPIN
```

## Future (2022-2100)

```sh
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL FORAMECOGEM muffin.CBE.worlg4.BASESFeTDTL.2100.[XXX]deg 78 muffin.CBE.worlg4.BASESFeTDTL.historical
```

## LGM

```sh
qsub -j y -o cgenie_log -V -S /bin/bash runmuffin.sh muffin.CBE.GIteiiva.BASESFeTDTL_rb FORAMECOGEM muffin.CBE.GIteiiva.BASESFeTDTL_rb.SPIN 10000
```


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================



================================================================
=== readme.txt =================================================
================================================================

Provided are as part of the code release the configuration files necessary to run the key model experiments presented in the paper.
The intention is to provide an oppertunity to question the paper assumptions and interpretation through re-analysis,
as well as the creation of new and different experiments. (Plus, to provide a means to replicate published results.)
This readme file details how the experiments can be run.
Refer to the mufifn manual:
https://github.com/derpycode/muffindoc
for details on model code installation and configuration, locating and visualizing model results, etc.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PUBLICATION DETAILS [summary of manuscript/publication]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Sensitivity of ocean circulation to warming during the Early Eocene

Sandra Kirtland Turner, Andy Ridgwell, Allison Keller, Max Vahlenkamp, Adam Aleksinksi1, Philip F. Sexton, Don Penman, Pincelli Hull, Richard D. Norris

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2023/07/14 -- README.txt file created by AR

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Three main model experiments were run:

(i)     An imposed carbon isotopic excursion in the atmosphere of -1 o/oo occurring linearly over 10 kyr, and then recovering linearly over the subsequent 30 kyr.
        The results of this are shown in Figures 3 and 4.
        This is experiment: muffin.p0055c.CIE1.EXPT
(ii)    As per (i), but with fixed (x3) radiative forcing and hence only the carbon cycle (and not climate) changes
        The results of this are shown in the SI.
        This is experiment: muffin.p0055c.CIE1.NOradfor.EXPT
(iii)   As per (i), but with only radiative forcing and climate changing, following the climate change of (i). No carbon emisisons are imposed.
        The results of this are shown in the SI.
        This is experiment: muffin.p0055c.CIE1.NOemissions.EXPT

All 3 experiments are run for a total of 50 kyr following from a spinup (experiment: muffin.p0055c.closed.SPIN).

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

To run the spinup (10 kyr total):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/submitted/KirtlandTurner_et_al.2023 muffin.p0055c.closed.SPIN 10000

Then to run the main experiment (i) from the spuinup (50 kyr total):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/submitted/KirtlandTurner_et_al.2023 muffin.p0055c.CIE1.EXPT 50000 muffin.p0055c.closed.SPIN

Additional experiment (ii) is run similarly:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/submitted/KirtlandTurner_et_al.2023 muffin.p0055c.CIE1.NOradfor.EXPT 50000 muffin.p0055c.closed.SPIN

However, experiment (iii) requires (conservative) atmospheric CH4 tracers to force climate without carbon emissions 
and hence a different base-config (cgenie.eb_go_gs_ac_bg.p0055c.BASESCH4rb):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESCH4rb PUBS/submitted/KirtlandTurner_et_al.2023 muffin.p0055c.CIE1.NOemissions.EXPT 50000 muffin.p0055c.closed.SPIN

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================
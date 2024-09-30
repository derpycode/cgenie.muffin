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

Extreme ocean methane cycling during the early Phanerozoic

Nathan L. Marshall1, Megan Rohrssen1,2, Andy Ridgwell1, Alexandre Pohl3, Bradley D. Cramer4, Woodward W. Fischer5, Olle Hints6, Chris Holmden7, Pavel Kabanov8, Alison Kuhl9, Carina Lee1,10, Michael J. Melchin11, Christopher T. Reinhard12, Richard D. Pancost13, Gordon D. Love1

1Department of Earth Sciences, University of California, Riverside, CA, 92521, USA.
2Department of Earth and Atmospheric Sciences, Central Michigan University, Mt. Pleasant, MI 48859, USA.
3Biogéosciences, UMR 6282 CNRS, Université de Bourgogne, 6 Boulevard Gabriel, 21000 Dijon, France.
4Department of Earth and Environmental Sciences, University of Iowa, Iowa City, IA 52242, USA.
5Division of Geological & Planetary Sciences, California Institute of Technology, Pasadena, CA 91125, USA.
6Department of Geology, Tallinn University of Technology, Ehitajate tee 5, 19086 Tallinn, Estonia.
7Department of Geological Sciences, University of Saskatchewan, Saskatoon, Saskatchewan S7N 5E2, Canada.
8Geological Survey of Canada Calgary, Calgary, AB T2L 2A7, Canada.
9School of Chemistry, University of Bristol, Cantock’s Close, Bristol BS8 1TS, UK.
10Texas State University, Jacobs JETS II, NASA Johnson Space Center, Houston, TX 77058, USA.
11Department of Earth Sciences, St. Francis Xavier University, Antigonish, NS, Canada B2G 2W5.
12School of Earth and Atmospheric Sciences, Georgia Institute of Technology, Atlanta, GA, 30332, USA.
13School of Chemistry, School of Earth Sciences, Cabot Institute for the Environment, University of Bristol, BS8 1TS Bristol, UK.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2023/12/05 -- README.txt file created by [AR]
2024/02/20 -- updated for submisson

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

As per (loosly paraphrasing) Methods:

The model is configured with 29 different sets of boundary conditions (continental distribution and ocean bathymetry, wind velocity and stress, planetary albedo, and greenhouse gas forcing). For 20 through 540 Ma (at 20 Myr intervals) these boundary conditions follow Pohl et al. [2022]. Our 0 Ma (present-day) configuration follows Cao et al. [2009] while 635 Ma (Cryogenian) follows Reinhard et al. [2020] , with the exception that we adjust (via pCO2) the annual mean global ocean surface temperature to match that of the 20 through 540 Ma time-slices (Pohl et al. [2022]). The representation of ocean-atmosphere biogeochemical cycling (of carbon (and isotopes), alkalinity, phosphate, oxygen, sulphur (SO4 and H2S), and CH4 (and isotopes)) follows Reinhard et al. [2020]. For each of the total 29 continental configurations we varied the assumed value of atmospheric pO2 and ocean SO4 concentration in the range ×0.2, ×0.5, and ×1.0 the respective modern (0.21 atm for pO2, and 28 mmol kg-1 for [SO4]) in all 9 permutations. Each modern ensemble member (9 redox permutations of each of 29 different time-slices) was run for 10,000 years, and we report the annual average from the last year of each simulation. In a second series of ensembles, rather than adopt the default vertical depth scale of remineralization of particulate organic matter (590 m (Cao et al. [2009])), we halved it (295 m) to represent a potential earlier Earth, ocean biogeochemical state characterized by shallower recycling of nutrients and carbon and the Mesozoic advent of planktic biomineralization (and increasingly complex ecosystem processing of primary production) (Meyer et al. [2016 ]; Lu et al. [2018 ]).

All the base-config and boundary configuration files required are included as part of the code release.

*** ensemble series #1 ***

User-configuration files are provided (this directory) for all 29 different time-slices, but all for modern pO2 and ocean SO4, and for the default POM reminerilization depth (590 m).

To change any one time-slice to represent a different ensemble member -- at the end of the user-config file, edit the parameters lines:

# atmospheric pO2 (atm) -- modern default = 0.21
ac_atm_init_6=0.210
# ocean SO4 (mol kg-1) -- modern default = 0.28
bg_ocn_init_38=0.028

The published ensemble members multiple these values by 0.2, 0.5, or 1.0, in all (9) permutations.

The results of these experiments are shown in Figure 2.

*** ensemble series #2 ***

To generate the 2nd series of time-slices and redox parameter ensembles, simply add the following line at the end:

# depth of remineralization of particulate organic matter (m)
bg_par_bio_remin_POC_eL1=294.9726

(which over-rides a setting earlier in the file)

Note that only a small sub-set fo the entire ensemble series #2 is shown in Figure 2.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

To run any of the 20 Ma through 540 Ma experiments:

./runmuffin.sh muffin.CB.???rdP1_.BASESCH4 PUBS/submitted/Marshall_et_al.2024 muffin.CB.BASESCH4.xxRF.??? 10000

where ??? is the code for the time-slice and is one of:

000
020
040 
...
500
520
540

For modern (O Ma), the command is:

./runmuffin.sh muffin.CB.p_worjh2.BASESCH4 PUBS/submitted/Marshall_et_al.2024 muffin.CB.BASESCH4.xxRF.000 10000

and for 635 Ma:

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/Marshall_et_al.2024 muffin.CB.BASESCH4.xxRF.635 10000

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================
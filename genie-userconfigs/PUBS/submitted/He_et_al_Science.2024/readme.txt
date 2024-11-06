================================================================
=== readme.txt =================================================
================================================================

Provided are as part of the code release the configuration files necessary to run the key model experiments presented in the paper.
The intention is to provide an opportunity to question the paper assumptions and interpretation through re-analysis,
as well as the creation of new and different experiments. (Plus, to provide a means to replicate published results.)
This readme file details how the experiments can be run.
Refer to the muffin manual:
https://github.com/derpycode/muffindoc
for details on model code installation and configuration, locating and visualizing model results, etc.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PUBLICATION DETAILS [summary of manuscript/publication]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

A reversed latitudinal ocean oxygen gradient in the Proterozoic 

*Ruliang He1*†, Alexandre Pohl2*†, Xingliang Zhang1*, Chao Chang1, Ashely Prow3,
Jonathan L. Payne4, Shuhai Xiao5, Andy Ridgwell6, Zunli Lu3*

1 State Key Laboratory of Continental Dynamics, Shaanxi Key Laboratory of
Early Life and Environments, Department of Geology, Northwest University,
Xi’an 710069, PRC
2 Biogéosciences, UMR 6282 CNRS, Université de Bourgogne, 6 boulevard Gabriel,
21 000 Dijon, France
3 Department of Earth and Environmental Sciences, Syracuse University,
Syracuse, NY 13244, USA
4 Department of Earth and Planetary Sciences, Stanford University, Stanford,
CA 94305, USA
5 Department of Geosciences, Virginia Tech, Blacksburg, VA 24061, USA
6 Department of Earth and Planetary Sciences, University of California
Riverside, Riverside, CA 92521, USA
*Corresponding authors. Email: rulianghe@nwu.edu.cn,
alexandre.pohl@u-bourgogne.fr, xzhang69@nwu.edu.cn, zunlilu@syr.edu 
†These authors contributed equally to this work. 

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/08/28 -- readme.txt file created by AP

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Total of 469 experiments used in Figs. 3, S3–S6

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#################### Experiments for Fig. 3 ####################

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_1.0PO4_590m_1298W_rad16 20000

#################### Experiments for Fig. S3 ####################

Same as Fig. 3.

#################### Experiments for Fig. S4 ####################

[panel a]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_1.0PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.25PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.5PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.75PO4_590m_1298W_rad4 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_1.0PO4_590m_1298W_rad4 20000

[panel b]

Same as Fig. 3.

[panel c]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_1.0PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.25PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.5PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.75PO4_590m_1298W_rad32 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_1.0PO4_590m_1298W_rad32 20000

[panel d]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_1.0PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.25PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.5PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.75PO4_590m_1368W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_1.0PO4_590m_1368W_rad16 20000

#################### Experiments for Fig. S5 ####################

[panel a]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_1.0PO4_590m_1298W_rad16 20000

[panel b]

Same as Fig. 3.

[panel c]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_1.0PO4_590m_1298W_rad16 20000

[panel d]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.0025pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.005pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.01pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.025pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.05pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.1pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.2pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_0.5pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.2SO4_1.0pO2_1.0PO4_295m_1298W_rad16 20000

[panel e]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.0025pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.005pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.01pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.025pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.05pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.1pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.2pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_0.5pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_0.5SO4_1.0pO2_1.0PO4_295m_1298W_rad16 20000

[panel f]

./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.0025pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.005pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.01pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.025pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.05pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.1pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.2pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_0.5pO2_1.0PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_0.25PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_0.5PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_0.75PO4_295m_1298W_rad16 20000
./runmuffin.sh muffin.CB.fm0635ca.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.635_1.0SO4_1.0pO2_1.0PO4_295m_1298W_rad16 20000

#################### Experiments for Fig. S6 ####################

[panels a-b]

./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.0025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.0025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.0025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.0025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.005pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.005pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.005pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.005pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.01pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.01pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.01pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.01pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.05pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.05pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.05pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.05pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.1pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.1pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.1pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.1pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.2pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.2pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.2pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.2pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.5pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.5pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.5pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_0.5pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_1.0pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_1.0pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_1.0pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.020rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.020_0.5SO4_1.0pO2_1.0PO4_590m_1298W_rad16 20000

[panels c-d]

./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.0025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.0025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.0025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.0025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.005pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.005pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.005pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.005pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.01pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.01pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.01pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.01pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.05pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.05pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.05pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.05pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.1pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.1pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.1pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.1pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.2pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.2pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.2pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.2pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.5pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.5pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.5pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_0.5pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_1.0pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_1.0pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_1.0pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.100rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.100_0.5SO4_1.0pO2_1.0PO4_590m_1298W_rad16 20000

[panels e-f]

./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.0025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.0025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.0025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.0025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.005pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.005pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.005pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.005pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.01pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.01pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.01pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.01pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.05pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.05pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.05pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.05pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.1pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.1pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.1pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.1pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.2pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.2pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.2pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.2pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.5pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.5pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.5pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_0.5pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_1.0pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_1.0pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_1.0pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.260rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.260_0.5SO4_1.0pO2_1.0PO4_590m_1298W_rad16 20000

[panels g-h]

./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.0025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.0025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.0025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.0025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.005pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.005pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.005pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.005pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.01pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.01pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.01pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.01pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.025pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.025pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.025pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.025pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.05pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.05pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.05pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.05pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.1pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.1pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.1pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.1pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.2pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.2pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.2pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.2pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.5pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.5pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.5pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_0.5pO2_1.0PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_1.0pO2_0.25PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_1.0pO2_0.5PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_1.0pO2_0.75PO4_590m_1298W_rad16 20000
./runmuffin.sh muffin.CB.500rdP1_.BASESCH4 PUBS/submitted/He_et_al_Science.2024 mf.CB.BASESCH4.500_0.5SO4_1.0pO2_1.0PO4_590m_1298W_rad16 20000

[panels i-j]

Same as Fig. 3.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

################################################################
### readme.txt #################################################
################################################################

For: 'Calibration of key temperature-dependent ocean microbial processes in the cgenie.muffin Earth system model'
K.A.Crichton, J.D.Wilson, A.Ridgwell, P.N.Pearson

################################################################
2019/11/27 -- README.txt file creation (K.A.C)
2019/12/01 -- added sub-directories for spin and trans configs
2020/07/14 -- revised naming, additional DOM experiments
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################################################################
### ENSEMBLE model experiments #################################
################################################################

### steady state to Pre-industrial #############################

located in subdirectory: ensemble.spin

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/crichtonetal.GMD.2019/ensemble.spin [letter][no1]_[no2]_worjh2_280ppm_m6_5permil_BASES.SPIN 10000

### transient forcing from Pre-industrial to Present ###########

located in subdirectory: ensemble.trans

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/crichtonetal.GMD.2019/ensemble.trans [letter][no1]_[no2]_worjh2_PD_BASES.TRANS 310 [letter][no1]_[no2]_worjh2_280ppm_m6_5permil_BASES.SPIN

### experiment naming convention ###############################
 
[letter] : Ea1 setting
b	55000
c	60000
d	53000
e	54000
f	56000

[no1] : Frac2 setting
1	0.002
2	0.008
3	0.032

[no2] : Vmax setting
4	4
7	7
10	10

################################################################
### TUNED & DOM sensitivity test experiments ###################
################################################################

In addition to the model ensembles, the paper focusses on a couple of model comparison experiments:

(1) Contrasting the historical glboal warming response of the optimal POM export (nutrient uptake) and POM T-dependent remineralization experiment 
    with the baseline (Cao et l. [2009]) non T-dependent model configuration.
    
(2) The impact of including a temperature-dependency for both the production of DOM as well as the remineralization of DOM.

These experiments, grouped again into spin-ups, and transient experiments, are as follows:

### steady state to Pre-industrial #############################

crichtonetal.CB.SPIN                    --  the baseline (Cao et l. [2009]) non T-dependent model configuration
crichtonetal.CBRU.SPIN                  --  the optimal e2_10 T-dependent  POM export and POM T-dependent remineralization model configuration
crichtonetal.CBRU.DOmTprod.SPIN         --  as crichtonetal.CBRU.SPIN, but adding T-dependent DOM/POM partitioning
crichtonetal.CBRU.DOMTremin.SPIN        --  as crichtonetal.CBRU.SPIN, but adding T-dependent DOM remineralization
crichtonetal.CBRU.DOMTprodTremin.SPIN   --  as crichtonetal.CBRU.SPIN, but adding both T-dependent DOM/POM partitioning and T-dependent DOM remineralization

These experiments are run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/crichtonetal.GMD.2019 crichtonetal.*.SPIN 10000

crichtonetal.*.SPIN is one of the experiment names listed above

### transient forcing from Pre-industrial to Present ###########

crichtonetal.CB.TRANS                   --  as per crichtonetal.CB.SPIN but transiently forced from Pre-industrial to Present
crichtonetal.CBRU.TRANS                 --  as per crichtonetal.CBRU.SPIN but transiently forced from Pre-industrial to Present
crichtonetal.CBRU.DOMTprod.TRANS        --  as per crichtonetal.CBRU.DOmTprod.SPIN but transiently forced from Pre-industrial to Present
crichtonetal.CBRU.DOMTremin.TRANS       --  as per crichtonetal.CBRU.DOMTremin.SPIN but transiently forced from Pre-industrial to Present
crichtonetal.CBRU.DOMTprodTremin.TRANS  --  as per ccrichtonetal.CBRU.DOMTprodTremin.SPIN but transiently forced from Pre-industrial to Present

These experiments are run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASES MS/crichtonetal.GMD.2019 crichtonetal.*.TRANS 310 crichtonetal.*.SPIN

crichtonetal.*.TRANS is one of the experiment names listed above

################################################################
################################################################
################################################################
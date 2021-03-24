################################################################
### README.txt #################################################
################################################################

For:
'Iron and sulphur cycling in the cGEnIE Earth System model – muffin release v.x.x.xx'
S.J. van de Velde, D. Hülse, C.T. Reinhard, A. Ridgwell

################################################################
27/08/2020 -- README.txt file creation (S.J.V.D.V.)
24/03/2021 -- revision for GMD
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################################################################
### model experiments -- baseline run ##########################
################################################################

Results from this experiment are used in Figs. 3, 7-10 and Table 6

Experiment is run from cold for 20000 years and the command 
to run this experiments is as follows:

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021 muffin.CBSR.fe_rw_lo.lowPP_baseline.PARSENS 20000

################################################################
### model experiments -- sensitivity tests #####################
################################################################

All experiments are run from cold for 20000 years and the commands 
to run these experiments are listed as follows:

(1) SAFE global sensitivity analysis

located in subdirectory: PARSENS_SAFER
Results from these experiments are used in Fig. 11

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_SAFER 210319_CBSR.fe_rw_lo.lowPP_1.PARSENS 20000
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_SAFER 210319_CBSR.fe_rw_lo.lowPP_2.PARSENS 20000
.
.
.
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_SAFER 210319_CBSR.fe_rw_lo.lowPP_100.PARSENS 20000

(2) Quantitative sensitivity analysis

located in subdirectory: PARSENS_quantitative
Results from these experiments are used in Fig. 12

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_K0high.PARSENS 20000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_K0low.PARSENS 20000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_Kihigh.PARSENS 20000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_Kilow.PARSENS 20000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_kPyPhigh.PARSENS 20000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_kPyPlow.PARSENS 20000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_kSMIhigh.PARSENS 20000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_quantitative 210319_CBSR.fe_rw_lo.lowPP_kSMIlow.PARSENS 20000 

(3) k_SMI sensitivity analysis

located in subdirectory: PARSENS_kSMI
Results from these experiments are used in Fig. 13

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_kSMI 210319_CBSR.fe_rw_lo.lowPP_kSMIlow_1.PARSENS 20000 
.
.
.
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMD.2021/PARSENS_kSMI 210319_CBSR.fe_rw_lo.lowPP_kSMIlow_8.PARSENS 20000 

################################################################
### model experiments -- modern iron cycle comparison ##########
################################################################

The published 'FeMIP' modern iron cycle [Tagliabue et al., How well do global ocean biogeochemistry models simulate dissolved iron distributions?, GBC DOI: 10.1002/2015GB005289 (2016)]
together with the 2 alternative iron cycle configurations employing the new iron cycle parameterizations found in this paper,
are run as follows.

(a) 'FeMIP'
./runmuffin.sh muffin.CB.p_worjh2.BASESFe.FeMIP MS/vandeveldeetal.GMD.2021 muffin.CB.p_worjh2.BASESFe.FeMIP.SPIN 10000

(b) as per 'FeMIP' (a) -- keeping scavening the same, but replacing the tracers: Fe, FeL, L, with: Fe, Fe2, TDL to introduce Fe2+ oxidation and Fe3+ reduction
./runmuffin.sh muffin.CB.p_worjh2.BASESFeFe2TL MS/vandeveldeetal.GMD.2021 muffin.CB.p_worjh2.BASESFeFe2TL.SPIN 10000

(c) as per (b), but scavening iron as FeOOH, and allowing this to undergo both Dissimilatory iron reduction and Dissimilatory sulphate reduction, and allowing pyrite formation
./runmuffin.sh muffin.CB.p_worjh2.BASESFeFe2TLFeOOH MS/vandeveldeetal.GMD.2021 muffin.CB.p_worjh2.BASESFeFe2TLFeOOH.POM_FeOOHFeS2.SPIN 10000

################################################################
################################################################
################################################################

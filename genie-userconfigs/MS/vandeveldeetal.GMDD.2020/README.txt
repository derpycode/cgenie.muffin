################################################################
### README.txt #################################################
################################################################

For:
'Anoxic iron and sulphur cycling in the cGEnIE Earth System model – muffin release v.x.x.xx'
S.J. van de Velde, D. Hülse, C.T. Reinhard, A. Ridgwell

################################################################
27/08/2020  -- README.txt file creation (S.J.V.D.V.)
20/09/14    -- updated by AR
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################################################################
### model experiments -- baseline run ##########################
################################################################

Results from this experiment are used in Figs. 5-8 and Table 5

Experiment is run from cold for 20000 years and the command 
to run this experiments is as follows:

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020 200401_CBSR.fe_rw_lo.lowPP_baseline.PARSENS 200000

To save time and still be able to retrieve all relevant output, as presented in the paper, the baseline epxeriment
was run with a limited output, and followed by a 10 year run during which all output was saved.
The command to run this experiments is as follows:

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020 200401_CBSR.fe_rw_lo.lowPP_baseline.PARSENS_fulloutput 10 200401_CBSR.fe_rw_lo.lowPP_baseline.PARSENS

################################################################
### model experiments -- sensitivity tests #####################
################################################################

All experiments are run from cold for 20000 years and the commands 
to run these experiments are listed as follows:

(1) SAFE global sensitivity analysis

located in subdirectory: PARSENS_SAFER
Results from these experiments are used in Fig. 9

The initial, 1st-stage closed system spin-up as detailed in the SI (e.g. see Ridgwell and Hargreaves [2007]):

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_SAFER 200401_CBSR.fe_rw_lo.lowPP_1.PARSENS 20000
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_SAFER 200401_CBSR.fe_rw_lo.lowPP_2.PARSENS 20000
.
.
.
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_SAFER 200401_CBSR.fe_rw_lo.lowPP_100.PARSENS 20000


(2) Quantitative sensitivity analysis

located in subdirectory: PARSENS_quantitative
Results from these experiments are used in Fig. 10

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_K0high.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_K0low.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_Kihigh.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_Kilow.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_kPyPhigh.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_kPyPlow.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_kSMIhigh.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_quantitative 200401_CBSR.fe_rw_lo.lowPP_kSMIlow.PARSENS 200000 


(3) k_SMI sensitivity analysis

located in subdirectory: PARSENS_kSMI
Results from these experiments are used in Fig. 11

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_1.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_2.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_3.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_4.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_5.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_6.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_7.PARSENS 200000 
./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_kSMIlow_8.PARSENS 200000 


To save time and still be able to retrieve all relevant output, as presented in the paper, each of these experiments
was run with a limited output, and followed by a 10 year run during which all output was saved.
The command to run these experiments are listed as follows (given is 1 example, all others are identical):

./runmuffin.sh muffin.CBSR.fe_rw_lo.FeS.FeSiso MS/vandeveldeetal.GMDD.2020/PARSENS_kSMI 200401_CBSR.fe_rw_lo.lowPP_1.PARSENS_fulloutput 10 200401_CBSR.fe_rw_lo.lowPP_1.PARSENS

################################################################
################################################################
################################################################

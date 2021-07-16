################################################################
### readme.txt #################################################
################################################################

For: Exploring the impact of climate change on non-spinose planktonic foraminifera global distribution with a trait-based ecosystem model
Maria Grigoratou, Fanny M. Monteiro, Jamie D. Wilson, Andy Ridgwell, Daniela N. Schmidt

################################################################
16/07/2021 -- README.txt file creation
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################## Model Experiments ###########################

The commands to run the two model configurations are listed as follows:

(1) ForamEcoGEnIE 10,000 year pre-industrial spin-up

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/grigoratouetal.GCB.2021 ForamECOGEM.8P7Z1F.Grigoratou 10000

(2) ForamEcoGEnIE historical and future (RCP6) continued from 10,000 year spin up

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/grigoratouetal.GCB.2021 ForECOGEM.8P7Z1F_sigma2_palatability09_RCP6p0 435 ForamECOGEM.8P7Z1F.Grigoratou

(3) ForamEcoGEnIE historical and future (RCP8.5) continued from 10,000 year spin up

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/grigoratouetal.GCB.2021 ForECOGEM.8P7Z1F_sigma2_palatability09_RCP8p5 435 ForamECOGEM.8P7Z1F.Grigoratou

Details for the data meta-analysis, figures and statistical analysis can be found at https://github.com/mariagrigoratou/ForamEcoGEnIE

################################################################
################################################################
################################################################

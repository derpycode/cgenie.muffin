################################################################
### readme.txt #################################################
################################################################

For:  'Positive feedback of marine N2O emissions during past extreme warming'
X. Zhao, F.M. Monteiro , K. Becker , L. Dirksen , J. Cordes , J. Lipp , S.J. Carter , N. Rochelle-Bates , E. Hollingsworth , M. Jones , B. Schultz , A. Dickson , M. Kaya , F. Güldner , E. Stüeken , G. Inglis , R. Pancost , K. Hinrichs , A. Pearson , F. Elling

################################################################
2/11/2026 -- README.txt file creation (X.Z.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################## Model Experiments ###########################

The commands to run the model configurations as listed in the Methods are listed here.

(1) Eocene simulations using Ridgwell and Schmidt (2010) physical forcing (Eocene55c), Reinhard et al. (2020) redox scheme (cgenie.muffin) and Naafs et al. (2019) nitrogen cycle parameters 
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Zhao_et_al.2026 Eocene55c_NT_05P2C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Zhao_et_al.2026 Eocene55c_NT_075P6C 10000

(2) Modern simulation 
./runmuffin.sh muffin.CB.worlg4.BASESFeTDTL_Ncycle PUBS/submitted/Zhao_et_al.2026 preind_NFeT_exp23_Nitri9_muffin 10000

################################################################
################################################################
################################################################

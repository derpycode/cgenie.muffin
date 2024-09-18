################################################################
### readme.txt #################################################
################################################################

For:  'North-East Peri-Tethyan water column deoxygenation and euxinia at the Paleocene Eocene Thermal Maximum'
L. Behrooz, B.D.A. Naafs, K.W.R. Taylor, F.M. Monteiro, A.J. Dickson, A. Pearson and R.D. Pancost

################################################################
09/07/2024 -- README.txt file creation (F.M.M.)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################## Model Experiments ###########################

The commands to run the model configurations as listed in the Methods are listed here.

(1) Eocene simulations using Ridgwell and Schmidt (2010) physical forcing (Eocene55c) and Reinhard et al. (2020) redox scheme (cgenie.muffin) 
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_05P2C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_075P2C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_1P2C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_1.5P2C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_05P6C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_075P6C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_1P6C 10000
./runmuffin.sh muffin.CB.p_p0055c.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene55c_NT_1.5P6C 10000

(2) Eocene simulation using  Remmelzwaal et al. (2019) physical forcing (Eocene52f) and Reinhard et al. (2019) redox scheme (cgenie.muffin)
./runmuffin.sh muffin.CB.p_p0052f.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene52f_muffin_NT_05P6C 10000
./runmuffin.sh muffin.CB.p_p0052f.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene52f_muffin_NT_075P6C 10000
./runmuffin.sh muffin.CB.p_p0052f.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene52f_muffin_NT_1P6C 10000
./runmuffin.sh muffin.CB.p_p0052f.BASESN PUBS/submitted/Behrooz_et_al.Paleoceano.2024 Eocene52f_muffin_NT_1.5P6C 10000

################################################################
################################################################
################################################################

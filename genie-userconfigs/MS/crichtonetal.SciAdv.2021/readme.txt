################################################################
### readme.txt #################################################
################################################################

For:  'Perspectives on the oceans Twilight Zone: the geological past to inform the future'
      'The past and future ocean Twilight Zone'
Katherine A. Crichton, Jamie D. Wilson, Andy Ridgwell, Paul .N. Pearson, Flavia Boscolo-Galazzo, Eleanor John, Bridget S. Wade.

################################################################
16/07/2021 -- README.txt file creation (J.D.W)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################## Model Experiments ###########################

The commands to run the model configurations as listed in the Methods are listed here.
Each experiment is run with two models of the biological pump, e.g.,

Crichtonetal_*_.SPIN - standard biological pump model (Ridgwell et al., 2007, Biogeosciences)
Crichtonetal_*_Tdep.SPIN - tempereature-dependent biological pump model (Crichton et al., 2021, Geoscientific Model Development)

(1) Modern spinups

./runmuffin.sh muffin.CB.p_worjh2.BASES MS/cricthonetal.SciAdv.2021 Crichtonetal_worjh2.SPIN 10000
./runmuffin.sh muffin.CB.p_worjh2.BASES MS/cricthonetal.SciAdv.2021 Crichtonetal_worjh2_Tdep.SPIN 10000

(2) Late Paleocene Early Eocene spinups

./runmuffin.sh muffin.CB.p_p0055c.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_p0055c.SPIN 10000
./runmuffin.sh muffin.CB.p_p0055c.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_p0055c_Tdep.SPIN 10000

(3) Miocene spinups

./runmuffin.sh muffin.CB.umQ15p0a.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_umQ15p0a.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_umQ15p0a_Tdep.SPIN 10000

(4) Future simulations

./runmuffin.sh muffin.CB.p_worjh2.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_worjh2.transient.logistic625.Winkelmann2015 11750 Crichtonetal_worjh2.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_worjh2_Tdep.transient.logistic625.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep.SPIN

./runmuffin.sh muffin.CB.p_worjh2.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_worjh2.transient.logistic2500.Winkelmann2015 11750 Crichtonetal_worjh2.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_worjh2_Tdep.transient.logistic2500.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep.SPIN

./runmuffin.sh muffin.CB.p_worjh2.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_worjh2.transient.logistic5000.Winkelmann2015 11750 Crichtonetal_worjh2.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES MS/crichtonetal.SciAdv.2021 Crichtonetal_worjh2_Tdep.transient.logistic5000.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep.SPIN

################################################################
################################################################
################################################################

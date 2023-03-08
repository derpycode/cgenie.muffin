################################################################
### readme.txt #################################################
################################################################

For:  'What can the geological past tell us about the future of the oceans twilight zone?"

Katherine A. Crichton, Jamie D. Wilson, Andy Ridgwell, Flavia Boscolo-Galazzo, Eleanor H. John, Bridget S. Wade, Paul N. Pearson.

################################################################
16/07/2021 -- README.txt file creation (J.D.W)
06/03/2023 -- updated for final revisions (J.D.W)
2023/03/07 -- all experiments tested; made directory change (AR)
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

./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2.SPIN 10000
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep.SPIN 10000
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep_Tdom.SPIN 10000

(2) Late Paleocene Early Eocene spinups

./runmuffin.sh muffin.CB.p_p0055c.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_p0055c.SPIN 10000
./runmuffin.sh muffin.CB.p_p0055c.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_p0055c_Tdep.SPIN 10000

(3) Miocene spinups

./runmuffin.sh muffin.CB.umQ15p0a.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_umQ15p0a.SPIN 10000
./runmuffin.sh muffin.CB.umQ15p0a.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_umQ15p0a_Tdep.SPIN 10000

(4) Future simulations

./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2.transient.logistic625.Winkelmann2015 11750 Crichtonetal_worjh2.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep.transient.logistic625.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep_Tdom.logistic625.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep_Tdom.SPIN

./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2.transient.logistic2500.Winkelmann2015 11750 Crichtonetal_worjh2.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep.transient.logistic2500.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep_Tdom.logistic2500.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep_Tdom.SPIN

./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2.transient.logistic5000.Winkelmann2015 11750 Crichtonetal_worjh2.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep.transient.logistic5000.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep.SPIN
./runmuffin.sh muffin.CB.p_worjh2.BASES PUBS/published/Crichton_et_al.2023 Crichtonetal_worjh2_Tdep_Tdom.logistic5000.Winkelmann2015 11750 Crichtonetal_worjh2_Tdep_Tdom.SPIN

################################################################
################################################################
################################################################

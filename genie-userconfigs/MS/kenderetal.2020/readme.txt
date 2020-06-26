################################################################
### readme.txt #################################################
################################################################

For:
Palaeocene/Eocene greenhouse gases released after less than 2°C of warming
Sev Kender, Andy Ridgwell, Kara Bogus, Gunver K. Pedersen, Karen Dybkjær, Tamsin A. Mather, Erica Mariani, James B. Riding, Thomas Wagner, Stephen P. Hesselbo, Melanie J. Leng

################################################################
20/04/14 -- README.txt file creation (A.R.)
################################################################

Provided is the code used to create the tuned model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an oppertunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### model experiments ##########################################

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

The model spin-up is conducted in 2 stages:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.SPIN1 20000

will run an initial equilibrium of ocean circulation and climate, plus ocean carbon and nutrient cycling.
 
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.SPIN2gl 100000 muffin.CBS.p0055ce.SPIN1 

then ensures that sediment composition and carbonate burial are at steady state and in equilibrium with terrestrial weathering.
(Note the different base-config.)

For the inversion experiments themselves:

-------------------------------------------
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_RAW_COMPm17m27.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_RAW_COMPm17m60.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_RAW_COMPm35m27.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_RAW_COMPm35m60.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_SM_COMPm17m27.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_SM_COMPm17m60.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_SM_COMPm35m27.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055e.BASES MS/kenderetal.2020 muffin.CBS.p0055ce.DOC_SM_COMPm35m60.EXPT 50000 muffin.CBS.p0055ce.SPIN2gl
-------------------------------------------

The first 4 invert the raw (un-smoothed) observed d13C record, while the second set of 4 invert the smoothed record.
The different d13C souces choices are as follows:

COMPm17m27 == -17 o/oo background d13C emissions; -27 o/oo during PETM onset
COMPm17m60 == -17 o/oo background d13C emissions; -60 o/oo during PETM onset
COMPm35m27 == -35 o/oo background d13C emissions; -27 o/oo during PETM onset
COMPm35m60 == -35 o/oo background d13C emissions; -60 o/oo during PETM onset

### NOTES ######################################################


################################################################
################################################################
################################################################

================================================================
=== readme.txt =================================================
================================================================

Provided are as part of the code release the configuration files necessary to run the key model experiments presented in the paper.
The intention is to provide an oppertunity to question the paper assumptions and interpretation through re-analysis,
as well as the creation of new and different experiments. (Plus, to provide a means to replicate published results.)
This readme file details how the experiments can be run.
Refer to the muffin manual:
https://github.com/derpycode/muffindoc
for details on model code installation and configuration, locating and visualizing model results, etc.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PUBLICATION DETAILS 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Characterizing the marine 1 iodine cycle and its relationship to ocean deoxygenation in an Earth System model
Keyi Cheng1, Andy Ridgwell2, Dalton S. Hardisty1
4 1Department of Earth and Environmental Sciences, Michigan State University, East Lansing, 48823, USA
5 2Department of Earth and Planetary Sciences, University of California Riverside, Riverside, 92521, USA
REVISED for Biogeosciences

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/08/23 -- README.txt file created by AR

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

3 different parameterizations-combinations are presented in the main paper, 
and these are tuned under both GENIE-projected distributions of [O2], and [O2] in the model fored to climatology (WOA).
This results in 6 total different model ensembles.

Provided here are:

(1) The tuned parameter sets of the 3 parameterizations-combinations, and with GENIE-O2 or WOA-O2, for the modern ocean.
(2) The 6 total tuned parameter sets for pre OAE-2 (although in the paper only the WOA-O2 parameter sets are focussed on).

These are all provied in the main directory (Cheng_et_al.Biogeosciences.2024).

Plus:

(3) An additional 2 parameterizations-combinations, and with GENIE-O2 or WOA-O2, for both modern and OAE-2.

These are provied in the sub-directory EXTRA.

NOTE:	the full ensemble of user-configuration files is not provided, only the data-calibrated configurations (e.g., Table 2).

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The format of the filenames is:

muffin.CB.XXX.BASESI.YYY.ZZZ.SPIN

where:

XXX == 	continental configuration, either:
		p_worjh2	==	modern
		p_p0093k	==	Cretaceous
YYY	==	parameterization-combination, one of (see paper):
		lifetime_threshold
		fennel_threshold
		reminO2lifetime_threshold
		(and in SI: lifetime_inhibition, lifetime_reminSO4lifetime)
ZZZ ==	assumed [O2] field, where:
		GENIEO2	==	internally-calculated dissolved O2 field
		WOAO2	==	dissolved O2 field nudged towards re-gridded Word Ocean Atlas O2 climatology
		
NOTE:	modern WOAO2 configurations including a restoring forcing of the 3D distribution of [O2] in the ocean, plus restoring of atmopsheric pCO2 and d13C of pCO2
		modern GENIEO2, and both paleo GENIEO2 and WOAO2 configurations, only restore atmopsheric pCO2 and d13C of pCO2

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

All experiments are run for 10 kyr from cold (no restart).
(Note that this methodlogy is very slightly different from how the full ensembles were generated in the paper, which were run for 2 kyr from a re-start.)

For modern (see above for the explanation of the codes: YYY and ZZZ):

./runmuffin.sh muffin.CB.p_worjh2.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024 muffin.CB.p_worjh2.BASESI.YYY.ZZZ.SPIN 10000

e.g.,

./runmuffin.sh muffin.CB.p_worjh2.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024 muffin.CB.p_p0093k.BASESI.lifetime_threshold.GENIEO2.SPIN 10000
./runmuffin.sh muffin.CB.p_worjh2.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024 muffin.CB.p_p0093k.BASESI.lifetime_threshold.WOAO2.SPIN 10000

which is the first listed tuned configuration in Table 2

For the Cretaceous, the general form of the experiment is:

./runmuffin.sh muffin.CB.p_p0093k.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024 muffin.CB.p_p0093k.BASESI.YYY.ZZZ.SPIN 10000

e.g., 

./runmuffin.sh muffin.CB.p_p0093k.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024 muffin.CB.p_p0093k.BASESI.reminO2lifetime_threshold.GENIEO2.SPIN 10000

which corresponds to the x0.6 ocean PO4, 'reminO2lifetime-threshold' configuration (3rd row) in Figure 8 and the bottom panel in Figure 9.

And for the supplimental parameterizations-combinations, the general form of the experiment (one for modern, one for Cretaceous) is:

./runmuffin.sh muffin.CB.p_worjh2.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024/EXTRA muffin.CB.p_worjh2.BASESI.YYY.ZZZ.SPIN 10000
./runmuffin.sh muffin.CB.p_p0093k.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024/EXTRA muffin.CB.p_p0093k.BASESI.YYY.ZZZ.SPIN 10000

e.g.,

./runmuffin.sh muffin.CB.p_worjh2.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024/EXTRA muffin.CB.p_worjh2.BASESI.lifetime_inhibition.GENIEO2.SPIN 10000
./runmuffin.sh muffin.CB.p_p0093k.BASESI PUBS/published/Cheng_et_al.Biogeosciences.2024/EXTRA muffin.CB.p_p0093k.BASESI.lifetime_inhibition.GENIEO2.SPIN 10000

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

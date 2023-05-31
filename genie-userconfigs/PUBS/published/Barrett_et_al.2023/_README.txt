################################################################
### MODELLING_README.txt #######################################
################################################################

For: Planktic foraminiferal resilience to the environmental change associated with the PETM
Ruby Barrett, Monsuru Adebowale, Heather Birch, Jamie Wilson and Daniela N. Schmidt

################################################################
18/08/2022 -- MODELLING_README.txt file creation (R.E.B)
2023/05/09 -- adjusted dir name; checked experiments run (AR)
################################################################

Provided is the code used to run the model experiments presented in the paper. 

- All experiments are run from:$HOME/cgenie.muffin/genie-main (unless a different installation directory has been used)

* denotes where a split spinup is used. i.e. an experiment with no ecosystem active has been run for 10,000 years first and used as the restart. To complete the spinup, above experiments were run with the ecosystem active for 50 years.
	
** This experiment uses a no ecosystem restart. The experiment is kickstarted at 40,000 years so the ecosystem has 1000 years to adjust before the CIE onset at 41,000. 

################## Model Experiments ###########################

The commands to run the model configurations as in the Methods are:

(1) Late Paleocene Early Eocene * [B]

./runmuffin.sh muffin.CBE.p0055c.BASES PUBS/published/Barrett_et_al.2023 Barrettetal.Eocene.p0055c.8P7Z3F.3x 50 Barrettetal.Eocene.p0055c.3x

(2) PETM ** [B]

./runmuffin.sh muffin.CBE.p0055c.BASES MS/Barrettetal.2022 Barrettetal.PETM.p0055c.8P7Z3F 200000 Barrettetal.Eocene.p0055c.3x 

(3) PETM seperating temperature impact on circulation from physiology ** [B]

./runmuffin.sh muffin.CBE.p0055c.BASES MS/Barrettetal.2022 Barrettetal.PETM.p0055c.8P7Z3F_PETM1SST 200000 Barrettetal.Eocene.p0055c.3x 

############## Supplementary Model Experiments ###############

The commands to run the model configurations as listed in the Supplementary Material are:

(S1) Modern * [A]

./runmuffin.sh muffin.CBE.worjh2.BASES MS/Barrettetal.2022 Barrettetal.Modern.worjh2.8P7Z3F.1x 50 Barrettetal.Modern.worjh2.1x

(S2) Modern with Iron Limitation * [C]

./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/Barrettetal.2022 Barrettetal.Modern.worlg4.8P7Z3F.1x.Fe 50 Barrettetal.Modern.worlg4.1x.Fe

#################### Split Spinup Restarts ####################

The commands to run the restarts which form the base of the above experiments are: 

[A] Modern (No Ecosystem)

./runmuffin.sh muffin.CB.worjh2.BASES MS/Barrettetal.2022 Barrettetal.Modern.worjh2.1x 10000


[B] Late Paleocene Early Eocene (No Ecosystem)

./runmuffin.sh muffin.CB.p0055c.BASES MS/Barrettetal.2022 Barrettetal.Eocene.p0055c.3x 10000


[C] Modern with Iron Limitation (No Ecosystem)

./runmuffin.sh muffin.CB.worlg4.BASESFeTDTL MS/Barrettetal.2022 Barrettetal.Modern.worlg4.1x.Fe 10000

############################# end #############################

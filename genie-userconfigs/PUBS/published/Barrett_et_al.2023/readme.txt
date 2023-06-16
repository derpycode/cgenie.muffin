################################################################
### MODELLING_README.txt #######################################
################################################################

For: Planktic foraminiferal resilience to the environmental change associated with the PETM
Ruby Barrett, Monsuru Adebowale, Heather Birch, Jamie Wilson and Daniela N. Schmidt

################################################################
18/08/2022 -- MODELLING_README.txt file creation (R.E.B)
2023/05/09 -- adjusted dir name; checked experiments run (AR)
2023/06/16 -- revised readme + testing (AR)
################################################################

Provided is the code used to run the model experiments presented in the paper. 

All experiments are run from:$HOME/cgenie.muffin/genie-main (unless a different installation directory has been used).

########################### start ##############################

The suffix SPIN.1 denotes the first stage of the spinup The suffix SPIN.2 denotes the second stage of the spinup The suffix .EXPERIMENT denotes the transient experiments 

################### First stage of spinup #################### The commands to run the first stage of spinup are:

[A] Modern (No Ecosystem)
./runmuffin.sh muffin.CB.worjh2.BASES PUBS/published/Barrett_et_al.2023 Barrettetal.Modern.worjh2.1x.SPIN1 10000

[B] Late Paleocene Early Eocene (No Ecosystem) 
./runmuffin.sh muffin.CB.p0055c.BASES PUBS/published/Barrett_et_al.2023 Barrettetal.Eocene.p0055c.3x.SPIN1 10000

[C] Modern with Iron Limitation (No Ecosystem) 
./runmuffin.sh muffin.CB.worlg4.BASESFeTDTL PUBS/published/Barrett_et_al.2023 Barrettetal.Modern.worlg4.1x.Fe.SPIN1 10000

################## Main Paper Model Experiments  ########### The commands to run the model configurations described in the Methods are:

(1) Late Paleocene Early Eocene [B]
./runmuffin.sh muffin.CBE.p0055c.BASES PUBS/published/Barrett_et_al.2023 Barrettetal.Eocene.p0055c.8P7Z3F.3x.SPIN2 50 Barrettetal.Eocene.p0055c.3x.SPIN1

(2) PETM [B]
./runmuffin.sh muffin.CBE.p0055c.BASES PUBS/published/Barrett_et_al.2023 Barrettetal.PETM.p0055c.8P7Z3F.EXPERIMENT 200000 Barrettetal.Eocene.p0055c.3x.SPIN1 

(3) PETM separating temperature impact on circulation from physiology  [B] 
./runmuffin.sh muffin.CBE.p0055c.BASES PUBS/published/Barrett_et_al.2023 Barrettetal.PETM.p0055c.8P7Z3F_PETM1SST.EXPERIMENT 200000 Barrettetal.Eocene.p0055c.3x.SPIN1 

############## Supplementary Model Experiments ############### The commands to run the model configurations as listed in the Supplementary Material are:

(S1) Modern [A]
./runmuffin.sh muffin.CBE.worjh2.BASES PUBS/published/Barrett_et_al.2023 Barrettetal.Modern.worjh2.8P7Z3F.1x.SPIN2 50 Barrettetal.Modern.worjh2.1x.SPIN1

(S2) Modern with Iron Limitation [C]
./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL PUBS/published/Barrett_et_al.2023 Barrettetal.Modern.worlg4.8P7Z3F.1x.Fe.SPIN2 50 Barrettetal.Modern.worlg4.1x.Fe.SPIN1

############################# end #############################

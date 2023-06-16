=========================== start ==========================

The suffix SPIN.1 denotes the first stage of the spinup The suffix SPIN.2 denotes the second stage of the spinup The suffix .EXPERIMENT denotes the transient experiments 

################### First stage of spinup #################### The commands to run the first stage of spinup are:

[A] Modern (No Ecosystem)
./runmuffin.sh muffin.CB.worjh2.BASES MS/Barrettetal.2022 Barrettetal.Modern.worjh2.1x.SPIN1 10000

[B] Late Paleocene Early Eocene (No Ecosystem) 
./runmuffin.sh muffin.CB.p0055c.BASES MS/Barrettetal.2022 Barrettetal.Eocene.p0055c.3x.SPIN1 10000

[C] Modern with Iron Limitation (No Ecosystem) 
./runmuffin.sh muffin.CB.worlg4.BASESFeTDTL MS/Barrettetal.2022 Barrettetal.Modern.worlg4.1x.Fe.SPIN1 10000

################## Main Paper Model Experiments  ########### The commands to run the model configurations described in the Methods are:

(1) Late Paleocene Early Eocene [B]
./runmuffin.sh muffin.CBE.p0055c.BASES MS/Barrettetal.2022 Barrettetal.Eocene.p0055c.8P7Z3F.3x.SPIN2 50 Barrettetal.Eocene.p0055c.3x.SPIN1

(2) PETM [B]
./runmuffin.sh muffin.CBE.p0055c.BASES MS/Barrettetal.2022 Barrettetal.PETM.p0055c.8P7Z3F.EXPERIMENT 200000 Barrettetal.Eocene.p0055c.3x.SPIN1 

(3) PETM separating temperature impact on circulation from physiology  [B] 
./runmuffin.sh muffin.CBE.p0055c.BASES MS/Barrettetal.2022 Barrettetal.PETM.p0055c.8P7Z3F_PETM1SST.EXPERIMENT 200000 Barrettetal.Eocene.p0055c.3x.SPIN1 

############## Supplementary Model Experiments ############### The commands to run the model configurations as listed in the Supplementary Material are:

(S1) Modern [A]
./runmuffin.sh muffin.CBE.worjh2.BASES MS/Barrettetal.2022 Barrettetal.Modern.worjh2.8P7Z3F.1x.SPIN2 50 Barrettetal.Modern.worjh2.1x.SPIN1

(S2) Modern with Iron Limitation [C]
./runmuffin.sh muffin.CBE.worlg4.BASESFeTDTL MS/Barrettetal.2022 Barrettetal.Modern.worlg4.8P7Z3F.1x.Fe.SPIN2 50 Barrettetal.Modern.worlg4.1x.Fe.SPIN1

=========================== end ========================== 

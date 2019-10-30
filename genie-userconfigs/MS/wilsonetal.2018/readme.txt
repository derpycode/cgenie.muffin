################################################################
### readme.txt #################################################
################################################################

For: ‘Linking marine plankton ecosystems and climate: A new modelling approach to the warm Early Eocene climate.’ 
Jamie D. Wilson, Fanny M. Monteiro, Daniela N. Schmidt, Ben A. Ward and Andy Ridgwell

################################################################
08/11/2018 -- README.txt file creation (J.D.W)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run the model experiments.

All experiments are run from:$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

################## Model Experiments ###########################

The commands to run the model configurations as listed in the Methods are:

(1) Modern 

./runmuffin.sh muffin.CBE.worjh2.BASES MS/wilsonetal.2018 wilsonetal.worjh2.8P8Z.pal.1x 10000

(2) Late Paleocene Early Eocene Paleogeography 

./runmuffin.sh muffin.CBE.p0055c.BASES MS/wilsonetal.2018 wilsonetal.p0055c.8P8Z.pal.1x 10000

(3) Early Eocene CO2 and Climate

./runmuffin.sh muffin.CBE.p0055c.BASES MS/wilsonetal.2018 wilsonetal.p0055c.8P8Z.pal.3x 10000

(4) Temperature Effects of Circulation and Ecology

./runmuffin.sh muffin.CBE.p0055c.BASES MS/wilsonetal.2018 wilsonetal.p0055c.8P8Z.pal.1x.3xSST 5000 wilsonetal.p0055c.8P8Z.pal.1x 

./runmuffin.sh muffin.CBE.p0055c.BASES MS/wilsonetal.2018 wilsonetal.p0055c.8P8Z.pal.3x.1xSST 5000 wilsonetal.p0055c.8P8Z.pal.3x 

################## Supplementary Model Experiments ##############

The commands to run the model configurations as listed in the Supplementary Material are:

(S1) Modern with 3 x CO2

./runmuffin.sh muffin.CBE.worjh2.BASES MS/wilsonetal.2018 wilsonetal.worjh2.8P8Z.pal.3x 10000

(S2) Temperature Effects of Circulation and Ecology for Modern

./runmuffin.sh muffin.CBE.worjh2.BASES MS/wilsonetal.2018 wilsonetal.p0055c.8P8Z.pal.1x.3xSST 5000 wilsonetal.p0055c.8P8Z.pal.1x 

./runmuffin.sh muffin.CBE.worjh2.BASES MS/wilsonetal.2018 wilsonetal.worjh2.8P8Z.pal.3x.1xSST 5000 wilsonetal.worjh2.8P8Z.pal.3x 

(S3) Modern with Iron Limitation

./runmuffin.sh muffin.CBE.worjh2.BASESFeTDTL MS/wilsonetal.2018 wilsonetal.worjh2.8P8Z.pal.1x_Fe 10000

################################################################
################################################################
################################################################


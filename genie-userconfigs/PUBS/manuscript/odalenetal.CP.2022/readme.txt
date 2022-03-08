################################################################
### readme.txt #################################################
################################################################

For:
'Model–based interpretation of ocean d13C from proxy records –- what can we learn about how the ocean changed across the deglaciation?'
Malin Ödalen, Carlye D. Peterson, Andy Ridgwell, Kevin I. C. Oliver, and Paul J. Valdes

################################################################
20/02/24 -- README.txt file creation 
22/03/07 -- copy to new directory
################################################################

Provided are all the configuration files and forcing files necessary to run the model experiments presented in the paper.

The intention is to provide an opportunity to question the assumptions and interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results!)

### base configuration ##########################################

Two different base-config files are used -- one for the late Holocene (HOL) and one for the Last Glacial Maximum (LGM)

These are found in the genie-main/configs directory and configured with a basic set of tracers plus ventillation age:

HOL -- muffin.CB.GIteiiaa.BASESFeTDTL_rb.config
LGM -- muffin.CB.GIteiiva.BASESFeTDTL_rb.config

For configurations using a more complete set of tracers, including 14C plus preformed nutrients, DIC, and d13C:

HOL -- muffin.CB.GIteiiaa.BASESFeTDTL_14Crbcol0137.config
LGM -- muffin.CB.GIteiiva.BASESFeTDTL_14Crbcol0137.config

### model experiments -- tuned ################################

The paper focussed on 2 main model configurations for the HOL and LGM, both of which were tuned against observed distributions of late Holocene and LGM benthic d13C, respectively.

The basic (no 14C, no preformed tracers) experiments can be spun-up as follows (in order: Holocene, the LGM):

./runmuffin.sh muffin.CB.GIteiiaa.BASESFeTDTL_rb PUBS/manuscript/odalenetal.CP.2022 muffin.CB.GIteiiaa.BASESFeTDTL_rb.SPIN 10000

./runmuffin.sh muffin.CB.GIteiiva.BASESFeTDTL_rb PUBS/manuscript/odalenetal.CP.2022 muffin.CB.GIteiiva.BASESFeTDTL_rb.SPIN 10000

For running with the fuller set of diagnostic tracers:

./runmuffin.sh muffin.CB.GIteiiaa.BASESFeTDTL_14Crbcol0137 PUBS/manuscript/odalenetal.CP.2022 muffin.CB.GIteiiaa.BASESFeTDTL_14Crbcol0137.SPIN 10000

./runmuffin.sh muffin.CB.GIteiiva.BASESFeTDTL_14Crbcol0137 PUBS/manuscript/odalenetal.CP.2022 muffin.CB.GIteiiva.BASESFeTDTL_14Crbcol0137.SPIN 10000

### data #######################################################

The Holocene and LGM benthic d13C datasets used are included:

Hol_d13c_CLEAN.txt
LGM_d13c_CLEAN.txt

The data columns are:

longitude (degrees E)
latitude (degrees N)
ocean depth (m)
d13C (o/oo)
site ID (string)

################################################################
################################################################
 
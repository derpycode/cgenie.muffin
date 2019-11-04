################################################################
### readme.txt #################################################
################################################################

For:
'VarModel–based interpretation of ocean d13C from proxy records –
what can we learn about how the ocean changed across the deglaciation?'
Malin Ödalen, Carlye D. Peterson, Andy Ridgwell, Kevin I. C. Oliver, and Paul J. Valdes

################################################################
19/11/01 -- README.txt file creation 
################################################################

Provided are all the configuration files and forcing files necessary to run the model experiments presented in the paper.

The intention is to provide an opportunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

### base configuration ##########################################

Two different base-config files are used -- one for the late Holocene (HOL) 
and one for the Last Glacial Maximum (LGM)

These are found in the genie-main/configs directory and for basic set of tracers plus ventillation age:

HOL -- muffin.CB.GIteiiaa.BASESFeTDTL_rb.config
LGM -- muffin.CB.GIteiiva.BASESFeTDTL_rb.config

and for the full set of tracers, including 14C and preformed nutrient, DIC, and d13C tracers:

HOL -- muffin.CB.GIteiiaa.BASESFeTDTL_14Crbcol0137.config
LGM -- muffin.CB.GIteiiva.BASESFeTDTL_14Crbcol0137.config

### model experiments -- tuned ################################

The paper focussed on 2 main model configurations for the HOL and LGM, both of which having tuned against 
observed distributions of late HOlocene and LGM benthic d13C, respectively.

The basic (no 14C, no preformed tracers) experiments can be spun-up as follows:

./runmuffin.sh muffin.CB.GIteiiaa.BASESFeTDTL_rb MS/odalenetal.CP.2019 muffin.CB.GIteiiaa.BASESFeTDTL_rb.SPIN 10000

./runmuffin.sh muffin.CB.GIteiiva.BASESFeTDTL_rb MS/odalenetal.CP.2019 muffin.CB.GIteiiva.BASESFeTDTL_rb.SPIN 10000

### model experiments - ensembles ############################




 
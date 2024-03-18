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
PUBLICATION DETAILS [summary of manuscript/publication]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Coupling of an implicit model for multiple CaCO3 particles diagenesis to an Earth system model: cGENIE.muffin._iMP v0.9

Yoshiki Kanzaki, Dominik Hülse, Jamie D. Wilson, Sandra Kirtland Turner, and Andy Ridgwell

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/03/15 -- README.txt file created by YK

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

OpenBLAS library is required for experiments

    (1) Library installation (if using a private computer): (see also instruction at iMP repository in GitHub https://github.com/imuds/iMP):
        1. Download OpenBLAS-#.#.##.tar.gz file from http://www.openblas.net/ 
        2. tar zxvf OpenBLAS-#.#.##.tar.gz
        3. cd OpenBLAS-#.#.##
        4. make PREFIX=/usr/local install

        If you get the error 'libopenblas.so.0: cannot open shared object file: No such file or directory' then try the following:
            1. sudo apt-get install libopenblas-base
            2. export LD_LIBRARY_PATH=/usr/lib/openblas-base/

    (2) Compiling cGENIE with iMP 
        (i) on a cluster
            1. Edit the openBLAS options for the cluster-specific library name and path in cgenie.muffin/genie-main/makefile.arc (L555-561)
            2. Add the following lines in the .bashrc file in your home directory with the relevant name of the openBLAS installation, e.g., openBLAS-0.2.20:
            module add openBLAS-#-#-##   
        (ii) on a private computer
            1. Comment out L557-559 (three lines below "### FOR CLUSTER") and uncomment L561 (one line below "### FOR LOCAL COMPUTER") 
            in cgenie.muffin/genie-main/makefile.arc 

Three main model experiments were run:

(i)     Simulations with CaCO3 dissolution by Archier 1991 vs. Kanzaki et al. 2021 are run with a signle class of CaCO3 particles (calcite). 
        Closed system spin-up for 20 kyr followed by open system spin-up for 20 kyr.
        The results of this are shown in Figures 1-3.
        This is experiment: worbe2.S36x36_10yr_test.SPIN1/2, worbe2.S36x36_10yr_arg0_v2.SPIN1/2
(ii)    Two classes CaCO3 particles, one calcite and another aragonite, are simualted.  
        Closed system spin-up for 20 kyr followed by open system spin-up for 20 kyr.
        CO2 injection simulation for 50 kyr with cumulative inputs of ~1000 and ~5000 PgC. 
        The results of this are shown in Figures 4-9.
        This is experiment: worbe2.S36x36_10yr_arg###_v2.SPIN1/2, .emit1k/5k where ### = 5, 10, 20, 40, 50  
(iii)   Two classes CaCO3 particles, one fine (defulat kinetics) and coarse (x 0.01 slower kinetics) calcite, are simualted.  
        Closed system spin-up for 20 kyr followed by open system spin-up for 20 kyr.
        CO2 injection simulation for 50 kyr with cumulative inputs of ~1000 and ~5000 PgC. 
        The results of this are shown in Figures 10-15.
        This is experiment: worbe2.S36x36_10yr_k###nnn_v2.SPIN1/2, .emit1k/5k where ### = 20, 40, 60, 80, 100

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

To run the closed spinup (20 kyr total):

1. with Archer 1991 option for SEDGEM (using same userconfigs as EXAMPLE.worbe2.RidgwellHargreaves1997_S36x36.SPIN1)
./runmuffin_sed10yr.sh cgenie.eb_go_gs_ac_bg_sg_rg.worbe2.BASE / worbe2.S36x36_10yr_test.SPIN1 20000 

2. with Kanzaki et al. 2021 option where *** = arg### or k###nnn for SEDGEM 
./runmuffin_sed10yr.sh cgenie.eb_go_gs_ac_bg_sg_rg.worbe2.BASE / worbe2.S36x36_10yr_***_v2.SPIN1 20000 

Then to run open system spin-up from the closed spuinup (20 kyr total):

1. with Archer 1991 option for SEDGEM (using same userconfigs as EXAMPLE.worbe2.RidgwellHargreaves1997_S36x36.SPIN2)
./runmuffin_sed10yr.sh cgenie.eb_go_gs_ac_bg_sg_rg.worbe2.BASE / worbe2.S36x36_10yr_test.SPIN2 20000 worbe2.S36x36_10yr_test.SPIN1 

2. with Kanzaki et al. 2021 option where *** = arg### or k###nnn for SEDGEM 
./runmuffin_sed10yr.sh cgenie.eb_go_gs_ac_bg_sg_rg.worbe2.BASE / worbe2.S36x36_10yr_***_v2.SPIN2 20000 worbe2.S36x36_10yr_***_v2.SPIN1

Then to run the main experiment (ii/iii) from the spuinup (50 kyr total) with Kanzaki et al. 2021 option where *** = arg### or k###nnn and %%% = emit1k/5k

./runmuffin_sed10yr.sh cgenie.eb_go_gs_ac_bg_sg_rg.worbe2.BASE / worbe2.S36x36_10yr_***_v2.%%% 50000 worbe2.S36x36_10yr_***_v2.SPIN2

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ANALYSING THE EXPERIMENTS 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Sediment profiles calculated by iMP is stored in sedgem/imp/profiles directory.
Global maps of sediment composition are stored in sedgem/imp directory.
Data are currently stored as ASCII data.

Those results files are:
ccbml-xxx 		(CaCO3 comp. in wt% at the bottom of mixed layer)
ccdis-xxx 		(CaCO3 dissolution flux in umol cm-2 yr-1)
ccsfc-xxx 		(CaCO3 comp. in wt% at the seawater-sediment interface)
ccsfcave-xxx 	(CaCO3 average comp. in wt% within mixed layer)
errf-xxx 		(relative error in total volume of solid sediment, used as an indicator of overall covergence)
om-xxx 			(OM comp. in wt% at the bottom of mixed layer)
pt-xxx 			(Detrital material comp. in wt% at the bottom of mixed layer)

xxx means time in yr but divided by 10 (e.g., xxx = 100 means record at 1000 yr).
	
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================

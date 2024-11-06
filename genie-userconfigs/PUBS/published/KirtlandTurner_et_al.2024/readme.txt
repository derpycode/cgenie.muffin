================================================================
=== readme.txt =================================================
================================================================

Provided are as part of the code release the configuration files necessary to run the key model experiments presented in the paper.
The intention is to provide an oppertunity to question the paper assumptions and interpretation through re-analysis,
as well as the creation of new and different experiments. (Plus, to provide a means to replicate published results.)
This readme file details how the experiments can be run.
Refer to the mufifn manual:
https://github.com/derpycode/muffindoc
for details on model code installation and configuration, locating and visualizing model results, etc.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PUBLICATION DETAILS [summary of manuscript/publication]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Sensitivity of ocean circulation to warming during the Early Eocene

Sandra Kirtland Turner, Andy Ridgwell, Allison Keller, Max Vahlenkamp, Adam Aleksinksi, Philip F. Sexton, Don Penman, Pincelli Hull, Richard D. Norris

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
EDITING LOG [list of changes made to this file, when, and by who]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

2024/04/16 -- Viewing experiment output section created by SKT
2024/04/16 -- Added SPINUP to diagnose mixed-layer depth by SKT
2023/07/14 -- README.txt file created by AR
2024/05/13 -- Updated (AR)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUMMARY OF EXPERIMENTS [summerize experiments detailed and in which e.g. figures they appear]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Three main model experiments were run:

(i)     An imposed carbon isotopic excursion in the atmosphere of -1 o/oo occurring linearly over 10 kyr, and then recovering linearly over the subsequent 30 kyr.
        The results of this are shown in Figures 3 and 4.
        This is experiment: muffin.p0055c.CIE1.EXPT
(ii)    As per (i), but with fixed (x3) radiative forcing and hence only the carbon cycle (and not climate) changes
        The results of this are shown in the SI.
        This is experiment: muffin.p0055c.CIE1.NOradfor.EXPT
(iii)   As per (i), but with only radiative forcing and climate changing, following the climate change of (i). No carbon emisisons are imposed.
        The results of this are shown in the SI.
        This is experiment: muffin.p0055c.CIE1.NOemissions.EXPT

All 3 experiments are run for a total of 50 kyr following from a spinup (experiment: muffin.p0055c.closed.SPIN).

An additional spinup experiment was used to diagnose the ocean mixed layer depth (Supplemental Figure S5) (experiment:muffin.p0055c.closed.SPIN.mld)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RUNNING THE EXPERIMENTS [command lines, broken down in sub-sections for spinups, main experiments, SI, etc where appropriate]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

To run the spinup (10 kyr total):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/published/KirtlandTurner_et_al.2024 muffin.p0055c.closed.SPIN 10000

Then to run the main experiment (i) from the spuinup (50 kyr total):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/published/KirtlandTurner_et_al.2024 muffin.p0055c.CIE1.EXPT 50000 muffin.p0055c.closed.SPIN

Additional experiment (ii) is run similarly:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/published/KirtlandTurner_et_al.2024 muffin.p0055c.CIE1.NOradfor.EXPT 50000 muffin.p0055c.closed.SPIN

However, experiment (iii) requires (conservative) atmospheric CH4 tracers to force climate without carbon emissions 
and hence a different base-config (cgenie.eb_go_gs_ac_bg.p0055c.BASESCH4rb):

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESCH4rb PUBS/published/KirtlandTurner_et_al.2024 muffin.p0055c.CIE1.NOemissions.EXPT 50000 muffin.p0055c.closed.SPIN

To run the spinup to diagnose mixed layer depth from the spinup (1 kyr total):
./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0055c.BASESrb PUBS/published/KirtlandTurner_et_al.2024 muffin.p0055c.closed.SPIN.mld 1000 muffin.p0055c.closed.SPIN

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

VIEWING EXPERIMENT OUTPUT [paths for datasets presented in the publication]

Figure 3 	Model time series output: muffin.p0055c.CIE1.EXPT/biogem/
		biogem_series_atm_pCO2_13C.res (atmospheric CO2 d13C)
		biogem_series_atm_temp.res (atmospheric temperature)
		biogem_series_ocn_DIC_13C.res (ocean dissolved inorganic carbon d13C)
		biogem_series_misc_opsi.res (global overturning)
Figure 3 and 4 	Model spatial output: muffin.p0055c.CIE1.EXPT/biogem/fields_biogem_3d.nc
		ocn_DIC_13C (ocean dissolved inorganic carbon d13C)
		misc_col_Dage (ocean ventilation age)
		
Supplemental Figures:
Supp. Fig. S1	muffin.p0055c.CIE1.EXPT/biogem/fields_biogem_3d.nc
		grid_mask (model grid)
Supp. Fig. S2 	muffin.p0055c.CIE1.EXPT/biogem/fields_biogem_2d.nc
		phys_opsi (global stream function)
Supp. Fig. S3	muffin.p0055c.CIE1.EXPT/biogem/fields_biogem_3d.nc
		misc_col_Dage (ocean ventilation age)
Supp. Fig. S4	muffin.p0055c.CIE1.EXPT/biogem/fields_biogem_3d.nc
		ocn_DIC_13C (ocean dissolved inorganic carbon d13C)
Supp. Fig. S5	muffin.p0055c.closed.SPIN.mld/biogem/fields_biogem_2d.nc
		phys_MLD (ocean mixed layer depth)
Supp. Fig. S6	muffin.p0055c.CIE1.EXPT/biogem/fields_biogem_3d.nc
		ocn_sal (ocean salinity)
Supp. Fig. S8	muffin.p0055c.CIE1.NOradfor.EXPT/biogem/fields_biogem_3d.nc
		ocn_DIC_13C (ocean dissolved inorganic carbon d13C)
		misc_col_Dage (ocean ventilation age)
Supp. Fig. S9	muffin.p0055c.CIE1.NOemissions.EXPT/biogem/fields_biogem_3d.nc
		ocn_DIC_13C (ocean dissolved inorganic carbon d13C)
		misc_col_Dage (ocean ventilation age)
Supp. Fig. S10	muffin.p0055c.CIE1.NOradfor.EXPT/biogem/
		biogem_series_atm_pCO2_13C.res (atmospheric CO2 d13C)
		biogem_series_atm_temp.res (atmospheric temperature)
		biogem_series_ocn_DIC_13C.res (ocean dissolved inorganic carbon d13C)
		biogem_series_misc_opsi.res (global overturning)
Supp. Fig. S11	muffin.p0055c.CIE1.NOemissions.EXPT/biogem/
		biogem_series_atm_pCO2_13C.res (atmospheric CO2 d13C)
		biogem_series_atm_temp.res (atmospheric temperature)
		biogem_series_ocn_DIC_13C.res (ocean dissolved inorganic carbon d13C)
		biogem_series_misc_opsi.res (global overturning)
Supp. Fig. S12	muffin.p0055c.CIE1.EXPT/biogem/fields_biogem_3d.nc
		bio_fpart_POC (particulate organic carbon flux density)
		bio_fpart_POC_13C (d13C of particulate organic carbon flux)
		muffin.p0055c.CIE1.EXPT/biogem/
		biogem_series_fexport_POC.res (global particulate organic carbon flux)
		biogem_series_misc_opsi.res (global overturning)
						
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Note that all experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

================================================================
================================================================
================================================================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This folder includes the configuration files used for the paper (Li et al., 2024, Nature Geoscience).

The contents are as follows:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:	ReadMe.txt
Note:	This file.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:	p0055c.Lietal2020.3pal.SPIN1

Note:	The initial spin-up phase, where the ocean-atmosphere carbon cycle is set to 'closed.' Global weathering fluxes are balanced with the sedimentary burial of CaCO3, and bioturbation is disabled in sediments. The model starts from a 'cold' state, with uniform temperature and salinity in the ocean, and uniform temperature and humidity in the atmosphere. All biogeochemical tracers are initialized with uniform concentrations, and deep-ocean sediments lack biogenic material. Alkalinity is set to 1975 μmol eq/kg, yielding a mean global CaCO3 content of approximately 47%. Although the final ensemble does not rely on this prescribed alkalinity, the phase is run with pCO2 fixed at 834 ppm and δ13C at -4.9%. The model runs for 20 kyr, reaching steady state by the end.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/lietal.NatGeo.2024 p0055c.Lietal2020.3pal.SPIN1 20000

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:	p0055c.Lietal2020.3pal.SPIN2gl

Note:	The second phase transitions to an 'open' system, enabling temperature-controlled carbonate and silicate weathering, with evolving pCO2 and δ13C values. Bioturbation is enabled in surface sediments. This phase runs for 200 kyr, with an acceleration ratio of 1:9 (10 years of model time for every 100 years of simulation), and pCO2 drifts within 2 ppm over 200 kyr.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/lietal.NatGeo.2024 p0055c.Lietal2020.3pal.SPIN2gl 200000 p0055c.Lietal2020.3pal.SPIN1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:	ML.petm016
By:	Mingsong Li
Date:	May 20, 2020
Detail:	
	60-member ensemble
	2 myr open system run
Comment:
	This ensemble investigates state-dependent climate sensitivity, where climate sensitivity varies with pCO2. It explores two variables: (1) volcanic outgassing rates (0.6 to 2.4 times the baseline rate of 7.24 × 10¹² mol C/yr in increments of 0.2) and (2) a climate feedback parameter (λ) for pCO2 doubling (ΔF2x). The ΔF2x/λ ratio (τ) is varied from 2.88 to 17.31, covering a range of radiative forcing. With the system set to ‘open’ and bioturbation enabled, each experiment undergoes a 2 million year run to a new steady state, using an acceleration ratio of 1:90 (10 years of model run per every 1000 years of simulation).

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/lietal.NatGeo.2024/ML.petm016 p0055c.Lietal2020impfac3pal.x0p6.delf2x2 2000000 p0055c.Lietal2020.3pal.SPIN2gl
	... 
	...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:	ML.petm023
By:	Mingsong Li
Date:	Jan 22, 2021
Detail:	
	This 10-member ensemble ran for 2 Myr, varying volcanic outgassing rates from 0.6 to 2.4 times the baseline rate.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0055c.BASES MS/lietal.NatGeo.2024/ML.petm023 p0055c.Lietal2020.3palcesm.0.6x.2myr 2000000
	... 
	...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:	ML.petm035
By:	Mingsong Li
Date:	Dec 17, 2023
Detail:	
	For the pre-PETM: a 100-member ensemble considering three uncertainties: pCO2 (1x-5x PAL 278 ppm), alkalinity (1422.26 - 2279.50 μmol eq/kg), and the rain ratio (0.05-0.25). The ensemble uses state-dependent climate sensitivity in a 'closed' system framework.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES MS/lietal.NatGeo.2024/ML.petm035 ML.petm035.ID.1 20000
	...
	...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:	ML.petm038
By:	Mingsong Li
Date:	Dec 27, 2023
Detail:	
	For the peak-PETM: a 100-member ensemble builds upon the pre-PETM simulations, incorporating carbon release events specific to the PETM.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES MS/lietal.NatGeo.2024/ML.petm038 ML.petm038.ID.1 5000 ML.petm035.ID.1
	...
	...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:   ML.petm042
By:	Mingsong Li
Date:	June 8, 2024
Detail:	
	For the pre-PETM: a 100-member ensemble, same as ML.petm035, but using fixed climate sensitivity.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES MS/lietal.NatGeo.2024/ML.petm042 ML.petm042.ID.1 20000
	...
	...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:   ML.petm043
By:	Mingsong Li
Date:	June 8, 2024
Detail:	
	For the peak-PETM: a 100-member ensemble, same as ML.petm038, but using fixed climate sensitivity.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES MS/lietal.NatGeo.2024/ML.petm043 ML.petm043.ID.1 5000 ML.petm042.ID.1
	...
	...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Name:   preEECO03
By:	Mingsong Li
Date:	Jan 8, 2024
Detail:	
	For both the pre-PETM and peak-PETM: a 100-member ensemble with initial pCO2 varying from 1x to 11x PAL (278 ppm), alkalinity ranging from 1422.26 – 2872.10 μmol eq/kg, and a rain ratio from 0.05 to 0.25.

Submit JOB to UCR cluster:
	qsub -q puppy0.q -j y -o cgenie_log -V -S /bin/bash runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.p0055c.BASES MS/lietal.NatGeo.2024/preEECO03 preEECO03.ID.1 20000
	...
	...

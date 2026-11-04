
1. Spin-up under HOL boundary conditions

./runmuffin.sh muffin.CBE.GIteiiva.BASESFeTDTL_14C_tracer_HOL PUBS/submitted/Shao_et_al.2026 muffin.CBE.GIteiiva.BASESFeTDTL_HOL_spinup 10000

2. 280 ocean circulation perturbation exps under LGM boundary conditions

example:
./runmuffin.sh muffin.CBE.GIteiiva.BASESFeTDTL_14C_tracer PUBS/submitted/Shao_et_al.2026/ocean_circulation LGM_NAFwF-0.05_BRF0.3_SOFwF-0.3_WSF1 7000 muffin.CBE.GIteiiva.BASESFeTDTL_HOL_spinup

3. 59 carbonate chemistry perturbation exps under LGM boundary conditions

example: 
./runmuffin.sh muffin.CBE.GIteiiva.BASESFeTDTL_14C_tracer PUBS/submitted/Shao_et_al.2026/ALK_inventory LGM_NAFwF-0.05_BRF0.3_SOFwF-0.3_WSF1_dALK-100 10000 LGM_NAFwF-0.05_BRF0.3_SOFwF-0.3_WSF1



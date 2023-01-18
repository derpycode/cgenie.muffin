################################################################
### readme.txt #################################################
################################################################

For:
Volcanic CO2 degassing postdates thermogenic carbon emission during the end-Permian mass extinction
Yuyang Wu, Ying Cui, Daoliang Chu, Haijun Song, Jinnan Tong, Jacopo Dal Corso, Andy Ridgwell

################################################################
22/03/17 -- readme.txt file creation 
23/01/18 -- updated for publication
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

### model experiments -- spinups ##########################################

(1) First stage SPINUP

The initial, 1st-stage closed system spin-up

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 spinup1 20000

(2) Second Stage SPINUP

The follow-on, 2nd-stage open system and accelerated spin-up

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 spinup2 400000 spinup1

### model experiments -- main ensemble ##########################

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2LO_180000yrs 180000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2MO_180000yrs 180000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2HI_180000yrs 180000 spinup2

### model experiments -- further sensitivity experiments ######################

#CIE magnitude 4‰
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2LO_CIE4_180000yrs 180000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2MO_CIE4_180000yrs 180000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2HI_CIE4_180000yrs 180000 spinup2

#CIE magnitude 5‰
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2LO_CIE5_180000yrs 180000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2MO_CIE5_180000yrs 180000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2HI_CIE5_180000yrs 180000 spinup2

#CIE duration 90000 yr

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2LO_90000yrs 90000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2MO_90000yrs 90000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2HI_90000yrs 90000 spinup2

#CIE duration 18000 yr

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2LO_18000yrs 18000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2MO_18000yrs 18000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 pCO2HI_18000yrs 18000 spinup2

#alignment scenario
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario1_pCO2MO_92000yrs 92000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario2_pCO2MO_92000yrs 92000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario3_pCO2MO_92000yrs 92000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario4_pCO2MO_92000yrs 92000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario5_pCO2MO_92000yrs 92000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario6_pCO2MO_92000yrs 92000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario7_pCO2MO_92000yrs 92000 spinup2
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg_gl.p0251b.BASESCO2 PUBS/published/Wu_et_al.2023 scenario8_pCO2MO_92000yrs 92000 spinup2

################################################################
################################################################
################################################################

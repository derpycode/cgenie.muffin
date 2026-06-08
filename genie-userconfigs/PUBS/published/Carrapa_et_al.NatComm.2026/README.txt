################################################################
### README.txt #################################################
################################################################

For:
'Andean volcanism, ocean fertilization, marine ecosystem turnover, and global cooling in the Late Miocene'
B. Carrapa, M. Clementz, N. J. Cosentino, P. Di Nezio, P. Vervoort, K. Thirumalai, J. T. Abell, D. Hülse, P. R. Martinez, C. Gutstein

################################################################
11/03/2026 -- README.txt file creation (PV)
################################################################

Provided is the code used to create the model experiments presented in the paper.
Also given are the configuration files necessary to run these experiments.

The intention is to provide an opportunity to question the assumptions and
interpretation through re-analysis and the creation of new and different experiments.
(Plus, to provide a means to replicate results.)

All experiments are run from:
$HOME/cgenie.muffin/genie-main
(unless a different installation directory has been used)

### model experiments -- spinup #################################
(1a) INITIAL SPINUP: closed system spin-up needs 20 kyr to reach equilibrium

The command to run the spinup is:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.worjh2.BASEFe PUBS/published/Carrapa_et_al.NatComm.2026 worjh2.sg_rg.PO4Fe.SPIN 20000


### model experiments -- main ensemble ##########################
(2a) EXPERIMENTS: open system with respect to the (in)organic carbon cycle
From top to bottom:
1. One (33.3%) Fe-PO4 ash flux every 25 yr 
2. One (100%) Fe-PO4 ash flux every 75 yr
3. One (200%) Fe-PO4 ash flux every 125 yr 
4. One (1200%) Fe-PO4 ash flux every 75 yr
5. One (100%) Fe-PO4 ash flux every 75 yr plus double dust Fe solubility

The commands to run the nutrient fertilization experiments are:

./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.worjh2.BASEFe PUBS/published/Carrapa_et_al.NatComm.2026 worjh2.sg_rg.PO4Fe.25yr.FePO4low.20kyr 20000 worjh2.sg_rg.PO4Fe.SPIN
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.worjh2.BASEFe PUBS/published/Carrapa_et_al.NatComm.2026 worjh2.sg_rg.PO4Fe.75yr.FePO4.20kyr 20000 worjh2.sg_rg.PO4Fe.SPIN
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.worjh2.BASEFe PUBS/published/Carrapa_et_al.NatComm.2026 worjh2.sg_rg.PO4Fe.125yr.FePO4high.20kyr 20000 worjh2.sg_rg.PO4Fe.SPIN
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.worjh2.BASEFe PUBS/published/Carrapa_et_al.NatComm.2026 worjh2.sg_rg.PO4Fe.75yr.FePO4.12x.20kyr 20000 worjh2.sg_rg.PO4Fe.SPIN
./runmuffin.sh cgenie.eb_go_gs_ac_bg_sg_rg.worjh2.BASEFe PUBS/published/Carrapa_et_al.NatComm.2026 worjh2.sg_rg.PO4Fe.75yr.FePO4.DUSTx2.20kyr 20000 worjh2.sg_rg.PO4Fe.SPIN


################################################################
################################################################
################################################################

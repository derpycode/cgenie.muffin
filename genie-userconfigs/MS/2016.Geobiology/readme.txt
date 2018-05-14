Meyer, K.M., A. Ridgwell, and J.L. Payne, The influence of the biological pump on ocean chemistry: implications for long-term trends in marine redox chemistry, the global carbon cycle, and the evolution of marine animal ecosystems, Geobiology, DOI: 10.1111/gbi.12176 (2016).

MODERN

All the experiments comprising the modern ensemble of varying oceanic PO4 inventory and organic matter remineralization depth,
were run on from a (the same) spinup experiment.

The modern spiup experiment is run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFe MS\2016.Geobiology 20140121a_worjh2.PO4Fe.SPIN 10000

The ensemble experiments are then run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.worjh2.BASEFe MS\2016.Geobiology xxx 10000 20140121a_worjh2.PO4Fe.SPIN 

where xxx is the name of the ensemble experiment. These are named and configured as follows:

ID	                    PO4 multiplier	e-folding depth
20141126a_worjh2.PO4Fe	0.5	            60	
20141126b_worjh2.PO4Fe	0.5	            200
20141126c_worjh2.PO4Fe	0.5	            589	
20141126d_worjh2.PO4Fe	0.5	            1000	
20141126e_worjh2.PO4Fe	1	            60	
20141126f_worjh2.PO4Fe	1	            200	
20141126g_worjh2.PO4Fe	1	            589	
20141126h_worjh2.PO4Fe	1	            1000
20141126i_worjh2.PO4Fe	2	            60	
20141126j_worjh2.PO4Fe	2	            200
20141126k_worjh2.PO4Fe	2	            589	
20141126l_worjh2.PO4Fe	2	            1000
20141126m_worjh2.PO4Fe	5	            60	
20141126n_worjh2.PO4Fe	5	            200
20141126o_worjh2.PO4Fe	5	            589
20141126p_worjh2.PO4Fe	5	            1000

END PERMIAN

A spinup experiment was run, BUT the experiments comprising the end Permian ensemble of varying oceanic PO4 inventory and organic matter remineralization depth,
were run from cold (and did not use a spin-up).
[The intention was to use the spinup. However, because the runs were 10,000 years duration, they attained steady state anyway.]

The end Permian spiup experiment is run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0251b.BASESFe MS\2016.Geobiology 20141212a_p0251b.PO4Fe.SPIN 10000

The ensemble experiments are then run:

./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0251b.BASESFe MS\2016.Geobiology xxx 10000

(or, from a restart: ./runmuffin.sh cgenie.eb_go_gs_ac_bg.p0251b.BASESFe MS\2016.Geobiology xxx 10000 20141212a_p0251b.PO4Fe.SPIN)

where xxx is the name of the ensemble experiment. These are named and configured as follows:

ID	                            PO4 multiplier	e-folding depth
20141213a_p0251b.PO4Fe.SPIN	    0.5	            60
20141213b_p0251b.PO4Fe.SPIN	    0.5	            200
20141213c_p0251b.PO4Fe.SPIN	    0.5	            589
20141213d_p0251b.PO4Fe.SPIN	    0.5	            1000
20141213e_p0251b.PO4Fe.SPIN	    1	            60
20141213f_p0251b.PO4Fe.SPIN	    1	            200
20141213g_p0251b.PO4Fe.SPIN	    1	            589
20141213h_p0251b.PO4Fe.SPIN	    1	            1000
20141213i_p0251b.PO4Fe.SPIN	    2	            60
20141213j_p0251b.PO4Fe.SPIN	    2	            200
20141213k_p0251b.PO4Fe.SPIN	    2	            589
20141213l_p0251b.PO4Fe.SPIN	    2	            1000
20141213m_p0251b.PO4Fe.SPIN	    5	            60
20141213n_p0251b.PO4Fe.SPIN	    5	            200
20141213o_p0251b.PO4Fe.SPIN	    5	            589
20141213p_p0251b.PO4Fe.SPIN	    5	            1000

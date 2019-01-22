c     ******************************************************************
c     *** GENIE ********************************************************
c     ******************************************************************

      program GENIE

      use omp_lib
      use genie_util, only : die
      use write_netcdf
      use genie_control
      use genie_global
      use genie_ini_wrappers
      use genie_loop_wrappers
      use genie_end_wrappers

      implicit none

c     ! >>> CHECK >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
c     SG.gt.This is (hopefully!) a temp measure for ENTS
#include "../genie-ents/src/fortran/var_ents.cmn"
c     SG <
c     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !

      print*
      print*,'*******************************************************'
      print '("  *** Welcome to cGENIE -- version:",I5)',genie_version
      print*,'*******************************************************'
      print*
      
c     ******************************************************************
c     *** INITIALIZE ***************************************************
c     ******************************************************************
c     
c     ==================================================================
c     Initialize GENIE
      call initialise_genie
c     ==================================================================
c     Initialise GOLDSTEIN ocean model
      if (flag_goldsteinocean) then
         call initialise_goldocean_wrapper
      endif
c     ==================================================================
c     Initialise EMBM atmosphere model
      if (flag_ebatmos) then
         call initialise_embm_wrapper
      endif
c     ==================================================================
c     Initialise PLASIM atmosphere model
      if (flag_plasimatmos) then
         call initialise_plasim_wrapper
      endif
c     ==================================================================
c     Initialise c-GOLDSTEIN sea-ice model
      if (flag_goldsteinseaice) then
         call ini_goldsteinseaice_wrapper
      endif
c     ==================================================================
c     Initialise land-surface scheme
      if (flag_ents) then
         call initialise_ents_wrapper
      endif
c     ==================================================================
c     *** GEM model - START
      if (flag_atchem.or.flag_biogem.or.flag_sedgem.or.flag_rokgem) then
         call initialise_gem_wrapper
      endif
c     ==================================================================
c     *** GOLDlite model - START
      if (flag_goldlite) then
         call initialise_goldlite_wrapper
      endif
c     ==================================================================
c     *** OCNlite model - START
      if (flag_ocnlite) then
         call initialise_ocnlite_wrapper
      endif
c     ==================================================================
c     *** ECOGEM model - START
      if (flag_ecogem) then
         call initialise_ecogem_wrapper
      endif
c     ==================================================================
c     *** BIOGEM model - START
      if (flag_biogem) then
         call initialise_biogem_wrapper
ccc         call biogem_climate_wrapper
         call biogem_tracercoupling_wrapper
      endif
c     ==================================================================
c     *** ATCHEM model - START
      if (flag_atchem) then
         call initialise_atchem_wrapper
         call cpl_comp_atmocn_wrapper
         call cpl_comp_EMBM_wrapper
      endif
c     ==================================================================
c     *** SEDGEM model - START
      if (flag_sedgem) then
         call initialise_sedgem_wrapper
         call cpl_flux_sedocn_wrapper
         call cpl_comp_sedocn_wrapper
      endif
c     ==================================================================
c     *** ROKGEM model - START
      if (flag_rokgem) then
         call initialise_rokgem_wrapper
c     call cpl_flux_rokocn_wrapper
      endif
c     ==================================================================
c     *** GEMlite model - START
      if (flag_gemlite) then
         call initialise_gemlite_wrapper
      endif
c     ==================================================================

c     ******************************************************************
c     *** MAIN PROGRAM *************************************************
c     ******************************************************************

      print*
      print*,'*******************************************************'
      print*,' *** Initialisation complete: simulation starting ...'
      print*,'*******************************************************'

c     ==================================================================
c     Initialize loop counters
      istep_atm=0
      istep_ocn=0
      istep_sic=0
      istep_gem=0
      istep_tot=0
c     initialize GEM time-stepping
      gem_yr    = gem_yr_min
      gem_notyr = gem_notyr_max
c     ==================================================================
c     *** BIOGEM model - headers and initialization run-time reporting 
      if (flag_biogem) then
         call biogem_climate_wrapper
         call diag_biogem_wrapper
      endif
c     ==================================================================
c     
c     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
c     ! *** MAIN TIME-STEPPING LOOP START **************************** !
c     ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
c     
c     NOTE: koverall is in hours
      do koverall=1,koverall_total
c     increment the clock which accumulates total time
         call increment_genie_clock
c     
         if (flag_gemlite) then
            if(mod(koverall,kgemlite*kocn_loop).eq.1) then
c     increment GEM year count
               istep_gem=istep_gem+1
               istep_tot=istep_tot+1
               if (debug_loop.gt.2) PRINT*,istep_gem,gem_notyr,gem_yr
c     ******************************************************************
c     test for for GEM count exceeding a complete 1st (normal) cycle
               if(istep_gem.eq.(gem_notyr+1)) then
c     ### SWITCH TO GEM CYCLE ##########################################
                  if (debug_loop.gt.0) PRINT*,'### START GEM CYCLE ###'
cc     adjust GEM cycle count
c                  gem_notyr = gem_notyr - gem_dyr
c                  if (gem_notyr.lt.gem_notyr_min) 
c     &                 gem_notyr = gem_notyr_min
c                  if (debug_loop.gt.1) 
c     &                 PRINT*,'*** gem_yr, D(gem_yr)  :',  
c     &                 gem_yr,gem_dyr
                  if (debug_loop.gt.3) PRINT*,'* gemlite_cycleinit'
                  call gemlite_cycleinit_wrapper
c     copy current state of tracer arrays to GEMlite
                  if (debug_loop.gt.3) PRINT*,'* cpl_comp_gemglt'
                  call cpl_comp_gemglt_wrapper
c     convert between sediment grids (for integrated sediment rain flux)
                  if (debug_loop.gt.3) 
     &                 PRINT*,'* cpl_flux_sedsed1'
                  call cpl_flux_sedsed1_wrapper
c     copy climate state variables (here: sea-ice)
                  if (debug_loop.gt.3) 
     &                 PRINT*,'* gemlite_climate_wrapper'
                  call gemlite_climate_wrapper
c     ##################################################################
               elseif (istep_gem.gt.(gem_notyr+gem_yr)) then
c     $$$ REVERT TO NORMAL CYCLE $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     NOTE: <genie_ocn> summed anomoly not re-set at this stage
c     (OK in the way integration is done and not carried forward?)
                  if (debug_loop.gt.0)
     &                 PRINT*,'$$$ REVERT NORMAL CYCLE $$$'
c     reset count
                  istep_gem = 1
                  gem_switch=-1
c     adjust GEM cycle count
                  gem_yr = gem_yr + gem_dyr
                  if (gem_yr.gt.gem_yr_max) gem_yr = gem_yr_max
                  gem_notyr = gem_notyr - gem_dyr
                  if (gem_notyr.lt.gem_notyr_min) 
     &                 gem_notyr = gem_notyr_min
                  if (debug_loop.gt.1) 
     &                 PRINT*,'*** gem_yr, gem_notyr, gem_dyr  :', 
     &                 gem_yr,gem_notyr,gem_dyr
c     apply *summed* anomolies to <atm>, <ocn>, <ts>, <ts1> so that
c     the last state of the seasonal cycle is preserved
c     NOTE: <ocn> has already been incrementally adjusted
c     (@ each gtl time-step) and does not need the summed anomoly
c     [genie-gemlite/gemlite.f90]
c     add summed <docn_sum> anomoly (excluding T,S) to <ts>, <ts1>
c     NOTE: before cpl_comp_gltgem because <docn_sum> gets re-set
                  if (debug_loop.gt.3) PRINT*,'* gemlite_ts'
                  call gemlite_gltts_wrapper
c     [genie-gemlite/cpl_comp_gemlite.f90]
c     write summed <datm>, <docn> anomolies to <genie_atm1>, <genie_ocn>
c     re-set <datm_sum>, <docn_sum>
                  if (debug_loop.gt.3) PRINT*,'* cpl_comp_gltgem'
                  call cpl_comp_gltgem_dsum_wrapper
c     [genie-atchem/cpl_comp_atchem.f90]
c     apply <genie_atm> compositional anomoly to <atm>
c     re-set <genie_atm1>
                  if (debug_loop.gt.3) PRINT*,'* cpl_comp_gematm'
                  call cpl_comp_gematm_wrapper
c     [genie-atchem/cpl_comp_atchem.f90]
c     update <sfcatm1> ocean interface array
                  if (debug_loop.gt.3) 
     &                 PRINT*,'* cpl_comp_atmocn_wrapper'
                  call cpl_comp_atmocn_wrapper
c     reset cumulative (annual average) weathering array
c     (<dum_sfxsumrok1_gem>)
c     + atm fluxes (outgassing, weathering CO2 consumption)
ccc   call gemlite_cycleclean_wrapper
                  call reinit_flux_rokocn_gem_wrapper
                  call reinit_flux_rokatm_gem_wrapper
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** TRIP CYCLE PHASE CHANGE **************************************
c     ensure that the experiment ends on a 'normal' year in order that
c     restarts (esp. climate modules) are saved
c     => when remaining time being less than 2 years,
c     force istep_gem to the end of a cycle (and normal phase start)
c     NOTE: also ensure that we are not already in a 'normal' phase
               elseif (
     &                 ((koverall_total-koverall).le.
     &                 (2*kgemlite*kocn_loop))
     &                 .and.
     &                 ((koverall_total-koverall).gt.
     &                 (kgemlite*kocn_loop))
     &                 .and.
     &                 (istep_gem.gt.gem_notyr)
     &                 ) then
                  if (debug_loop.gt.0)
     &                 PRINT*,'### FLAG SWITCH TO NORMAL FOR END -> $$$'
                  istep_gem=gem_notyr+gem_yr
c     ******************************************************************
               else
c     *** NOTHING! *****************************************************
c     NO NEED TO SWITCH CYCLE PHASE => keep calm and pony on
c     ******************************************************************
               endif
            endif
         endif
c     ******************************************************************
c     
c     test for being within 1st part of cycle
         if((istep_gem.le.gem_notyr).or.(.not.flag_gemlite)) then
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     $$$ START NORMAL $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            gem_status=0
            if (flag_gemlite.AND.(istep_gem.eq.gem_notyr)) gem_switch=1
            if (flag_gemlite) then
               if (mod(koverall,kgemlite*kocn_loop).eq.0) then
                  if (debug_loop.gt.1)
     &                 print*,'gem_status, gem_switch :',
     &                 gem_status,gem_switch
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ccc            call radfor_wrapper
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** c-GOLDSTEIN surface flux routine
            if (flag_ebatmos.and.flag_goldsteinocean) then
c     the next line should read ".eq.1" so that c-GOLDSTEIN
c     surflux is executed on the first time-step
               if(mod(koverall,kocn_loop).eq.1) then
                  if (debug_loop.gt.2) PRINT*,'>>> SURFLUX @ ',
     &                 koverall,kocn_loop
                  istep_ocn=istep_ocn+1
                  call surflux_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif 
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** GOLDSTEIN & PLASIM advance timestep
            if (flag_plasimatmos.and.flag_goldsteinocean) then
               if(mod(koverall,kocn_loop).eq.0) then
                  istep_ocn=istep_ocn+1
               endif
            endif 
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** EMBM atmosphere model
            if (flag_ebatmos) then
               if(mod(koverall,katm_loop).eq.0) then
                  if (debug_loop.gt.2) PRINT*,'>>> EMBM @ ',
     &                 koverall,katm_loop
                  istep_atm=istep_atm+1
                  call embm_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** PLASIM atmosphere model
            if (flag_plasimatmos) then
               if(mod(koverall,katm_loop).eq.0) then
                  istep_atm=istep_atm+1
                  call plasim_wrapper
                  call cpl_flux_lndatm_wrapper
!reset plasim averages for ocean
                  if(mod(koverall-1,kocn_loop).eq.0) then
                     latent_ocn=0.0
                     sensible_ocn=0.0
                     netsolar_ocn=0.0
                     netlong_ocn=0.0
                     latent_coeff_atm=0.0
                     sensible_coeff_atm=0.0
                     evap_ocn=0.0
                     precip_ocn=0.0
                     runoff_ocn=0.0
                     ocean_stressx2_ocn=0.0
                     ocean_stressy2_ocn=0.0
                     ocean_stressx3_ocn=0.0
                     ocean_stressy3_ocn=0.0
                     go_solfor=0.0
                  endif
!accumulate plasim fluxes for ocean
                  latent_ocn=latent_ocn+surf_latent_atm
                  sensible_ocn=sensible_ocn+surf_sensible_atm
                  netsolar_ocn=netsolar_ocn+netsolar_atm
                  netlong_ocn=netlong_ocn+netlong_atm
                  latent_coeff_atm=latent_coeff_atm+surf_latent_coeff_atm
                  sensible_coeff_atm=sensible_coeff_atm+surf_sensible_coeff_atm
                  evap_ocn=evap_ocn+evap_atm
                  precip_ocn=precip_ocn+precip_atm
                  runoff_ocn=runoff_ocn+land_runoff_atm
                  ocean_stressx2_ocn=ocean_stressx2_ocn+surf_stressx2_atm
                  ocean_stressy2_ocn=ocean_stressy2_ocn+surf_stressy2_atm
                  ocean_stressx3_ocn=ocean_stressx3_ocn+surf_stressx3_atm
                  ocean_stressy3_ocn=ocean_stressy3_ocn+surf_stressy3_atm
                  go_solfor=go_solfor+solfor_atm
!average plasim fluxes for ocean
                  if(mod(koverall,kocn_loop).eq.0) then
                     latent_ocn=latent_ocn/float(kocn_loop)
                     sensible_ocn=sensible_ocn/float(kocn_loop)
                     netsolar_ocn=netsolar_ocn/float(kocn_loop)
                     netlong_ocn=netlong_ocn/float(kocn_loop)
                     latent_coeff_atm=latent_coeff_atm/float(kocn_loop)
                     sensible_coeff_atm=sensible_coeff_atm/float(kocn_loop)
                     evap_ocn=evap_ocn/float(kocn_loop)
                     precip_ocn=precip_ocn/float(kocn_loop)
                     runoff_ocn=runoff_ocn/float(kocn_loop)
                     ocean_stressx2_ocn=ocean_stressx2_ocn/float(kocn_loop)
                     ocean_stressy2_ocn=ocean_stressy2_ocn/float(kocn_loop)
                     ocean_stressx3_ocn=ocean_stressx3_ocn/float(kocn_loop)
                     ocean_stressy3_ocn=ocean_stressy3_ocn/float(kocn_loop)
                     go_solfor=go_solfor/float(kocn_loop)
                  endif
!reset plasim averages for sea ice
                  if(mod(koverall-1,ksic_loop).eq.0) then
                     insolar_sic=0.0
                     inlong_sic=0.0
                     netheat_sic=0.0
                     surft_atm_sic=0.0
                     surfq_atm_sic=0.0
                     surfp_atm_sic=0.0
                     surf_windspeed_sic=0.0
                  endif
!accumulate plasim fluxes for sea ice
                  insolar_sic=insolar_sic+insolar_atm
                  inlong_sic=inlong_sic+inlong_atm
                  netheat_sic=netheat_sic+netheat_atm
                  surft_atm_sic=surft_atm_sic+surft_atm
                  surfq_atm_sic=surfq_atm_sic+surfq_atm
                  surfp_atm_sic=surfp_atm_sic+surfp_atm
                  surf_windspeed_sic=surf_windspeed_sic+surf_windspeed_atm
!average plasim fluxes for sea ice
                  if(mod(koverall,ksic_loop).eq.0) then
                     insolar_sic=insolar_sic/float(ksic_loop)
                     go_fxsw=insolar_sic !for biogem BUT FIX - NEED TO AVERAGE OVER BIOGEM TIMESTEP
                     inlong_sic=inlong_sic/float(ksic_loop)
                     netheat_sic=netheat_sic/float(ksic_loop)
                     surft_atm_sic=surft_atm_sic/float(ksic_loop)
                     surfq_atm_sic=surfq_atm_sic/float(ksic_loop)
                     surfp_atm_sic=surfp_atm_sic/float(ksic_loop)
                     surf_windspeed_sic=surf_windspeed_sic/float(ksic_loop)
                  endif 
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** c-GOLDSTEIN sea-ice model
            if (flag_goldsteinseaice) then
               if(mod(koverall,ksic_loop).eq.0) then
                  if (debug_loop.gt.2) PRINT*,'>>> GOLDICE @ ',
     &                 koverall,ksic_loop
                  istep_sic=istep_sic+1
                  if(flag_plasimatmos) then
                    call plasim_surflux_wrapper
                  endif
                  call gold_seaice_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** GOLDSTEIN ocean model
            if (flag_goldsteinocean) then
               if(mod(koverall,kocn_loop).eq.0) then
                  if (debug_loop.gt.2) PRINT*,'>>> GOLDSTEIN @ ',
     &                 koverall,kocn_loop
                  call goldstein_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** GOLDlite ocean transport model
            if (flag_goldlite) then
               if (mod(koverall,conv_kocn_kgoldlite*kocn_loop).eq.0)
     &              then
                  if (debug_loop.gt.2) PRINT*,'>>> GOLDLITE @ ',
     &                 koverall,conv_kocn_kgoldlite*kocn_loop
                  call goldlite_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** OCNlite ocean transport model
            if (flag_ocnlite) then
               if (mod(koverall,conv_kocn_kocnlite*kocn_loop).eq.0)
     &              then
                  if (debug_loop.gt.2) PRINT*,'>>> OCNLITE @ ',
     &                 koverall,conv_kocn_kocnlite*kocn_loop
                  call ocnlite_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** ENTS land model
            if (flag_ents) then
               if(mod(koverall,klnd_loop).eq.0) then
                  if (debug_loop.gt.2) PRINT*,'>>> ENTS @ ',
     &                 koverall,klnd_loop
                  call ents_wrapper
                  call cpl_flux_lndatm_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** ROKGEM model - UPDATE
            if (flag_rokgem) then
               if (mod(koverall,conv_kocn_krokgem*kocn_loop).eq.0)
     &              then
                  if (debug_loop.gt.2) PRINT*,'>>> ROKEGM @ ',
     &                 koverall,conv_kocn_krokgem*kocn_loop
                  call rokgem_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
c     If GEMlite: test for the end of the 1st (normal) part of cycle
c     => create annual averages of weathering <dum_sfxsumrok1_gem>
c        + atm exchnage (outgassing and weathering CO2 consumption)
                  if (flag_gemlite) then
                     if(istep_gem.eq.(gem_notyr)) then
                        if (debug_loop.gt.3) PRINT*,'* cpl_flux_rokgem'
                        call cpl_flux_rokatm_gem_wrapper
                        call cpl_flux_rokocn_gem_wrapper
                     endif
                  endif
c      Couple ocean and atmosphere ROKGEM fluxes
c      NOTE: carry out after possible pre-GEMlite integration step,
c            as fluxes passed from ROKGEM are re-set ...
                  if (debug_loop.gt.3) 
     &                 PRINT*,'* cpl_flux_rokatm_wrapper'
                  call cpl_flux_rokatm_wrapper
                  if (debug_loop.gt.3) 
     &                 PRINT*,'* cpl_flux_rokocn_wrapper'
                  call cpl_flux_rokocn_wrapper
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

c$$$  c$$$!$omp parallel private(nthreads,thread_id)
c$$$  
c$$$  c$$$            call omp_set_num_threads(4)
c$$$  c$$$            nthreads = omp_get_max_threads()
c$$$  c$$$            print*,'* # MAX THREADS: ',nthreads
c$$$  c$$$            nthreads = omp_get_num_threads()
c$$$  c$$$            print*,'* # ACTUAL THREADS: ',nthreads
c$$$  
c$$$  c$$$!$OMP SECTIONS
c$$$  c$$$!$OMP SECTION
c$$$  
c$$$  c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c$$$  c     *** GOLDSTEIN ocean model
c$$$  if (flag_goldsteinocean) then
c$$$  if(mod(koverall,kocn_loop).eq.0) then
c$$$  if (debug_loop.gt.2) PRINT*,'>m> GOLDSTEIN @ ',
c$$$  &                 koverall,kocn_loop
c$$$  call goldstein_wrapper
c$$$  if (debug_loop.gt.2) PRINT*,'<m< GOLDSTEIN'
c$$$  endif
c$$$  endif
c$$$  c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c$$$  
c$$$  c$$$!$OMP SECTION
c$$$  
c$$$  c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c$$$  c     *** BIOGEM model - UPDATE
c$$$  if (flag_biogem) then
c$$$  if (mod(koverall,conv_kocn_kbiogem*kocn_loop).eq.0)
c$$$  &              then
c$$$  if (debug_loop.gt.2) PRINT*,'>m> BIOGEM @ ',
c$$$  &                 koverall,conv_kocn_kbiogem*kocn_loop
c$$$  call biogem_forcing_wrapper
c$$$  call biogem_wrapper
c$$$  if (debug_loop.gt.2) PRINT*,'<m< BIOGEM'
c$$$  endif
c$$$  endif
c$$$  c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c$$$  
c$$$  c$$$!$OMP END SECTIONS
c$$$  c$$$!$omp end parallel

c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** ECOGEM model - UPDATE
            if (flag_ecogem) then
               if (mod(koverall,conv_kocn_kecogem*kocn_loop).eq.0)
     &              then
                  if (debug_loop.gt.2) PRINT*,'>>> ECOGEM @ ',
     &            koverall,conv_kocn_kecogem*kocn_loop
                  call ecogem_wrapper              
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** BIOGEM model - UPDATE
c     NOTE: call biogem_climate_sol_wrapper on the very first BIOGEM
c           time-step in order to initialize solar radiation to biota
c     NOTE: the calls have to be in this order (phy update after biogem)
c           so that BIOGEM is normally working with physics matching
c           the trace field (in terms of last time-step update)
c     NOTE: if the first or last year of a NORMAL phase (cf. gem_switch)
c           => force t-series save
            if (flag_biogem) then
               if (mod(koverall,conv_kocn_kbiogem*kocn_loop).eq.0)
     &              then
                  if (debug_loop.gt.2) PRINT*,'>>> BIOGEM @ ',
     &                 koverall,conv_kocn_kbiogem*kocn_loop
ccc THIS MOSTLY FIXES THE INITIAL LACK-OF-INSOLATION PROBLEM,
ccc BUT NEEDS BIOGEM TEST REPLACING (AND *CARE* TAKEN)
                  if (koverall.eq.(conv_kocn_kbiogem*kocn_loop))
     &                 call biogem_climate_sol_wrapper
                  if (flag_ecogem) call diag_biogem_ecogem_wrapper ! extract ECOGEM diagnostics
                  call biogem_forcing_wrapper
                  call biogem_wrapper
                  call biogem_tracercoupling_wrapper
                  call biogem_climate_wrapper 
                  if (debug_loop.gt.2) PRINT*,'<<<'
                  if (mod(koverall,conv_kocn_korb*kocn_loop).eq.0)
     &                 then
                     call diag_biogem_rec_orb_wrapper
                  endif
                  call diag_biogem_timeslice_wrapper
                  if (flag_ecogem) call diag_ecogem_timeslice_wrapper ! save and write ECOGEM timeslice data after BIOGEM
                  if (flag_gemlite.and.(gem_switch.ne.0)) then
                     call diag_biogem_force_timeseries_wrapper
                  else
                     call diag_biogem_timeseries_wrapper
                  end if
                  call cpl_flux_ocnatm_wrapper
                  if (flag_sedgem) call cpl_flux_ocnsed_wrapper
                  if (flag_sedgem) call cpl_comp_ocnsed_wrapper
                  if (flag_rokgem) call reinit_flux_rokocn_wrapper
                  if (debug_loop.gt.0) call diag_biogem_wrapper
c     If GEMlite: test for the end of the 1st (normal) part of cycle
c     => create annual averages of <ocn> (for later transfer to GEMlite)
                  if (flag_gemlite.and.(istep_gem.eq.gem_notyr)) then
                     if (debug_loop.gt.3) PRINT*,'* cpl_comp_ocngem'
                     call cpl_comp_ocngem_wrapper
                  endif
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** ATCHEM model - UPDATE
c     NOTE: for now, continue NOT to update <atm> with SAT
c           (subroutine: atchem_climate_wrapper)
c     NOTE: diabled call to cpl_comp_lndEMBM_wrapper, as duplicate of
c           cpl_comp_EMBM_wrapper?
c           (call to atchem_climate_wrapper)
            if (flag_atchem) then
               if (mod(koverall,conv_kocn_katchem*kocn_loop).eq.0) 
     &              then
                  if (debug_loop.gt.2) PRINT*,'>>> ATCHEM @ ',
     &                 koverall,conv_kocn_katchem*kocn_loop
                  call atchem_wrapper
ccc                  call atchem_climate_wrapper 
                  if (debug_loop.gt.2) PRINT*,'<<<'
                  call cpl_comp_EMBMatm_wrapper
ccc                  call cpl_comp_EMBM_wrapper
                  call cpl_comp_atmocn_wrapper
                  call cpl_comp_atmlnd_wrapper
ccc                  call cpl_comp_lndEMBM_wrapper
c     test for the end of the 1st part of cycle
c     create annual averages of <atm> (for later transfer to GEMlite)
                  if(istep_gem.eq.(gem_notyr)) then
                     if (debug_loop.gt.3) PRINT*,'* cpl_comp_atmgem'
                     call cpl_comp_atmgem_wrapper
                  endif
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     $$$ ADAPT CYCLE STEP $$$
c     extend duration of 'normal' cycle if rate of pCO2 change
c     is above the allowed threshold
            if (flag_biogem .AND. gem_adapt_auto) then
               if (mod(koverall,kgemlite*kocn_loop).eq.0) then
c     update (@ current time-step, not annual average) value of gem_pCO2
                  call diag_biogem_pCO2_wrapper
c     NOTE: test for excessive rate of pCO2 change only after first year
c     to avoid reverting to istep_gem = 0 state
c     NOTE: also avoid last year to allow the end of a NORMAL cycle
c     to be cleanly predicted (at the start fo the last N cycle)
                  if((istep_gem.gt.1).AND.(istep_gem.lt.gem_notyr)) then
                     if ( abs(gem_pCO2 - gem_pCO2_OLD).gt.
     &                    gem_adapt_dpCO2dt ) then
c     decrement cycle phase counter to
c     prolongue operation in full (non-GEM) time-stepping cycle phase
                        istep_gem = istep_gem - 1
                        if (debug_loop.gt.1) 
     &                       PRINT*,'$$$ NORMAL phase extended:', 
     &                       istep_gem,
     &                       ' / dpCO2 (ppm) = ',
     &                       abs(gem_pCO2 - gem_pCO2_OLD)
c     copy current state of tracer arrays to GEMlite
c     NOTE: arrays don't actually need copying yet, but it wil serve
c     to reset the cumulative annual average arrays
c     (now that the Normal phase has been extended another yr)
                        if (debug_loop.gt.3) PRINT*,'* cpl_comp_gemglt'
                        call cpl_comp_gemglt_wrapper
                     endif
                  endif
c     save current value of gem_pCO2 as gem_pCO2_OLD
c     for calculating rate of change of pCO2 between GEM (year) steps
                  gem_pCO2_OLD = gem_pCO2
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     *** SEDGEM model - UPDATE
            if (flag_sedgem) then
               if (mod(koverall,conv_kocn_ksedgem*kocn_loop).eq.0) 
     &              then
                  if(istep_gem.ne.gem_notyr) then
                     if (debug_loop.gt.2) PRINT*,'>>> SEDGEM @ ',
     &                    koverall,conv_kocn_ksedgem*kocn_loop
                     call sedgem_wrapper
                     if (debug_loop.gt.2) PRINT*,'<<<'
                  else
                     if (debug_loop.gt.2) PRINT*,'>>> SEDEGM_GEM @ ',
     &                    koverall,conv_kocn_ksedgem*kocn_loop
                     call sedgem_glt_wrapper
                     if (debug_loop.gt.2) PRINT*,'<<<'
                  endif
                  call cpl_flux_sedocn_wrapper
                  call cpl_comp_sedocn_wrapper
               endif
            endif
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
            if (flag_gemlite) then
               If ((istep_gem.gt.1).AND.(istep_gem.lt.gem_notyr)) 
     &              gem_switch=0
            end if
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     $$$ END NOTMAL $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         else
c     ##################################################################
c     ### START GEMLITE ################################################
c     ##################################################################
            gem_status=1
            gem_switch=0
            if (flag_gemlite) then
               if (mod(koverall,kgemlite*kocn_loop).eq.0) then
                  if (debug_loop.gt.1)
     &                 print*,'gem_status, gem_switch :',
     &                 gem_status,gem_switch
               endif
            endif
c     ##################################################################
c     *** EMBM - UPDATE
            if (flag_ebatmos) then
               if(mod(koverall,katm_loop).eq.0) then
                  istep_atm=istep_atm+1
               endif
            endif
c     ##################################################################
c     *** c-GOLDSTEIN sea-ice - UPDATE
            if (flag_goldsteinseaice) then
               if(mod(koverall,ksic_loop).eq.0) then
                  istep_sic=istep_sic+1
               endif
            endif
c     ##################################################################
c     *** c-GOLDSTEIN - UPDATE
            if (flag_ebatmos) then
               if(mod(koverall,kocn_loop).eq.1) then
                  istep_ocn=istep_ocn+1
               endif
            endif 
c     ##################################################################
c     *** ROKGEM model - UPDATE (GEMlite)
            if (flag_rokgem) then
               if (mod(koverall,conv_kocn_krokgem*kocn_loop).eq.0)
     &              then
c     nothing(!)
c     NOTE: ROKGEM not called so as to preserve the annual mean
c     of what might be a highly seasonal weathering flux
c     NOTE: in any case, climate is invarient (and so too weathering)
c     NOTE: removal of tracers from the atmosphere in a non
c     'short-cut' situation is not currently handled
c     call cpl_flux_rokatm_wrapper
               endif
            endif
c     ##################################################################
c     *** ECOGEM model - UPDATE (GEMlite)
            if (flag_ecogem) then
               if (mod(koverall,conv_kocn_kecogem*kocn_loop).eq.0)
     &              then
c     DO NOTHING! (currently)
               endif
            endif
c     ##################################################################
c     *** BIOGEM-GEMlite model - UPDATE
c     NOTE: *do not* update ATCHEM <atm> until end of GEMLITE phase
c     (interface values *are* updated for runtime reporting)
            if (flag_gemlite) then
               if (mod(koverall,kgemlite*kocn_loop).eq.0) then
                  if (debug_loop.gt.2) PRINT*,'>>> GEMLITE @ ',
     &                 koverall,kgemlite*kocn_loop
                  call gemlite_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
c     [genie-gemlite/cpl_comp_gemlite.f90]
c     write <datm>, <docn> anomolies to <genie_atm>, <genie_ocn>
                  if (debug_loop.gt.2) PRINT*,'* cpl_comp_gltgem'
                  call cpl_comp_gltgem_d_wrapper
c     [genie-biogem/cpl_comp_biogem.f90]
c     increment <sfcatm1> array with <genie_datm1>
c     => needed for reporting current <sfcatm1> from BIOGEM
                  call cpl_comp_gematm1_wrapper
c     [genie-biogem/cpl_comp_biogem.f90]
c     update biogem <ocn> array from <ocn> + <genie_docn>
c     => needed for reporting current <ocn> from BIOGEM
                  call cpl_comp_gemocn_wrapper
c     [genie-sedgem/cpl_comp_sedgem.f90]
c     convert <sfcocn1> (ocn grid) -> <sfcsumocn> (sed grid)
c     => needed by SEDGEM for update
c     NOTE: no time-sveraging (GEMLITE, SEDGEM both @ 1yr time-steps)
                  call cpl_comp_ocnsed_gem_wrapper
               endif
            endif
c     update BIOGEM diagnostics time points
c     NOTE: failure to do this risks BIOGEM losing track of 
c     which time-point to save time-series and slices at
c     NOTE: BUT ... you will not get an exactly eqilvilant e.g. 
c     annual average as GEMlite onyl updates at the very END
c     of its (yearly) time-step
c     (=> you will effectively get the start-of-year value
c     (repeated each time-step over the year)
c     rather than the yearly mid-point (or average))
c     i.e. ... the time-series is out of phase during the GEM segment
c     NOTE: also update BIOGEM forcing time-series [biogem_forcing]
c     (even though forcings are not applied in the GEMlite phase)
c     (although ... sub_update_sig will find the correct point 
c     in the forcing time-series regardless ...)
            if (flag_biogem) then
               if (mod(koverall,conv_kocn_kbiogem*kocn_loop).eq.0)
     &              then
                  if (debug_loop.gt.2) PRINT*,'>>> diag_biogem_gem @ ',
     &                 koverall,conv_kocn_kbiogem*kocn_loop
c     NOTE: the namelist parameter *gem_adapt_diag_biogem_full*
c     (an input parameter passed in the diag_biogem loop wrappers)
c     sets whether to update the current time point alone,
c     or also calculate a full time-series and slice average and save
                  call biogem_forcing_wrapper
                  call diag_biogem_gem_timeslice_wrapper
                  call diag_biogem_gem_timeseries_wrapper
                  if (debug_loop.gt.1) call diag_biogem_gem_wrapper
                  if (debug_loop.gt.2) PRINT*,'<<<'
               endif
            endif
c     ### ADAPT CYCLE STEP ###
c     extend GEM cycle phase if the cumulative DpCO2 change 
c     remains below the specified threshold
            if (flag_gemlite) then
               if (mod(koverall,kgemlite*kocn_loop).eq.0) then
c     update (@ current time-step, not annual average) value of gem_pCO2
                  call diag_biogem_pCO2_wrapper
                  if (gem_adapt_auto) then
c     if first GEM step of sequence => set initial pCO2 value
c     if not => test cumulative DpCO2 change for
c     excessive pCO2 drift has occurred
                     if (istep_gem.eq.(gem_notyr+1)) then
                        gem_pCO2_INIT = gem_pCO2
                        if (debug_loop.gt.1) 
     &                       PRINT*,'### SETTING: gem_pCO2_INIT',
     &                       gem_pCO2_INIT
                     else
                        if (debug_loop.gt.1) 
     &                       PRINT*,'### CURRENT: gem_pCO2',
     &                       gem_pCO2
                        if ( abs(gem_pCO2_INIT - gem_pCO2).lt. 
     &                       gem_adapt_DpCO2 ) then
c     decrement cycle phase counter to
c     prolongue operation in accelerated (GEM) time-stepping cycle phase
c     but ONLY if selected (some risk of spurious stuff happening
c     if staying in the GEM phase for ever ...)
                           if (gem_adapt_auto_unlimitedGEM) then
                              istep_gem = istep_gem - 1
                              if (debug_loop.gt.1) 
     &                             PRINT*,'### GEM phase extended :',
     &                             istep_gem,
     &                             ' / SUM(DpCO2) (ppm) =',
     &                             abs(gem_pCO2_INIT - gem_pCO2)
                           endif
                        else
                           if ( istep_gem.lt.
     &                          (gem_notyr+gem_yr-1) ) then
                              if (debug_loop.gt.0)
     &                             PRINT*,'### EXCESS DpCO2 => $$$'
                              istep_gem=gem_notyr+gem_yr-1
                           endif
                        endif
                     endif
                  endif
               endif
            endif
c     ##################################################################
c     *** ATCHEM model - UPDATE (GEMlite)
            if (flag_atchem) then
               if (mod(koverall,conv_kocn_katchem*kocn_loop).eq.0) 
     &              then
c     DO NOTHING! (currently)
               endif
            endif
c     ##################################################################
c     *** SEDGEM model - UPDATE (GEMlite)
            if (flag_sedgem) then
               if (mod(koverall,conv_kocn_ksedgem*kocn_loop).eq.0) 
     &              then
c     force (CaCO3) sediment aging
c     (in the absence of BIOGEM sedimentation age updating)
c     NOTE: strictly, upon the very first time this is called,
c     only 0.5 years should be subtracted
c     (cf. mid-point average created by summing BIOGEM output)
                  call sedgem_dsedage_wrapper
c     main SEDGEM routine
                  if(istep_gem.eq.(gem_notyr+gem_yr)) then
                     if (debug_loop.gt.2) PRINT*,'>>> SEDEGM @ ',
     &                    koverall,conv_kocn_ksedgem*kocn_loop
                     call sedgem_wrapper
                     if (debug_loop.gt.2) PRINT*,'<<<'
                  else
                     if (debug_loop.gt.2) PRINT*,'>>> SEDEGM_GLT @ ',
     &                    koverall,conv_kocn_ksedgem*kocn_loop
                     call sedgem_glt_wrapper
                     if (debug_loop.gt.2) PRINT*,'<<<'
                  endif
c     coupling routines
                  call cpl_flux_sedocn_wrapper
                  call cpl_comp_sedocn_wrapper
               endif
            endif
c     ##################################################################
c     ### END GEMLITE ##################################################
c     ##################################################################
         endif
c     
      enddo
c     
c     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !
c     ! *** MAIN TIME-STEPPING LOOP END ****************************** !
c     ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !

c     ******************************************************************
c     *** BIOGEOCHEM RUN-END RESTART SAVE ******************************
c     ******************************************************************

c     ==================================================================
c     *** ECOGEM model - RESTARTS
      if (flag_ecogem) then
         if (debug_loop.gt.1) PRINT*,'>>> SAVING ECOGEM RESTART <<<'
         call ecogem_save_restart_wrapper
      endif
c     ==================================================================
c     *** BIOGEM model - RESTARTS
      if (flag_biogem) then
         if (debug_loop.gt.1) PRINT*,'>>> SAVING BIOGEM RESTART <<<'
         call biogem_save_restart_wrapper
      endif
c     ==================================================================
c     *** ATCHEM model - RESTARTS
      if (flag_atchem) then
         call atchem_wrapper
         if (debug_loop.gt.1) PRINT*,'>>> SAVING ATCHEM RESTART <<<'
         call atchem_save_restart_wrapper
      endif
c     ==================================================================
c     *** SEDGEM model - RESTARTS
      if (flag_sedgem) then
         if (debug_loop.gt.1) PRINT*,'>>> SAVING SEDEGM RESTART <<<'
         call sedgem_save_restart_wrapper
      endif
c     ==================================================================
c     *** ROKGEM model - RESTARTS
      if (flag_rokgem) then
         if (debug_loop.gt.1) PRINT*,'>>> SAVING ROKGEM RESTART <<<'
         call rokgem_save_restart_wrapper
      endif
c     ==================================================================

c     ******************************************************************
c     *** SHUTDOWN *****************************************************
c     ******************************************************************

      print*
      print*,'*******************************************************'
      print*,' *** Simulation complete: shutdown starting ...'
      print*,'*******************************************************'
      print*

c     ==================================================================
c     *** GOLDSTEIN - END
      if (flag_goldsteinocean) then
         call end_goldstein
      endif
c     ==================================================================
c     *** EMBM - END
      if (flag_ebatmos) then
         call end_embm
      endif
c     ==================================================================
c     *** PLASIM - END
      if (flag_plasimatmos) then
         call epilog
         call mpstop
      endif
c     ==================================================================
c     *** GOLDSTEIN sea-ice - END
      if (flag_goldsteinseaice) then
         call end_seaice
      endif
c     ==================================================================
c     *** GEM model - END
      if (flag_atchem.or.flag_biogem.or.flag_sedgem.or.flag_rokgem) then
ccc   
      endif
c     ==================================================================
c     *** GOLDlite model - END
      if (flag_goldlite) then
         call end_goldlite_wrapper
      endif
c     ==================================================================
c     *** OCNlite model - END
      if (flag_ocnlite) then
         call end_ocnlite_wrapper
      endif
c     ==================================================================
c     *** ECOGEM model - END
      if (flag_ecogem) then
         call end_ecogem_wrapper
      endif
c     ==================================================================
c     *** BIOGEM model - END
      if (flag_biogem) then
         call end_biogem_wrapper
      endif
c     ==================================================================
c     *** ATCHEM model - END
      if (flag_atchem) then
         call end_atchem_wrapper
      endif
c     ==================================================================
c     *** SEDGEM model - END
      if (flag_sedgem) then
         call end_sedgem_wrapper
      endif
c     ==================================================================
c     *** ROKGEM model - END
      if (flag_rokgem) then
         call end_rokgem_wrapper
      endif
c     ==================================================================
c     *** GEMlite model - END
      if (flag_gemlite) then
         call end_gemlite_wrapper
      endif
c     ==================================================================
c     *** GEM model - END
      if (flag_atchem.or.flag_biogem.or.flag_sedgem.or.flag_rokgem) then
         call end_gem_wrapper
      endif
c     ==================================================================
c     *** GENIE - END
      call end_genie
c     ==================================================================

c     ******************************************************************
c     ******************************************************************
c     ******************************************************************

      print*
      print*,'*******************************************************'
      print*,' *** Shutdown complete; home time'
      print*,'*******************************************************'
      print*

      stop
      end

c     ******************************************************************
c     ******************************************************************
c     ******************************************************************

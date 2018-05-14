

MODULE genie_ini_wrappers

  ! ===========================================================
  ! This module contains wrapper subroutines to hide arg lists
  !             __Initialisation Routines Only__
  ! ===========================================================

  use genie_global

contains

  !!
  subroutine initialise_embm_wrapper
    implicit none
    call initialise_embm( &
         alon1_atm,alat1_atm, &
         alon2_atm,alat2_atm,alon3_atm,alat3_atm, &
         aboxedge1_lon_atm,aboxedge1_lat_atm, &
         aboxedge2_lon_atm,aboxedge2_lat_atm, &
         aboxedge3_lon_atm,aboxedge3_lat_atm, &
         ilandmask1_atm,ilandmask2_atm,ilandmask3_atm, &
         ias_out,iaf_out,ips_out,ipf_out,jsf_out, &
         tstar_ocn, &
         koverall_total, &
         co2_atm,ch4_atm,n2o_atm, &
         ocean_stressx2_ocn,ocean_stressy2_ocn, &
         ocean_stressx3_ocn,ocean_stressy3_ocn, &
         tstar_atm,surf_qstar_atm,atmos_dt_tim, &
         genie_solar_constant, &
         eb_rmax,eb_dphi,eb_rdtdim,eb_ca, &
         global_daysperyear, &
         torog_atm, &
         surf_orog_atm, &
         landice_slicemask_lic, &
         go_syr, &
         flag_ents, &
         atmos_lowestlu2_atm, &
         atmos_lowestlv3_atm &
         )
  end subroutine initialise_embm_wrapper

  !!
  subroutine initialise_plasim_wrapper
    implicit none
    call mpstart
    call prolog(go_nyear,kocn_loop,genie_sfcatm_lnd(3,1,1))
    call ini_steps(tstar_ocn,temp_sic,hght_sic,frac_sic,albd_sic)
  end subroutine initialise_plasim_wrapper

  !!
  subroutine ini_goldsteinseaice_wrapper
    implicit none
    call initialise_seaice( &
         alon1_sic,alat1_sic, &
         alon2_sic,alat2_sic,alon3_sic,alat3_sic, &
         aboxedge1_lon_sic,aboxedge1_lat_sic, &
         aboxedge2_lon_sic,aboxedge2_lat_sic, &
         aboxedge3_lon_sic,aboxedge3_lat_sic, &
         ilandmask1_sic,ilandmask2_sic, &
         ilandmask3_sic, &
         koverall_total, &
         hght_sic,frac_sic,temp_sic,albd_sic, &
         test_energy_seaice)
  end subroutine ini_goldsteinseaice_wrapper

  !!
  subroutine initialise_goldocean_wrapper
    implicit none
    call initialise_goldstein( &
         alon1_ocn,alat1_ocn, &
         alon2_ocn,alat2_ocn,alon3_ocn,alat3_ocn, &
         aboxedge1_lon_ocn,aboxedge1_lat_ocn, &
         aboxedge2_lon_ocn,aboxedge2_lat_ocn, &
         aboxedge3_lon_ocn,aboxedge3_lat_ocn, &
         depth1_ocn,depth2_ocn, &
         ilandmask1_ocn,ilandmask2_ocn, &
         ilandmask3_ocn,koverall_total, &
         tstar_ocn,sstar_ocn,ustar_ocn,vstar_ocn,albedo_ocn, &
         ias_out,iaf_out,ips_out,ipf_out,jsf_out, &
         lrestart_genie, & 
         go_saln0,go_rhoair,go_cd,go_ds,go_dphi,go_ips,go_ipf, &
         go_usc,go_dsc,go_fsc,go_rh0sc, &
         go_rhosc,go_cpsc,go_scf, &
         go_k1,go_dz,go_dza, &
         go_ias,go_iaf,go_jsf,go_c,go_cv,go_s,go_sv, &
         go_ts,go_ts1, &
         go_rsc,go_syr,go_nyear,go_lin,go_ec,go_istep0)
  end subroutine initialise_goldocean_wrapper

  !!
  subroutine initialise_gem_wrapper
    implicit none
    call initialise_gem ()
  end subroutine initialise_gem_wrapper

  !!
  subroutine initialise_goldlite_wrapper
    implicit none
    call initialise_goldlite(                                                 &
         & go_ts(intrac_ocn-n_intrac_col:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn), & ! input/output
         & go_ts1(intrac_ocn-n_intrac_col:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn) & ! input/output
         & )
  end subroutine initialise_goldlite_wrapper

  !!
  subroutine initialise_ocnlite_wrapper
    implicit none
    call initialise_ocnlite()
  end subroutine initialise_ocnlite_wrapper

  !!
  subroutine initialise_ecogem_wrapper
    implicit none
    call initialise_ecogem(                     &
         & egbg_sfcpart,                        & ! input/output -- change in particulate concentration field
         & egbg_sfcremin,                       & ! input/output -- change in remin concentratoin field
         & egbg_sfcocn,                         & ! input/output -- tracer concentrations
         & go_dsc,                              &
         & go_k1(1:ilon1_ocn,1:ilat1_ocn),      &
         & go_dz(1:inl1_ocn),go_dza(1:inl1_ocn),&
         & go_sv(0:ilat1_ocn)                   &
         & )
  end subroutine initialise_ecogem_wrapper

  !!
  subroutine initialise_biogem_wrapper
    implicit none
    call initialise_biogem(                                                                        &
         & go_saln0,go_rhoair,go_cd,go_ds,go_dphi,                                                 &
         & go_usc,go_dsc,go_fsc,go_rh0sc,                                                          &
         & go_rhosc,go_cpsc,genie_solar_constant,go_scf,                                           &
         & go_ips(1:ilat1_ocn),go_ipf(1:ilat1_ocn),go_ias(1:ilat1_ocn),go_iaf(1:ilat1_ocn),go_jsf, &
         & go_k1(1:ilon1_ocn,1:ilat1_ocn),                                                         &
         & go_dz(1:inl1_ocn),go_dza(1:inl1_ocn),                                                   &
         & go_c(0:ilat1_ocn),go_cv(0:ilat1_ocn),go_s(0:ilat1_ocn),go_sv(0:ilat1_ocn),              &
         & go_ts(1:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn),                                 &
         & go_ts1(1:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn),                                &
         & genie_sfcatm1,                                                                          &
         & genie_sfxatm1,                                                                          &
         & genie_sfcocn1,                                                                          &
         & genie_sfxocn1,                                                                          &
         & genie_sfcsed1,                                                                          &
         & genie_sfxsed1                                                                           &
         & )
  end subroutine initialise_biogem_wrapper

  !!
  subroutine initialise_atchem_wrapper
    implicit none
    call initialise_atchem ( &
         & go_c(0:ilat1_ocn),go_cv(0:ilat1_ocn),go_s(0:ilat1_ocn),go_sv(0:ilat1_ocn),              &
         & genie_sfxsumatm,  &
         & genie_sfcatm      &
         & )
   end subroutine initialise_atchem_wrapper

  !!
  subroutine initialise_sedgem_wrapper
    implicit none
    call initialise_sedgem ( &
         & genie_timestep,   & 
         & genie_sfxsumsed,  &
         & genie_sfcsumocn,  &
         & genie_sfcsed,     &
         & genie_sfxocn      &
         & )
  end subroutine initialise_sedgem_wrapper

  !! SG Initialising ENTS module
  subroutine initialise_ents_wrapper
    implicit none
    call initialise_ents( &
         go_lin,go_rsc,go_syr,go_nyear, &
         go_ds,go_dphi,inl1_ocn, &
         go_k1(1:ilon1_ocn,1:ilat1_ocn), &
         eb_rmax,eb_rdtdim, &
         tstar_atm,surf_qstar_atm,eb_ca,co2_atm, &
         global_daysperyear,alat1_ocn, &
         landice_slicemask_lic, &
         albs_atm, &
         land_albs_snow_lnd, &
         land_albs_nosnow_lnd, &
         land_snow_lnd, &
         land_bcap_lnd, &
         land_z0_lnd, &
         land_temp_lnd, &                                               ! output
         land_moisture_lnd, &                                           ! output
         intrac_atm_max, &
         genie_sfcatm_lnd, &
         genie_sfxatm_lnd &
         )
  end subroutine initialise_ents_wrapper

  !! SG Initialising ENTS module
  subroutine initialise_ents_wrapper2
    implicit none
    call initialise_ents( &
         go_lin,go_rsc,go_syr,go_nyear, &
         go_ds,go_dphi,inl1_ocn, &
         go_k1(1:ilon1_ocn,1:ilat1_ocn), &
         eb_rmax,eb_rdtdim, &
         tstar_atm,surf_qstar_atm,eb_ca,co2_atm, &
         global_daysperyear,alat1_ocn, &
         landice_slicemask_lic, &
         albs_atm, &
         land_albs_snow_lnd, &
         land_albs_nosnow_lnd, &
         land_snow_lnd, &
         land_bcap_lnd, &
         land_z0_lnd, &
         land_temp_lnd, &                                               ! output
         land_moisture_lnd, &                                           ! output
         intrac_atm_max, &
         genie_sfcatm_lnd, &
         genie_sfxatm_lnd &
         )
  end subroutine initialise_ents_wrapper2

  !!
  subroutine initialise_rokgem_wrapper
    implicit none
    call initialise_rokgem ( &
         & genie_timestep,   & 
         & genie_sfxrok,     &
         & genie_sfxsumrok1  &
         & )
   end subroutine initialise_rokgem_wrapper

  !!
  subroutine initialise_gemlite_wrapper
    implicit none
    call initialise_gemlite(                     &                      ! 
         & go_dsc,                               &                      ! 
         & go_k1(1:ilon1_ocn,1:ilat1_ocn),       &                      ! 
         & go_dz(1:inl1_ocn),go_dza(1:inl1_ocn), &                      ! 
         & go_sv(0:ilat1_ocn),                   &                      ! 
         & genie_sfxsumsed,                      &                      !
         & genie_sfxsumrok1_gem,                 &
         & genie_sfxsumatm1_gem                  &
         & )
  end subroutine initialise_gemlite_wrapper

END MODULE genie_ini_wrappers

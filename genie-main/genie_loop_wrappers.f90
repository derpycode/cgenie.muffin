

MODULE genie_loop_wrappers

  use omp_lib
  use genie_global

contains

  !!
  subroutine surf_ocn_sic_wrapper
    implicit none
    ! Surflux module : GOLDSTEIN-GOLDSEAICE (parentage = c-GOLDSTEIN)
    !
    ! Inputs :  tstar_ocn                ocean surface temperature
    !           sstar_ocn                ocean surface salinity
    !           albedo_ocn               ocean albedo
    !           tstar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           surf_pres_atm            surface pressure
    !           surf_hght_atm            surface height (atmosphere)
    !           hght_sic                 sea-ice height
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature
    !           albd_sic                 sea-ice albedo (also an output)
    !           ocean_lowestlu2_ocn      surface wind speed (x) at u point
    !           ocean_lowestlv2_ocn      surface wind speed (y) at u point
    !           ocean_lowestlu3_ocn      surface wind speed (x) at v point
    !           ocean_lowestlv3_ocn      surface wind speed (y) at v point
    !           netsolar_ocnsic          net short-wave to ocean + sea-ice
    !           netlong_ocnsic           net long-wave to ocean + sea-ice
    !           albavg_ocn               average ocean grid cell albedo
    !           rough_ocn                ocean roughness
    !           ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    !           latent_ocn               latent heat flux
    !           sensible_ocn             sensible heat flux
    !           netsolar_ocn             net short-wave heat flux to ocean only
    !           netlong_ocn              net long-wave heat flux to ocean only
    !           evap_ocn                 evaporation
    !           surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           evap_atm                 evaporation
    !           dhght_sic                change in sea-ice height
    !           dfrac_sic                change in sea-ice fractional cover
    call surf_ocn_sic( &
         istep_gsurf, &               !
         tstar_ocn, sstar_ocn, albedo_ocn, &        ! input
         ocean_lowestlt_ocn, ocean_lowestlq_ocn, &  ! input
         ocean_lowestlp_ocn, ocean_lowestlh_ocn, &  ! input
         hght_sic, frac_sic, temp_sic, albd_sic, &  ! input (temp_sic is input AND output!)
         ocean_lowestlu2_ocn,ocean_lowestlv2_ocn, & ! input
         ocean_lowestlu3_ocn,ocean_lowestlv3_ocn, & ! input
         ocean_atm_netsolar_ocn, &                  ! input
         ocean_atm_netlong_ocn, &                   ! input
         albavg_ocn, &                              ! input
         rough_ocn, &                               ! output
         ocean_stressx2_ocn, ocean_stressy2_ocn, &  ! output
         ocean_stressx3_ocn, ocean_stressy3_ocn, &  ! output
         ocean_latent_ocn,ocean_sensible_ocn, &     ! input/output
         ocean_netsolar_ocn,ocean_netlong_ocn, &    ! output
         ocean_evap_ocn, &                          ! output
         atmos_latent_ocn,atmos_sensible_ocn, &     ! output
         atmos_evap_ocn, &                          ! output
         dhght_sic,dfrac_sic, &                     ! output
         test_energy_seaice, &                      ! input/output
         weight_ocn)                                ! input
  end subroutine surf_ocn_sic_wrapper

  !!
  subroutine surflux_wrapper
    implicit none
    ! Surflux module : GOLDSTEIN-EMBM-GOLDSEAICE (parentage = c-GOLDSTEIN)
    !
    ! Inputs :  tstar_ocn                ocean surface temperature
    !           sstar_ocn                ocean surface salinity
    !           tstar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           hght_sic                 sea-ice height
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature
    !           albd_sic                 sea-ice albedo
    !           ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    ! Outputs : albedo_ocn               ocean albedo (excl. sea-ice)
    !           latent_ocn               latent heat flux
    !           sensible_ocn             sensible heat flux
    !           netsolar_ocn             net short-wave heat flux
    !           netlong_ocn              net long-wave heat flux
    !           evap_ocn                 evaporation
    !           precip_ocn               precipitation
    !           runoff_ocn               runoff to ocean
    !           runoff_land              runoff on land
    !           surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           netsolar_atm             net short-wave heat flux
    !           netlong_atm              net long-wave heat flux
    !           evap_atm                 evaporation
    !           precip_atm               precipitation (not used normally)
    !           dhght_sic                change in sea-ice height
    !           dfrac_sic                change in sea-ice fractional cover
    !           atmos_lowestlh_atm       Height of lowest atmos level (m)
    !           atmos_lowestlu2_atm      zonal component of wind speed
    !           atmos_lowestlv3_atm      meridional component of wind speed
    !
    call surflux( &
         istep_ocn, &                             !
         tstar_ocn, sstar_ocn, &                  ! input
         tstar_atm, surf_qstar_atm, &             ! input
         hght_sic,frac_sic,temp_sic,albd_sic, &   ! input
         ocean_stressx2_ocn,ocean_stressy2_ocn, & ! input
         ocean_stressx3_ocn,ocean_stressy3_ocn, & ! input
         albedo_ocn,latent_ocn,sensible_ocn, &    ! output
         netsolar_ocn,netlong_ocn, &              ! output
         evap_ocn,precip_ocn, &                   ! output
         runoff_ocn,runoff_land, &                ! output
         surf_latent_atm,surf_sensible_atm, &     ! output
         netsolar_atm,netlong_atm, &              ! output
         evap_atm,precip_atm, &                   ! output
         dhght_sic,dfrac_sic, &                   ! output
         atmos_lowestlh_atm, &                    ! output
         go_solfor,go_fxsw, &                     ! output (to BIOGEM)
         intrac_atm_max, &                        ! input
         genie_sfcatm1, &                         ! input (from ATCHEM)
         eb_ca,global_daysperyear, &
         eb_fx0a,eb_fx0o,eb_fxsen,eb_fxlw, &
         eb_evap,eb_pptn,eb_relh, &               ! (also output to BIOGEM)
         eb_uv, &                                 ! output (to BIOGEM)
         eb_usurf, &                              ! output (to BIOGEM)
         genie_solar_constant, &
         co2_atm,ch4_atm,n2o_atm, &               ! output
         surf_orog_atm, &                         ! in/output
         landice_slicemask_lic, &                 ! in/output
         albs_atm, &                              ! in/output
         land_albs_snow_lnd, &                    ! input
         land_albs_nosnow_lnd, &                  ! input
         land_snow_lnd, &                         ! in/output
         land_bcap_lnd, &                         ! in/output
         land_z0_lnd, &                           ! in/output
         land_temp_lnd, &                         ! in/output
         land_moisture_lnd, &                     ! in/output
         flag_ents, &                             ! input
         atmos_lowestlu2_atm, &                   ! in/output
         atmos_lowestlv3_atm &                    ! in/output
         )
    !
  end subroutine surflux_wrapper

  !!
  subroutine plasim_surflux_wrapper
    implicit none
    call surflux_goldstein_seaice( &
!input
         hght_sic,frac_sic, &
         tstar_ocn,sstar_ocn, &
         surft_atm_sic,surfq_atm_sic,surfp_atm_sic, &
         insolar_sic,inlong_sic, &
         netheat_sic, &
         surf_windspeed_sic, &
         evap_ocn, &
         latent_coeff_atm, &
         sensible_coeff_atm, &
! input and output
         latent_ocn, sensible_ocn, &
         netsolar_ocn, netlong_ocn, &
! output
         dhght_sic,dfrac_sic,temp_sic,albd_sic, &
         delta_flux &
    )
    !
  end subroutine plasim_surflux_wrapper

  !!
  subroutine embm_wrapper
    implicit none
    ! Atmosphere module : EMBM (parentage = c-GOLDSTEIN)
    !
    ! Inputs  : surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           surf_netsolar_atm        net short-wave heat flux
    !           surf_netlong_atm         net long-wave heat flux
    !           evap_atm                 evaporation
    !           precip_atm               precipitation (not used)
    ! Outputs : ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    !           tatar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           atmos_lowestlu2_atm      zonal component of wind speed
    !           atmos_lowestlv3_atm      meridional component of wind speed
    call embm( &
         istep_atm, &                             ! 
         surf_latent_atm,surf_sensible_atm, &     ! input
         netsolar_atm,netlong_atm, &              ! input
         evap_atm,precip_atm, &                   ! input
         ocean_stressx2_ocn,ocean_stressy2_ocn, & ! output
         ocean_stressx3_ocn,ocean_stressy3_ocn, & ! output
         tstar_atm,surf_qstar_atm, &              ! output
         koverall, &                              !
         torog_atm, &                             ! output
         surf_orog_atm,&                          ! input
         flag_ents, &                             ! input
         atmos_lowestlu2_atm, &                   ! in/output
         atmos_lowestlv3_atm &                    ! in/output
         )
  end subroutine embm_wrapper

  !!
  subroutine plasim_wrapper
    implicit none
!    real test(64,32)
!    test=genie_sfxatm_lnd(3,:,:)
!    print*,size(test),kind(test)
!    print*,istep_atm,kind(istep_atm)
!    print*,size(tstar_ocn),kind(tstar_ocn),sum(tstar_ocn)
!    print*,size(temp_sic),kind(temp_sic),sum(temp_sic)
!    print*,size(hght_sic),kind(hght_sic),sum(hght_sic)
!    print*,size(frac_sic),kind(frac_sic),sum(frac_sic)
!    print*,size(albd_sic),kind(albd_sic),sum(albd_sic)
!    print*,size(delta_flux),kind(delta_flux),sum(delta_flux)
!    print*,size(surf_latent_atm),kind(surf_latent_atm),sum(surf_latent_atm)
!    print*,size(surf_latent_coeff_atm),kind(surf_latent_coeff_atm),sum(surf_latent_coeff_atm)
!    print*,size(surf_sensible_atm),kind(surf_sensible_atm),sum(surf_sensible_atm)
!    print*,size(surf_sensible_coeff_atm),kind(surf_sensible_coeff_atm),sum(surf_sensible_coeff_atm)
!    print*,size(netsolar_atm),kind(netsolar_atm),sum(netsolar_atm)
!    print*,size(netlong_atm),kind(netlong_atm),sum(netlong_atm)
!    print*,size(insolar_atm),kind(insolar_atm),sum(insolar_atm)
!    print*,size(inlong_atm),kind(inlong_atm),sum(inlong_atm)
!    print*,size(netheat_atm),kind(netheat_atm),sum(netheat_atm)
!    print*,size(surft_atm),kind(surft_atm),sum(surft_atm)
!    print*,size(surfq_atm),kind(surfq_atm),sum(surfq_atm)
!    print*,size(surfp_atm),kind(surfp_atm),sum(surfp_atm)
!    print*,size(evap_atm),kind(evap_atm),sum(evap_atm)
!    print*,size(precip_atm),kind(precip_atm),sum(precip_atm)
!    print*,size(land_runoff_atm),kind(land_runoff_atm),sum(land_runoff_atm)
!    print*,size(surf_stressx2_atm),kind(surf_stressx2_atm),sum(surf_stressx2_atm)
!    print*,size(surf_stressy2_atm),kind(surf_stressy2_atm),sum(surf_stressy2_atm)
!    print*,size(surf_stressx3_atm),kind(surf_stressx3_atm),sum(surf_stressx3_atm)
!    print*,size(surf_stressy3_atm),kind(surf_stressy3_atm),sum(surf_stressy3_atm)
!    print*,size(surf_windspeed_atm),kind(surf_windspeed_atm),sum(surf_windspeed_atm)
!    print*,size(solfor_atm),kind(solfor_atm),sum(solfor_atm)
    call master(istep_atm, &
!input
      tstar_ocn, &
      temp_sic,hght_sic,frac_sic,albd_sic, &
      delta_flux, &
      genie_sfcatm_lnd(3,1,1), &
!output
      genie_sfxatm_lnd(3,:,:), &
      surf_latent_atm,surf_latent_coeff_atm, &
      surf_sensible_atm,surf_sensible_coeff_atm, &
      netsolar_atm,netlong_atm, &
      insolar_atm,inlong_atm,netheat_atm, &
      surft_atm,surfq_atm,surfp_atm, &
      evap_atm,precip_atm,land_runoff_atm, &
      surf_stressx2_atm,surf_stressy2_atm, &
      surf_stressx3_atm,surf_stressy3_atm, &
      surf_windspeed_atm,solfor_atm &
    )
    !
  end subroutine plasim_wrapper

  !!
  subroutine radfor_wrapper
    implicit none
    ! 
    call radfor(istep_atm, global_daysperyear, &
         genie_solar_constant,                 &
         flag_ents                             &  ! input
         & )                                      ! input
  end subroutine radfor_wrapper

  !!
  subroutine gold_seaice_wrapper
    implicit none
    ! Sea-ice module : GOLDSTEIN sea-ice (parentage = c-GOLDSTEIN)
    !
    ! Inputs  : dhght_sic                change in sea-ice height
    !           dfrac_sic                change in sea-ice fractional cover
    !           ustar_ocn                surface ocean velocity (u)
    !           vstar_ocn                surface ocean velocity (v)
    ! Outputs : hght_sic                 sea-ice height
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature } technically
    !           albd_sic                 sea-ice albedo              } inputs
    !           waterflux_ocn            freshwater flux to ocean (melting)
    !           conductflux_ocn          heat flux to ocean (melting)
    !
    call gold_seaice( &
         istep_sic, &                            !
         dhght_sic,dfrac_sic, &                  ! input
         ustar_ocn,vstar_ocn, &                  ! input
         hght_sic,frac_sic,temp_sic,albd_sic, &  ! output
         waterflux_ocn,conductflux_ocn, &        ! output
         test_energy_seaice,test_water_seaice, & ! output
         koverall)                               !
  end subroutine gold_seaice_wrapper

  !!
  subroutine goldstein_wrapper
    ! Ocean module : GOLDSTEIN (parentage = c-GOLDSTEIN)
    ! Inputs  : latent_ocn               latent heat flux
    !           sensible_ocn             sensible heat flux
    !           netsolar_ocn             net short-wave heat flux
    !           netlong_ocn              net long-wave heat flux
    !           conductflux_ocn          heat flux to ocean (sea-ice melting)
    !           evap_ocn                 evaporation
    !           precip_ocn               precipitation
    !           runoff_ocn               runoff
    !           waterflux_ocn            freshwater flux to ocean (sea-ice melting)
    !           stressx_ocn              surface wind stress (x) on ocean
    !           stressy_ocn              surface wind stress (y) on ocean
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature
    ! Outputs : tstar_ocn                ocean surface temperature
    !           sstar_ocn                ocean surface salinity
    !           ustar_ocn                surface ocean velocity (u)
    !           vstar_ocn                surface ocean velocity (v)
    !
    implicit none
    !
    integer ::  thread_id
    if (debug_loop.gt.2) then
       thread_id = omp_get_thread_num()
       print *,"*m* goldstein_wrapper #thread: ",thread_id
    end if
    call goldstein( &
         istep_ocn, &                              !
         latent_ocn,sensible_ocn, &                ! input
         netsolar_ocn,netlong_ocn, &               ! input
         conductflux_ocn, &                        ! input
         evap_ocn,precip_ocn,runoff_ocn, &         ! input
         waterflux_ocn, &                          ! input
         ocean_stressx2_ocn,ocean_stressy2_ocn, &  ! input
         ocean_stressx3_ocn,ocean_stressy3_ocn, &  ! input
         tstar_ocn,sstar_ocn, &                    ! output
         ustar_ocn,vstar_ocn, &                    ! output
         albedo_ocn, &                             ! output
         test_energy_ocean,test_water_ocean, &     ! output
         koverall, &                               ! input
         go_ts,go_ts1, &                           ! output (to BIOGEM)
         go_cost,go_uvw,go_tau, &                  ! output (to BIOGEM)
         go_psi, &                                 ! output (to BIOGEM)
         go_mldta, &                               ! output (to BIOGEM)
         go_rho, &                                 ! output (to ENTS)
         go_diffv, &                               ! output (to biogem)
         go_dzrho)                                 ! output (to biogem) 
  end subroutine goldstein_wrapper
  !!
  subroutine ents_wrapper
    implicit none
    call ents(istep_ocn,go_nyear, &
         torog_atm, &
         co2_atm, &
         go_rh0sc,go_rhosc,go_rsc,go_ds,go_dphi, &
         go_dsc,go_saln0,go_dz,go_ec,go_rho, &
         eb_fx0a,eb_fx0o,eb_fxsen,eb_fxlw, &
         eb_evap,eb_pptn,eb_relh,go_istep0, &
         el_photo,el_respveg,el_respsoil,el_leaf, & ! GHC - added these for use with rokgem
         landice_slicemask_lic, &
         albs_atm, &
         land_albs_snow_lnd, &
         land_albs_nosnow_lnd, &
         land_snow_lnd, &
         land_bcap_lnd, &                         ! output
         land_z0_lnd, &                           ! output
         land_temp_lnd, &                         ! input
         land_moisture_lnd, &                     ! input
         intrac_atm_max, &
         genie_sfcatm_lnd, &
         genie_sfxatm_lnd &
         )
  end subroutine ents_wrapper

  !!
  subroutine cpl_flux_ocnatm_wrapper
    implicit none
    call cpl_flux_ocnatm(                                    &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & intrac_atm_max,                                   & ! input
         & ilon1_atm,ilat1_atm,                              & ! input
         & ilon1_ocn,ilat1_ocn,                              & ! input
         & genie_sfxatm1,                                    & ! input/output
         & genie_sfxsumatm                                   & ! input/output
         & )
  end subroutine cpl_flux_ocnatm_wrapper

  !!
  subroutine cpl_flux_lndatm_wrapper
    implicit none
    call cpl_flux_lndatm(                                    &
         & real(klnd_loop)*genie_timestep,                   & ! input
         & intrac_atm_max,                                   & ! input
         & ilon1_atm,ilat1_atm,                              & ! input
         & ilon1_lnd,ilat1_lnd,                              & ! input
         & genie_sfxatm_lnd,                                 & ! input/output
         & genie_sfxsumatm                                   & ! input/output
         & )
  end subroutine cpl_flux_lndatm_wrapper

  !!
  subroutine cpl_comp_lndEMBM_wrapper
    implicit none
    call cpl_comp_EMBM(         &
         & intrac_atm_max,      & ! input
         & ilon1_atm,ilat1_atm, & ! input
         & ilon1_lnd,ilat1_lnd, & ! input
         & tstar_atm,           & ! input (surface temperature)
         & surf_qstar_atm,      & ! input (surface specific humidity)
         & genie_sfcatm_lnd     & ! input/output
         & )
  end subroutine cpl_comp_lndEMBM_wrapper

  !!
  subroutine cpl_flux_ocnsed_wrapper
    implicit none
    call cpl_flux_ocnsed(                                    &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, &          ! input
         & intrac_sed_max,                                   &          ! input
         & ilon1_ocn,ilat1_ocn,                              &          ! input
         & ilon1_sed,ilat1_sed,                              &          ! input
         & genie_sfxsed1,                                    &          ! input/output
         & genie_sfxsumsed                                   &          ! input/output
         & )
  end subroutine cpl_flux_ocnsed_wrapper

  !!
  subroutine cpl_flux_sedocn_wrapper
    implicit none
    call cpl_flux_sedocn(       &
         & intrac_ocn_max,      & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & ilon1_sed,ilat1_sed, & ! input
         & genie_sfxocn1,       & ! input/output
         & genie_sfxocn         & ! output
         & )
  end subroutine cpl_flux_sedocn_wrapper

  !!
  subroutine cpl_flux_sedsed1_wrapper
    implicit none
    call cpl_flux_sedsed1(                                   &
         & intrac_sed_max,                                   &          ! input
         & ilon1_ocn,ilat1_ocn,                              &          ! input
         & ilon1_sed,ilat1_sed,                              &          ! input
         & genie_sfxsumsed,                                  &          ! input
         & genie_sfxsumsed1                                  &          ! output
         & )
  end subroutine cpl_flux_sedsed1_wrapper

  !!
  subroutine cpl_comp_ocnsed_wrapper
    implicit none
    call cpl_comp_ocnsed(                       &
         & int(koverall/kocn_loop),             &
         & conv_kocn_kbiogem,conv_kocn_ksedgem, &
         & intrac_ocn_max,                      & ! input
         & ilon1_ocn,ilat1_ocn,                 & ! input
         & ilon1_sed,ilat1_sed,                 & ! input
         & genie_sfcocn1,                       & ! input
         & genie_sfcsumocn                      & ! input/output
         & )
  end subroutine cpl_comp_ocnsed_wrapper

  !!
  ! NOTE: set the three time-step integers equal to effectively force:
  !       <genie_sfcsumocn> = <genie_sfcocn1>
  !       (no e.g. annual averaging of the ocean array is needed in the case of the annual GEMlite tiem-step)
  subroutine cpl_comp_ocnsed_gem_wrapper
    implicit none
    call cpl_comp_ocnsed(                       &
         & conv_kocn_ksedgem,                   &
         & conv_kocn_ksedgem,conv_kocn_ksedgem, &
         & intrac_ocn_max,                      & ! input
         & ilon1_ocn,ilat1_ocn,                 & ! input
         & ilon1_sed,ilat1_sed,                 & ! input
         & genie_sfcocn1,                       & ! input
         & genie_sfcsumocn                      & ! input/output
         & )
  end subroutine cpl_comp_ocnsed_gem_wrapper

  !!
  subroutine cpl_comp_sedocn_wrapper
    implicit none
    call cpl_comp_sedocn(       &
         & intrac_sed_max,      & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & ilon1_sed,ilat1_sed, & ! input
         & genie_sfcsed1,       & ! input
         & genie_sfcsed         & ! input/output
         & )
  end subroutine cpl_comp_sedocn_wrapper

  !!
  subroutine rokgem_wrapper
    implicit none
    call rokgem(              &
         & real(conv_kocn_krokgem*kocn_loop)*genie_timestep, & ! input
         & genie_sfcatm1,     & ! input
         & runoff_land,       & ! input
         & el_photo,          & ! input
         & el_respveg,        & ! input
         & genie_sfxrok,      & ! input/output
         & genie_sfxatm1      & ! input/output
         & )
  end subroutine rokgem_wrapper

  !!
  subroutine cpl_flux_rokatm_wrapper
    implicit none
    call cpl_flux_rokatm(       &
         & real(conv_kocn_krokgem*kocn_loop)*genie_timestep,&
         & intrac_atm_max,      & ! input
         & ilon1_rok,ilat1_rok, & ! input
         & ilon1_atm,ilat1_atm, & ! input
         & genie_sfxatm1,       & ! input/output
         & genie_sfxsumatm,     & ! input/output
         & .false.              & ! input
         & )
  end subroutine cpl_flux_rokatm_wrapper

  !!
  subroutine cpl_flux_rokatm_gem_wrapper
    implicit none
    call cpl_flux_rokatm(       &
         & real(conv_kocn_krokgem*kocn_loop)*genie_timestep,&
         & intrac_atm_max,       & ! input
         & ilon1_rok,ilat1_rok,  & ! input
         & ilon1_atm,ilat1_atm,  & ! input
         & genie_sfxatm1,        & ! input/output
         & genie_sfxsumatm1_gem, & ! input/output
         & .true.                & ! input
         & )
  end subroutine cpl_flux_rokatm_gem_wrapper

!!$  !!
!!$  subroutine reinit_flux_rokatm_wrapper
!!$    implicit none
!!$    call reinit_flux_rokatm(    &
!!$         & genie_sfxsumatm1     & ! input/output
!!$         & )
!!$  end subroutine reinit_flux_rokatm_wrapper

  !!
  subroutine reinit_flux_rokatm_gem_wrapper
    implicit none
    call reinit_flux_rokatm(                                 &
         & genie_sfxsumatm1_gem                              & ! input/output
         & )
  end subroutine reinit_flux_rokatm_gem_wrapper

  !!
  subroutine cpl_flux_rokocn_wrapper
    implicit none
    call cpl_flux_rokocn(       &
         & real(conv_kocn_krokgem*kocn_loop)*genie_timestep,&
         & intrac_ocn_max,      & ! input
         & ilon1_rok,ilat1_rok, & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & genie_sfxrok,        & ! input
         & genie_sfxsumrok1,    & ! input/output
         & .false.              & ! input
         & )
  end subroutine cpl_flux_rokocn_wrapper

  !!
  subroutine cpl_flux_rokocn_gem_wrapper
    implicit none
    call cpl_flux_rokocn(                                    &
         & real(conv_kocn_krokgem*kocn_loop)*genie_timestep, &
         & intrac_ocn_max,                                   & ! input
         & ilon1_rok,ilat1_rok,                              & ! input
         & ilon1_ocn,ilat1_ocn,                              & ! input
         & genie_sfxrok,                                     & ! input
         & genie_sfxsumrok1_gem,                             & ! input/output
         & .true.                                            & ! input
         & )
  end subroutine cpl_flux_rokocn_gem_wrapper

  !!
  subroutine reinit_flux_rokocn_wrapper
    implicit none
    call reinit_flux_rokocn(    &
         & genie_sfxsumrok1     & ! input/output
         & )
  end subroutine reinit_flux_rokocn_wrapper

  !!
  subroutine reinit_flux_rokocn_gem_wrapper
    implicit none
    call reinit_flux_rokocn(                                 &
         & genie_sfxsumrok1_gem                              & ! input/output
         & )
  end subroutine reinit_flux_rokocn_gem_wrapper

  !!
  subroutine cpl_comp_rokEMBM_wrapper
    !    call cpl_comp_rokEMBM(      &
    !         & )
  end subroutine cpl_comp_rokEMBM_wrapper

  !!
  subroutine rokgem_save_restart_wrapper
    implicit none
    call rest_rokgem()
  end subroutine rokgem_save_restart_wrapper


! ******************************************************************************************************************************** !
! *** BIOGEM ********************************************************************************************************************* !
! ******************************************************************************************************************************** !

  !!
  subroutine biogem_wrapper
    implicit none
    !
    integer ::  thread_id
    if (debug_loop.gt.2) then
       thread_id = omp_get_thread_num()
       print *,"*m* biogem_wrapper #thread: ",thread_id
    end if
    call biogem(                                             &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input/output
         & genie_sfxatm1,                                    & ! input/output
         & genie_sfcocn1,                                    & ! input/output
         & genie_sfxocn1,                                    & ! input/output
         & genie_sfcsed1,                                    & ! input/output
         & genie_sfxsed1,                                    & ! input/output
         & genie_sfxsumrok1                                  & ! input/output
         )
  end subroutine biogem_wrapper

  !!
  subroutine biogem_tracercoupling_wrapper
    implicit none
    call biogem_tracercoupling(                              &
         & go_ts,                                            & ! input/output
         & go_ts1,                                           & ! input/output
	 & genie_clock,                                     & ! input (matrix)
         & egbg_sfcpart,                                     & ! input
         & egbg_sfcremin,                                    & ! input
         & egbg_sfcocn                                       & ! output
         )
  end subroutine biogem_tracercoupling_wrapper

  !!
  subroutine biogem_forcing_wrapper
    implicit none
    call biogem_forcing(                                     &
         & genie_clock                                       & ! input
         )
  end subroutine biogem_forcing_wrapper

  !!
  subroutine biogem_climate_wrapper
    implicit none
    call biogem_climate(                                     &
         & hght_sic,                                         & ! input
         & frac_sic,                                         & ! input
         & go_cost,                                          & ! input/output
         & go_solfor,                                        & ! input
         & go_fxsw,                                          & ! input
         & go_uvw,                                           & ! input
         & go_tau,                                           & ! input
         & go_psi,                                           & ! input
         & eb_uv,                                            & ! input
         & eb_usurf,                                         & ! input
         & go_mldta,                                         & ! input
         & eb_evap,                                          & ! input
         & eb_pptn,                                          & ! input
         & genie_solar_constant,                             & ! input/output
         & go_diffv,                                         & ! input
         & go_dzrho,                                         & ! input
         & go_rho                                            & ! input 
         )
  end subroutine biogem_climate_wrapper
  !!
  subroutine biogem_climate_sol_wrapper
    implicit none
    call biogem_climate_sol(                                 &
         & go_solfor,                                        & ! input
         & go_fxsw,                                          & ! input
         & genie_solar_constant                              & ! input/output
         )
  end subroutine biogem_climate_sol_wrapper

  !!
  subroutine diag_biogem_timesync_wrapper
    implicit none
    call diag_biogem_timesync( &
         & genie_tseries,      & ! output
         & genie_tslice,       & ! output
         & genie_intseries,    & ! output
         & genie_intslice,     & ! output
         & genie_endseries,    & ! output
         & genie_endslice      & ! output
         )
  end subroutine diag_biogem_timesync_wrapper
  !!
  subroutine diag_biogem_wrapper
    implicit none
    call diag_biogem(       &
         & genie_clock,     & ! input
         & genie_sfcatm1,   & ! input
         & .false.          & ! input
         )
  end subroutine diag_biogem_wrapper

  !!
  subroutine diag_biogem_gem_wrapper
    implicit none
    call diag_biogem(     &
         & genie_clock,   & ! input
         & genie_sfcatm1, & ! input
         & .true.         & ! input
         )
  end subroutine diag_biogem_gem_wrapper

  !!
  subroutine diag_biogem_pCO2_wrapper
    implicit none
    call diag_biogem_pCO2(  &
         & genie_sfcatm1, & ! input
         & gem_pCO2       & ! input/output
         )
  end subroutine diag_biogem_pCO2_wrapper

  !!
  subroutine diag_biogem_ecogem_wrapper
    implicit none
    call diag_biogem_ecogem( &
         & egbg_sfcpart,     &
         & egbg_sfcremin     &
         )
  end subroutine diag_biogem_ecogem_wrapper

  !!
  subroutine diag_biogem_timeslice_wrapper
    implicit none
    call diag_biogem_timeslice(                              &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input
         & genie_sfxatm1,                                    & ! input
         & genie_sfxocn1,                                    & ! input
         & genie_sfcsed1,                                    & ! input
         & genie_sfxsed1,                                    & ! input
         & genie_sfxsumrok1,                                 & ! input
         & .true.,                                           & ! input
         & .false.                                           & ! input
         )
  end subroutine diag_biogem_timeslice_wrapper

  !!
  subroutine diag_biogem_gem_timeslice_wrapper
    implicit none
    call diag_biogem_timeslice(                              &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input
         & genie_sfxatm1,                                    & ! input
         & genie_sfxocn1,                                    & ! input
         & genie_sfcsed1,                                    & ! input
         & genie_sfxsed1,                                    & ! input
         & genie_sfxsumrok1_gem,                             & ! input
         & gem_adapt_diag_biogem_full,                       & ! input
         & .true.                                            & ! input
         )
  end subroutine diag_biogem_gem_timeslice_wrapper

  !!
  subroutine diag_biogem_timeseries_wrapper
    implicit none
    call diag_biogem_timeseries(                             &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input
         & genie_sfxatm1,                                    & ! input
         & genie_sfxocn1,                                    & ! input
         & genie_sfcsed1,                                    & ! input
         & genie_sfxsed1,                                    & ! input
         & genie_sfxsumrok1,                                 & ! input
         & .true.,                                           & ! input
         & .false.,                                          & ! input
         & .false.                                           & ! input
         )
  end subroutine diag_biogem_timeseries_wrapper

  !!
  subroutine diag_biogem_force_timeseries_wrapper
    implicit none
    call diag_biogem_timeseries(                             &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input
         & genie_sfxatm1,                                    & ! input
         & genie_sfxocn1,                                    & ! input
         & genie_sfcsed1,                                    & ! input
         & genie_sfxsed1,                                    & ! input
         & genie_sfxsumrok1,                                 & ! input
         & .true.,                                           & ! input
         & .true.,                                           & ! input
         & .false.                                           & ! input
         )
  end subroutine diag_biogem_force_timeseries_wrapper

  !!
  subroutine diag_biogem_gem_timeseries_wrapper
    implicit none
    call diag_biogem_timeseries(                             &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input
         & genie_sfxatm1,                                    & ! input
         & genie_sfxocn1,                                    & ! input
         & genie_sfcsed1,                                    & ! input
         & genie_sfxsed1,                                    & ! input
         & genie_sfxsumrok1_gem,                             & ! input
         & gem_adapt_diag_biogem_full,                       & ! input
         & .false.,                                          & ! input
         & .true.                                            & ! input
         )
  end subroutine diag_biogem_gem_timeseries_wrapper
  
  !!
  subroutine diag_biogem_rec_orb_wrapper
    implicit none
    call diag_biogem_rec_orb( &
         & genie_clock,       & ! input
         & genie_sfcatm1      & ! input/output
         )
  end subroutine diag_biogem_rec_orb_wrapper

  !!
  subroutine cpl_comp_ocngem_wrapper
    implicit none
    call cpl_comp_ocngem(                                    &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, &          ! input
         & intrac_ocn_max,                                   &          ! input
         & ilon1_ocn,ilat1_ocn,inl1_ocn,                     &          ! input
         & genie_ocn                                         &          ! output
         )
  end subroutine cpl_comp_ocngem_wrapper

  !!
  subroutine cpl_comp_gemocn_wrapper
    implicit none
    call cpl_comp_gemocn(                &
         & intrac_ocn_max,               &                              ! input
         & ilon1_ocn,ilat1_ocn,inl1_ocn, &                              ! input
         & genie_ocn                     &                              ! input/output
         )
  end subroutine cpl_comp_gemocn_wrapper

  ! genie-biogem/cpl_comp_biogem.f90
  ! update atm interface tracer array
  subroutine cpl_comp_gematm1_wrapper
    implicit none
    call cpl_comp_gematm1(      &
         & intrac_atm_max,      &                                       ! input
         & ilon1_ocn,ilat1_ocn, &                                       ! input
         & genie_atm1,          &                                       ! input
         & genie_sfcatm1        &                                       ! input/output
         )
  end subroutine cpl_comp_gematm1_wrapper

  !!
  subroutine biogem_save_restart_wrapper
    implicit none
    call biogem_save_restart(                                &
         & genie_clock                                       & ! input
         & )
  end subroutine biogem_save_restart_wrapper

! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! *** ECOGEM ********************************************************************************************************************* !
! ******************************************************************************************************************************** ! 

 !!
  subroutine ecogem_wrapper
    implicit none
    
    ! get biogem timeslice data
!    call diag_biogem_timesync( &
!        & genie_tseries,      & ! output
!        & genie_tslice,       & ! output
!        & genie_intseries,    & ! output
!        & genie_intslice,     & ! output
!        & genie_endseries,    & ! output
!        & genie_endslice      & ! output
!        )
        
    call ecogem(            &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,     & ! input
         & go_fxsw,         & ! input
         & go_mldta,        & ! input
         & egbg_sfcocn,     & ! input  -- tracer concentrations
         & egbg_sfcpart,    & ! output -- change in particulate concentration field
         & egbg_sfcremin    & ! output -- change in remin concentration field
         )
  end subroutine ecogem_wrapper

  !!
  subroutine diag_ecogem_timeslice_wrapper
    implicit none
    
    ! get biogem timeslice data
    call diag_biogem_timesync( &
        & genie_tseries,      & ! output
        & genie_tslice,       & ! output
        & genie_intseries,    & ! output
        & genie_intslice,     & ! output
        & genie_endseries,    & ! output
        & genie_endslice      & ! output
        )
    
    ! ecogem timeslice calculations and saving
    call diag_ecogem_timeslice( &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,     & ! input
         & genie_tseries,   & ! input
         & genie_tslice,    & ! input
         & genie_intseries, & ! input
         & genie_intslice,  & ! input
         & genie_endseries, & ! input
         & genie_endslice,  & ! input
         & go_fxsw,         & ! input
         & go_mldta,        & ! input
         & egbg_sfcocn,     & ! input  -- tracer concentrations
         & egbg_sfcpart,    & ! output -- change in particulate concentration field
         & egbg_sfcremin    & ! output -- change in remin concentration field
         )
  end subroutine diag_ecogem_timeslice_wrapper
  
  !!
  subroutine ecogem_save_restart_wrapper
    implicit none
    call ecogem_save_rst(      &
         & genie_clock         & ! input
         & )
  end subroutine ecogem_save_restart_wrapper

! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! *** ATCHEM ********************************************************************************************************************* !
! ******************************************************************************************************************************** !

  !!
  subroutine atchem_wrapper
    implicit none
    call atchem(                                             &
         & real(conv_kocn_katchem*kocn_loop)*genie_timestep, &          ! input
         & genie_sfxsumatm,                                  &          ! input/output
         & genie_sfcatm                                      &          ! input/output
         & )
  end subroutine atchem_wrapper

  !!
  subroutine cpl_comp_atmgem_wrapper
    implicit none
    call cpl_comp_atmgem(                                    &
         & real(conv_kocn_katchem*kocn_loop)*genie_timestep, &          ! input
         & intrac_atm_max,                                   &          ! input
         & ilon1_ocn,ilat1_ocn,                              &          ! input
         & genie_atm1                                        &          ! input/output
         )
  end subroutine cpl_comp_atmgem_wrapper

  !!
  subroutine cpl_comp_gematm_wrapper
    implicit none
    call cpl_comp_gematm(       &
         & intrac_atm_max,      &                                       ! input
         & ilon1_ocn,ilat1_ocn, &                                       ! input
         & genie_atm1           &                                       ! input/output
         )
  end subroutine cpl_comp_gematm_wrapper

  !!
  subroutine cpl_comp_atmocn_wrapper
    implicit none
    call cpl_comp_atmocn(       &
         & intrac_atm_max,      &                                       ! input
         & ilon1_atm,ilat1_atm, &                                       ! input
         & ilon1_ocn,ilat1_ocn, &                                       ! input
         & genie_sfcatm,        &                                       ! input
         & genie_sfcatm1        &                                       ! input/output
         & )
  end subroutine cpl_comp_atmocn_wrapper

  !!
  subroutine cpl_comp_EMBMatm_wrapper
    implicit none
    call cpl_comp_EMBMatm(         &
         & intrac_atm_max,      &                                       ! input
         & ilon1_atm,ilat1_atm, &                                       ! input
         & tstar_atm,           &                                       ! input (surface temperature)
         & surf_qstar_atm,      &                                       ! input (surface specific humidity)
         & genie_sfcatm        &                                        ! input/output
         & )
  end subroutine cpl_comp_EMBMatm_wrapper

  !!
  subroutine cpl_comp_EMBM_wrapper
    implicit none
    call cpl_comp_EMBM(         &
         & intrac_atm_max,      &                                       ! input
         & ilon1_atm,ilat1_atm, &                                       ! input
         & ilon1_ocn,ilat1_ocn, &                                       ! input
         & tstar_atm,           &                                       ! input (surface temperature)
         & surf_qstar_atm,      &                                       ! input (surface specific humidity)
         & genie_sfcatm1        &                                       ! input/output
         & )
  end subroutine cpl_comp_EMBM_wrapper

  !!
  subroutine cpl_comp_atmlnd_wrapper
    implicit none
    call cpl_comp_atmlnd(       &
         & intrac_atm_max,      &                                       ! input
         & ilon1_atm,ilat1_atm, &                                       ! input
         & ilon1_lnd,ilat1_lnd, &                                       ! input
         & genie_sfcatm,        &                                       ! input
         & genie_sfcatm_lnd     &                                       ! input/output
         & )
  end subroutine cpl_comp_atmlnd_wrapper

  !!
  subroutine atchem_save_restart_wrapper
    implicit none
    call atchem_save_rst(      &
         & genie_clock         & ! input
         & )
  end subroutine atchem_save_restart_wrapper

  !!
  subroutine atchem_climate_wrapper
    implicit none
    call atchem_climate(        &
         & tstar_atm,           &                                       ! input (surface temperature)
         & surf_qstar_atm       &                                       ! input (surface specific humidity)
         )
  end subroutine atchem_climate_wrapper

! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! *** SEDGEM ********************************************************************************************************************* !
! ******************************************************************************************************************************** !

  !!
  subroutine sedgem_wrapper
    implicit none
    call sedgem(                                             &
         & real(conv_kocn_ksedgem*kocn_loop)*genie_timestep, &          ! input
         & genie_sfxsumsed,                                  &          ! input/output
         & genie_sfcsumocn,                                  &          ! input
         & genie_sfcsed,                                     &          ! output
         & genie_sfxocn,                                     &          ! output
         & .true.                                            &          ! input
         & )
  end subroutine sedgem_wrapper

  !!
  subroutine sedgem_glt_wrapper
    implicit none
    call sedgem(                                             &
         & real(conv_kocn_ksedgem*kocn_loop)*genie_timestep, &          ! input
         & genie_sfxsumsed,                                  &          ! input/output
         & genie_sfcsumocn,                                  &          ! input
         & genie_sfcsed,                                     &          ! output
         & genie_sfxocn,                                     &          ! output
         & .false.                                           &          ! input
         & )
  end subroutine sedgem_glt_wrapper

  !!
  subroutine sedgem_dsedage_wrapper
    implicit none
    call sedgem_dsedage(                                     &
         & real(conv_kocn_ksedgem*kocn_loop)*genie_timestep, &          ! input
         & genie_sfxsumsed                                   &          ! input/output
         & )
  end subroutine sedgem_dsedage_wrapper

  !!
  subroutine sedgem_save_restart_wrapper
    implicit none
    call sedgem_save_rst(                                    &
         & genie_clock,                                      & ! input
         & genie_sfxocn                                      & ! input
         & )
  end subroutine sedgem_save_restart_wrapper

! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! *** GEMLITE ******************************************************************************************************************** !
! ******************************************************************************************************************************** !

  !!
  subroutine gemlite_wrapper
    implicit none
    call gemlite(            &
         & genie_sfcocn1,    &                                          ! output
         & genie_sfxsumsed1, &                                          ! input
         & genie_sfxocn1,    &                                          ! input
         & genie_sfxsumrok1_gem, &                                      ! input
         & genie_sfxsumatm1_gem  &                                      ! input
         )
  end subroutine gemlite_wrapper

  !!
  subroutine gemlite_climate_wrapper
    implicit none
    call gemlite_climate( &
         & frac_sic       &                                             ! input
         )
  end subroutine gemlite_climate_wrapper

  !!
  subroutine gemlite_cycleinit_wrapper
    implicit none
    call gemlite_cycleinit( &
         )
  end subroutine gemlite_cycleinit_wrapper

  !!
  subroutine gemlite_cycleclean_wrapper
    implicit none
    call gemlite_cycleclean( &
         & genie_sfxsumrok1_gem                  &                      ! 
         )
  end subroutine gemlite_cycleclean_wrapper

  !!
  subroutine gemlite_gltts_wrapper
    implicit none
    call gemlite_ts( &
         & go_ts,    &                                                  ! input/output
         & go_ts1    &                                                  ! input/output
         )
  end subroutine gemlite_gltts_wrapper

  !!
  subroutine cpl_comp_gemglt_wrapper
    implicit none
    call cpl_comp_gemglt(                 &
         & intrac_atm_max,intrac_ocn_max, &                             ! input
         & ilon1_ocn,ilat1_ocn,inl1_ocn,  &                             ! input
         & genie_atm1,                    &                             ! input/output
         & genie_ocn                      &                             ! input/output
         )
  end subroutine cpl_comp_gemglt_wrapper

  !!
  subroutine cpl_comp_gltgem_d_wrapper
    implicit none
    call cpl_comp_gltgem_d(               &
         & intrac_atm_max,intrac_ocn_max, & ! input
         & ilon1_ocn,ilat1_ocn,inl1_ocn,  & ! input
         & genie_atm1,                    & ! output
         & genie_ocn                      & ! output
         )
  end subroutine cpl_comp_gltgem_d_wrapper

  !!
  subroutine cpl_comp_gltgem_dsum_wrapper
    implicit none
    call cpl_comp_gltgem_dsum(            &
         & intrac_atm_max,intrac_ocn_max, & ! input
         & ilon1_ocn,ilat1_ocn,inl1_ocn,  & ! input
         & genie_atm1,                    & ! output
         & genie_ocn                      & ! output
         )
  end subroutine cpl_comp_gltgem_dsum_wrapper

! ******************************************************************************************************************************** !


  !!
  subroutine goldlite_wrapper
    implicit none
    call goldlite(                                                            &
         & go_ts(intrac_ocn-n_intrac_col:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn), & ! input/output
         & go_ts1(intrac_ocn-n_intrac_col:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn) & ! input/output
         )
  end subroutine goldlite_wrapper

  !!
  subroutine ocnlite_wrapper
    implicit none
    call ocnlite(            &
         )
  end subroutine ocnlite_wrapper


END MODULE genie_loop_wrappers

! ******************************************************************************************************************************** !
! END SEDGEM
SUBROUTINE end_sedgem( &
     & dum_dts,        &
     & dum_sfcsumocn   &
     & )
  use genie_control
  USE gem_cmn
  USE sedgem_lib
  USE sedgem_data
  USE sedgem_data_netCDF
  USE genie_util, ONLY: check_iostat
  IMPLICIT NONE
  ! ---------------------------------------------------------- !
  ! DEFINE DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  REAL,INTENT(in)::dum_dts
  real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfcsumocn
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  real::loc_dtyr ! local time step in years
  real::loc_dts  ! local time step in seconds
  real::loc_yr
  integer::loc_iou
  ! ---------------------------------------------------------- !
  ! INITIALIZE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  loc_yr = 0.0
  ! ---------------------------------------------------------- !
  ! SHUTDOWN ...
  ! ---------------------------------------------------------- !
  print*,'======================================================='
  print*,' >>> Initialising SEDGEM module shutdown ...'
  ! ------------------------------------------------------- !
  ! SAVE DATA: 2-D netCDF + ASCII output
  ! ------------------------------------------------------- !
  ! calculate sediment model time step length
  loc_dts  = dum_dts
  loc_dtyr = loc_dts/conv_yr_s
  ! save diagnostics
  call sub_data_save_seddiag_GLOBAL(loc_dtyr,dum_sfcsumocn)
  ! save requested sediment cores as ASCII
  if (ctrl_data_save_ascii) call sub_sedgem_save_sedcore()
  ! save final oecan-sediment interface properties
  ! NOTE: netCDF was set up to be able to save multiple time-slices, but is only being used here to save a single final slice
  if (ctrl_data_save_ascii) call sub_data_save_seddiag_2D(loc_dtyr,dum_sfcsumocn)
  call sub_save_netcdf(const_real_zero)
  call sub_save_netcdf_sed2d(loc_dtyr,dum_sfcsumocn)
  call sub_closefile(ntrec_siou)
  ntrec_sout = ntrec_sout + 1
  ! ------------------------------------------------------- !
  ! SAVE DATA: SEDCORES
  ! ------------------------------------------------------- !
  if (nv_sedcore > 0) then
     string_ncsedcorein = TRIM(par_rstdir_name)//trim(par_ncsedcore_name)
     string_ncsedcoreout = TRIM(par_outdir_name)//trim(par_ncsedcore_name)
     ncsedcore_ntrec = 0
     call sub_data_netCDF_sedcoresave(trim(string_ncsedcorein),trim(string_ncsedcoreout),loc_yr,loc_iou)
  end if
  ! ---------------------------------------------------------- !
  ! CLEAN UP
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! deallocate arrays
  DEALLOCATE(phys_sed,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_mask,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_mask_reef,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_mask_muds,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_top,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_top_h,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_fsed,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_fdis,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sedocn_fnet,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_carb,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_carbconst,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_carbalk,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_carbisor,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_save_mask,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_fsed_OLD,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_fdis_OLD,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  deALLOCATE(vsedcore_store,STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_Fsed_det,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_Fsed_caco3,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_Fsed_opal,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  DEALLOCATE(sed_diag,STAT=dealloc_error)
  call check_iostat(dealloc_error,__LINE__,__FILE__)
  ! ---------------------------------------------------------- ! deallocate arrays -- lookup tables
  if (par_sed_diagen_CaCO3opt == 'ridgwell2001lookup' .OR. par_sed_diagen_CaCO3opt == 'ridgwell2001lookupvec') then
     DEALLOCATE(lookup_sed_dis_cal,STAT=dealloc_error)
     call check_iostat(dealloc_error,__LINE__,__FILE__)
  end if
  if (par_sed_diagen_CaCO3opt == 'ridgwell2001lookupvec') then
     DEALLOCATE(lookup_vec_D,STAT=dealloc_error)
     call check_iostat(dealloc_error,__LINE__,__FILE__)
     DEALLOCATE(lookup_vec_dco3,STAT=dealloc_error)
     call check_iostat(dealloc_error,__LINE__,__FILE__)
     DEALLOCATE(lookup_vec_frac,STAT=dealloc_error)
     call check_iostat(dealloc_error,__LINE__,__FILE__)
     DEALLOCATE(lookup_vec_fCorg,STAT=dealloc_error)
     call check_iostat(dealloc_error,__LINE__,__FILE__)
  end if
  if (par_sed_diagen_opalopt == 'ridgwelletal2003lookup') then
     DEALLOCATE(lookup_sed_dis_opal,STAT=dealloc_error)
     call check_iostat(dealloc_error,__LINE__,__FILE__)
  end if
  ! ---------------------------------------------------------- !
  print*,' <<< Shutdown complete'
  print*,'======================================================='
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
END SUBROUTINE end_sedgem
! ******************************************************************************************************************************** !

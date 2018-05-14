
! ******************************************************************************************************************************** !
! END GEM
SUBROUTINE end_gem()
  USE gem_cmn
  
  print*,'======================================================='
  print*,' >>> Initialising GEM module shutdown ...'

  ! ---------------------------------------------------------- !
  !  DEALLOCATE ARRAYS
  !- --------------------------------------------------------- !
  !- --------------------------------------------------------- ! 
  DEALLOCATE(conv_ls_lo,STAT=alloc_error)
  DEALLOCATE(conv_lD_lP,STAT=alloc_error)
  DEALLOCATE(conv_lP_lD,STAT=alloc_error)
  DEALLOCATE(conv_lRD_lP,STAT=alloc_error)
  DEALLOCATE(conv_lP_lRD,STAT=alloc_error)
  DEALLOCATE(conv_ls_lo_N,STAT=alloc_error)
  DEALLOCATE(conv_ls_lo_S,STAT=alloc_error)
  DEALLOCATE(conv_ls_lo_meth,STAT=alloc_error)
  DEALLOCATE(conv_ls_lo_i,STAT=alloc_error)
  DEALLOCATE(conv_lD_lP_i,STAT=alloc_error)
  DEALLOCATE(conv_lP_lD_i,STAT=alloc_error)
  DEALLOCATE(conv_lRD_lP_i,STAT=alloc_error)
  DEALLOCATE(conv_lP_lRD_i,STAT=alloc_error)
  DEALLOCATE(conv_ls_lo_i_N,STAT=alloc_error)
  DEALLOCATE(conv_ls_lo_i_S,STAT=alloc_error)
  DEALLOCATE(conv_ls_lo_i_meth,STAT=alloc_error)
  !- --------------------------------------------------------- !

  print*,' <<< Shutdown complete'
  print*,'======================================================='

END SUBROUTINE end_gem
! ******************************************************************************************************************************** !

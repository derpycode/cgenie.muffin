
! File: end_rokgem.f90
!
! Description: shuts down RokGeM.
!
! Subroutine: end_rokgem
!
! shuts down RokGeM.
!
! Calls:
!
! - <rest_rokgem>
! - <deallocate_arrays>

SUBROUTINE end_rokgem()

  USE rokgem_lib, ONLY: deallocate_arrays

  print*,'======================================================='
  print*,' >>> Initialising rokgem module shutdown ...'

  call rest_rokgem()
  call deallocate_arrays()

  print*,' <<< Shutdown complete'
  print*,'======================================================='

END SUBROUTINE end_rokgem
! ******************************************************************************************************************************** !


! File: end_reggem.f90
!
! Description: shuts down reggem.
!
! Subroutine: end_reggem
!
! shuts down reggem.
!
! Calls:
!
! - <rest_reggem>
! - <deallocate_arrays>

SUBROUTINE end_reggem()

  USE reggem_lib, ONLY: deallocate_arrays

  print*,'======================================================='
  print*,' >>> Initialising reggem module shutdown ...'

  call rest_reggem()
  call deallocate_arrays()

  print*,' <<< Shutdown complete'
  print*,'======================================================='

END SUBROUTINE end_reggem
! ******************************************************************************************************************************** !

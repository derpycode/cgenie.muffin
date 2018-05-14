
! ******************************************************************************************************************************** !
! SETUP GEMlite
SUBROUTINE initialise_ocnlite( &
     & )
  USE ocnlite_lib
  USE ocnlite_data
  USE genie_util, ONLY: check_iostat
  ! dummy arguments
!!!
  ! local variables
!!!

  print*,'======================================================='
  print*,' >>> Initialising OCNlite ocean biogeochem. module ...'

  ! *** load goin information ***
  call sub_load_goin_ocnlite()

  ! *** dimension tracer arrays ***
!!!

  ! *** INITIALIZE OCNlite ***
!!!

  print*,' <<< Initialisation complete'
  print*,'======================================================='

  return

END SUBROUTINE initialise_ocnlite
! ******************************************************************************************************************************** !


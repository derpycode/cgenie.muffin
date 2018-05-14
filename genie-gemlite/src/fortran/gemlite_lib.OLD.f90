! *************************************************************************************************
! gemlite_lib.f90
! GEochemistry Model lite 1.1.0 - 06/05/03
! LIBRARY MODULE
! *************************************************************************************************


MODULE gemlite_lib


  USE gem_lib
  IMPLICIT NONE
  SAVE



  ! *********************************************************
  ! *** GLOBAL VARIABLE AND RUN-TIME SET PARAMETER ARRAYS ***
  ! *********************************************************

  !
  integer::par_gemlite_litestp_min
  integer::par_gemlite_litestp_max
  integer::par_gemlite_litestp
  INTEGER::par_gemlite_stp
  INTEGER::par_gemlite_sigstp
  INTEGER::par_gemlite_diagstp
  !
  real::gemlite_t,gemlite_dt                    ! accumulated GeMlite time (years)
  real::par_gemlite_dtyr_acc                    ! Time interval of accelerated geochemistry (yr)
  real::par_gemlite_dtyr_nonacc                 ! Time interval of non-accelerated 'normal' mode operation (yr)
  real::par_gemlite_tyr_acc_start
  !
  LOGICAL,DIMENSION(n_opt_gemlite)::opt_gemlite






  !        *******
  !    ***************
  !  ********************
  ! **********************
  ! *** CODE GRAVEYARD ***
  ! **********************
  ! **********************
  ! **      **  **      **
  ! **  **  **  **  **  **
  ! **  **  **  **  **  **
  ! **  *  ***  **      **
  ! **  **  **  **  ******
  ! **  **  **  **  ******
  ! **  **  **  **  ******
  ! **********************
  ! **********************
  ! **********************
  ! **********************



  
  
END MODULE gemlite_lib








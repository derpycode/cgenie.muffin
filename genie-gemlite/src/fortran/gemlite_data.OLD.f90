! *************************************************************************************************
! gemlite_data.f90
! GEochemistry lite
! DATA LOADING/SAVING ROUTINES
! *************************************************************************************************


MODULE gemlite_data

  
  USE gemlite_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ********************************************
  ! *** DATA LOADING/INITIALIZATION ROUTINES ***
  ! ********************************************


  ! *** load GeM run-time options ***
  SUBROUTINE sub_load_gemlite_config()
    ! local variables
    INTEGER::n
    INTEGER::loc_n_elements,loc_n_start
    CHARACTER(len=255)::loc_filename
    ! check file format
    loc_filename = TRIM(string_gemlite_dir)//'gemlite_config.par'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=loc_filename,action="read")
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! INTEGRATION CONTROL
    READ(unit=in,fmt='(1X)')
    READ(unit=in,fmt='(1X)')
    READ(unit=in,fmt='(E10.3)') par_gemlite_dtyr_nonacc
    READ(unit=in,fmt='(E10.3)') par_gemlite_dtyr_acc
    READ(unit=in,fmt='(I6)') par_gemlite_stp
    READ(unit=in,fmt='(I6)') par_gemlite_sigstp
    ! I/O
    READ(unit=in,fmt='(1X)')
    READ(unit=in,fmt='(1X)')
    READ(unit=in,fmt='(I6)') par_gemlite_diagstp
    READ(unit=in,fmt='(L4)') opt_gemlite(iopt_gemlite_debug)
     ! close file pipe
    CLOSE(unit=in)
  END SUBROUTINE sub_load_gemlite_config



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




END MODULE gemlite_data







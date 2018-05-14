! ******************************************************************************************************************************** !
! ocnlite_data.f90
! 
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE ocnlite_data

  
  USE ocnlite_lib
  IMPLICIT NONE
  SAVE
  
  
CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! LOAD OCNlite 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_ocnlite()
    USE genie_util, ONLY: check_unit,check_iostat
!    ! local variables
!    integer::ios                                                        !
!    ! read data_OCNLTIE file
!    call check_unit(in,__LINE__,__FILE__)
!    open(unit=in,file='data_OCNLITE',status='old',action='read',iostat=ios)
!    if (ios /= 0) then
!       print*,'ERROR: could not open OCNLTIE initialisation namelist file'
!       stop
!    end if
!    ! read in namelist and close data_OCNLTIE file
!    read(UNIT=in,NML=ini_ocnlite_nml,IOSTAT=ios)
!    if (ios /= 0) then
!       print*,'ERROR: could not read OCNLTIE namelist'
!       stop
!    else
!       close(unit=in)
!    end if
if (ctrl_debug_init > 0) then
    ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
    ! ############################################################################################################################ !
end if
  END SUBROUTINE sub_load_goin_ocnlite
  ! ****************************************************************************************************************************** !

END MODULE ocnlite_data


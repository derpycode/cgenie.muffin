! ******************************************************************************************************************************** !
! goldlite_data.f90
! 
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE goldlite_data

  
  USE goldlite_lib
  IMPLICIT NONE
  SAVE
  
  
CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! LOAD OCNlite 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_goldlite()
    USE genie_util, ONLY: check_unit,check_iostat
    ! local variables
    integer::ios                                                        !
    ! read data_GOLDLTIE file
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='data_GOLDLITE',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open GOLDLTIE initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_GOLDLTIE file
    read(UNIT=in,NML=ini_goldlite_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read GOLDLTIE namelist'
       stop
    else
       close(unit=in)
    end if
if (ctrl_debug_init > 0) then
    ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
    print*,'Color pattern option                                : ',opt_colorpattern
    ! ############################################################################################################################ !
end if
  END SUBROUTINE sub_load_goin_goldlite
  ! ****************************************************************************************************************************** !

END MODULE goldlite_data


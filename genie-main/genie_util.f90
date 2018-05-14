!! Module for GENIE-wide utility routines, such as file checks, 
!! NetCDF I/O routines? etc.

module genie_util
  
  implicit none

  ! ========================================================================
  ! defensive programming:
  ! entities are private by default to avoid unwanted access or modification.
  ! public entities are explicitly listed.
  ! ========================================================================
  private
  public :: check_unit, check_iostat, message, die

contains

  ! ========================================================================
  ! print a message and abort the program
  ! optional references to the line number and file of the error
  ! ========================================================================
  subroutine die(msg, line, file)

    implicit none

    ! args
    character(len=*), intent(in)           :: msg
    integer,          intent(in), optional :: line
    character(len=*), intent(in), optional :: file
    !
    write(6,*) "ERROR: ", trim(msg)
    if(present(line)) then
       write(6,*) 'at line: ', line
    end if
    if(present(file)) then
       write(6,*) 'in file: ', trim(file)
    end if
    write (6,*) 'stopping'
    call flush(6)
    stop

  end subroutine die

  ! ========================================================================
  !     Subroutine to test whether a file unit is already in use      
  ! ========================================================================
  subroutine check_unit(unitNum, line, file)
    
    use genie_control, only : BUFSIZ

    implicit none
    ! args 
    integer,          intent(in)           :: unitNum
    integer,          intent(in), optional :: line
    character(len=*), intent(in), optional :: file
    ! locals
    logical :: lUnitInUse
    character(len=BUFSIZ)                  :: errStr      ! for error messages
    ! body
    inquire(UNIT=unitNum,OPENED=lUnitInUse)
    if(lUnitInUse) then
       write(errStr,*) "unit", unitNum, "already in use"
       call die(errStr,line,file)
    end if
    
  end subroutine check_unit
  
  ! ========================================================================
  !     Subroutine to test the IOSTAT value for an I/O action
  ! ========================================================================
  subroutine check_iostat(ios, line, file)
    
    use genie_control, only : BUFSIZ

    implicit none
    
    ! args
    integer,          intent(in)           :: ios
    integer,          intent(in), optional :: line
    character(len=*), intent(in), optional :: file
    ! body
    if(ios/=0) then
       call die("I/O ERROR", line, file)
    end if
    
  end subroutine check_iostat

  ! ========================================================================
  ! write the message to screen if verbosity is sufficient
  ! ========================================================================
  subroutine message(msg,level,filename)

    use genie_global, only : verbosity  ! only want to access this one variable
    use genie_global, only : BUFSIZ

    implicit none

    ! args
    character(len=*),intent(in)          :: msg         ! the message to print to screen 
    integer         ,intent(in)          :: level       ! only print if level <= global verbosity
    character(len=*),intent(in),optional :: filename    ! optional log filename 

    ! locals
    integer, parameter                   :: unitNum = 8 ! unit number to access file
    integer                              :: ios         ! error checking
    character(len=BUFSIZ)                :: errStr      ! for error messages

    if(level.le.verbosity) then
       if(present(filename)) then
          ! check for a free unit number
          call check_unit(unitNum,__LINE__,__FILE__)
          ! open the specified filename for writing
          open(unit=unitNum,file=filename,status='UNKNOWN',iostat=ios)
          if (ios /= 0) then
             write(errStr,*) 'could not open log file writing:', trim(filename)
             call die(errStr,__LINE__,__FILE__)
          end if
          ! write the message to file
          write(unitNum,*) msg
          ! and close the file
          close(unitNum,iostat=ios)
          if (ios /= 0) then
             write(errStr,*) 'could not close log file:', trim(filename)
             call die(errStr,__LINE__,__FILE__)
          end if
       end if
       ! otherwise write the message to screen
       write(6,*) msg
    end if

  end subroutine message


end module genie_util

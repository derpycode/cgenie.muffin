!========================================
! Marc 1/4/03
! This file just closes a netcdf file and is the partner to open_file.f
!========================================
      subroutine close_file_nc(filein, ncid)
      implicit none
!----------------------------------------
! Define variable in the subroutine header
!----------------------------------------
      character(*), intent(in) :: filein    ! Name of file to open
      integer, intent(in)      :: ncid      ! netCDF dataset ID
!----------------------------------------
! Define the other netcdf variable
!----------------------------------------
      integer :: status                      ! return code
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'
!----------------------------------------
! close file
!----------------------------------------
      status=nf_close(ncid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: closing ', filein
         stop
      endif
      end

!========================================
! Marc 1/4/03
! This file just opens a netcdf file
!========================================
      subroutine open_file_nc(filein, ncid)
      implicit none
!----------------------------------------
! Define variables in the subroutine header
!----------------------------------------
      character(*), intent(in) :: filein    ! Name of file to open
      integer, intent(out)     :: ncid      ! netCDF dataset ID
!----------------------------------------
! Define the other netcdf variables
!----------------------------------------
      integer :: status                      ! return code
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'
!----------------------------------------
! open the netcdf file
!----------------------------------------
      status=nf_open(filein, nf_nowrite, ncid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: opening ', filein
         stop
      endif
      end

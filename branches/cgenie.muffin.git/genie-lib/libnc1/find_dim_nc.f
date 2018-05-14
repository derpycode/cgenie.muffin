!========================================
! Marc 21/5/03
! Given the netcdf ID, ncid, and the name of a dimension this 
! subroutine will find the size of the dimension. 
! File must first be opened using subroutine open_file
!========================================
      subroutine find_dim_nc(ncid, dimname, dim)
      implicit none
!----------------------------------------
! Define variables in the subroutine header
!----------------------------------------
      integer, intent(in)      :: ncid      ! netCDF dataset ID
      character(*), intent(in) :: dimname   ! name of dimension
      integer, intent(out)     :: dim       ! Size of dimension 
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status                     ! return code
     &     , dimid                          ! ID of dimension
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'
!----------------------------------------
! find the dimension ID
!----------------------------------------
      status=nf_inq_dimid(ncid, dimname, dimid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find ID of', dimname
         stop
      endif
!----------------------------------------
! find size of dimension
!----------------------------------------
      status=nf_inq_dimlen(ncid, dimid, dim)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find size of ', dimname
         stop
      endif
      end

!========================================
! Marc 22/5/03
! Given the netcdf ID, ncid, this file will read one point from
! a 1d array.
! File must first be opened using subroutine open_file
!========================================
      subroutine get1d_data_point_nc(ncid, varname, ipoint, 
     :                               varout,ifail)
      implicit none

!----------------------------------------
! For precision
!----------------------------------------
      integer :: realkind
!----------------------------------------
! Define variables in the subroutine header
!----------------------------------------
      integer, intent(in)      :: ncid      ! netCDF dataset ID
      character(*), intent(in) :: varname   ! name of variable to collect
      integer, intent(in)      :: ipoint    ! position of element in 1d array  
      real, intent(out)        :: varout    ! vlaue taken from 1d array
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status                     ! return code
     &     , varid                          ! ID of variable
     &     , dimid                          ! IDs for dimensions
!----------------------------------------
! local variables
!----------------------------------------
      integer :: ndims                      ! number of dims. 
     &     , dim1nc                         ! size of 1st dim. in netcdf file
     &     , ifail
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'

      realkind=kind(varout)

!----------------------------------------
! Find the variable ID
!----------------------------------------
      ifail=0
      status=nf_inq_varid(ncid, varname, varid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find ', varname
         ifail=1
         return
      endif
!----------------------------------------
! find out information on variable
!----------------------------------------
      status=nf_inq_varndims(ncid, varid, ndims)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find dims. of variable'
         stop
      endif
      if (ndims.ne.1) then
         write(6, *) 'ERROR: variable has ', ndims, 
     &        ' dimensions and we expect 1'
      endif
      status=nf_inq_vardimid(ncid, varid, dimid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find dimension IDs'
         stop
      endif
!----------------------------------------
! Check size of the dimensions are OK
!----------------------------------------
      status=nf_inq_dimlen(ncid, dimid, dim1nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: Could not get 1st dimension from '
     &        //'netcdf file'
         stop
      endif
      if (ipoint.gt.dim1nc) then 
         write(6, *) 'ERROR: netcdf variable is not big enough'
         stop
      endif
!----------------------------------------
! read in value
!----------------------------------------
      if (realkind.eq.4) then
        status=nf_get_var1_real(ncid, varid, ipoint, varout)
      else if (realkind.eq.8) then
        status=nf_get_var1_double(ncid, varid, ipoint, varout)
      else
        print*,'precision problem in get1d_data_point_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting variable', varname
         stop
      endif

      end

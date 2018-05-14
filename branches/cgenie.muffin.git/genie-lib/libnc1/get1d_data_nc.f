!========================================
! Marc 20/5/03
! Given the netcdf ID, ncid, this file will read variable from it
! File must first be opened using subroutine open_file
!========================================
      subroutine get1d_data_nc(ncid, varname, dim1
     &     , arrayout,ifail)
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
      integer, intent(in)      :: dim1      ! Size of array in 1st dimension
      real, intent(out) :: arrayout(dim1) ! the output array 
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status                     ! return code
     &     , varid                          ! ID of variable
!     &     , dim1id                         ! ID for 1st dimension
!----------------------------------------
! local variables
!----------------------------------------
      integer :: ndims                      ! dims. for netcdf variable
     &     , dimid                          ! dimension IDs
     &     , dim1nc                         ! 1st dim. of netcdf var. 
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'
      integer ifail 

      realkind=kind(arrayout)

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
! get information on variable
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
         write(6, *) 'ERROR: could not find dimension ID'
         stop
      endif
!----------------------------------------
! check that dimensions match
!----------------------------------------
      status=nf_inq_dimlen(ncid, dimid, dim1nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: Could not get 1st dimension from '
     &        //'netcdf file'
         stop
      endif
      if (dim1nc.ne.dim1) then
         write(6, *) 'ERROR: 1st dimension of variable in model '
     &        //'and netcdf file do not match' 
         write(6, *) 'model and netcdf dims are ', dim1, ' and '
     &        , dim1nc
         stop
      endif
!----------------------------------------
! get variable
!----------------------------------------
      if (realkind.eq.4) then
        status=nf_get_var_real(ncid, varid, arrayout) 
      else if (realkind.eq.8) then
        status=nf_get_var_double(ncid, varid, arrayout)
      else
        print*,'precision problem in get1d_data_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting variable'
         stop
      endif

      end
!
!     Integer version of above code. Any changes in above should
!     also be reflected here
!
      subroutine get1di_data_nc(ncid, varname, dim1
     &     , arrayout,ifail)
      implicit none
!----------------------------------------
! Define variables in the subroutine header
!----------------------------------------
      integer, intent(in)      :: ncid      ! netCDF dataset ID
      character(*), intent(in) :: varname   ! name of variable to collect
      integer, intent(in)      :: dim1      ! Size of array in 1st dimension
      integer, intent(out) :: arrayout(dim1) ! the output array 
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status                     ! return code
     &     , varid                          ! ID of variable
!     &     , dim1id                         ! ID for 1st dimension
!----------------------------------------
! local variables
!----------------------------------------
      integer :: ndims                      ! dims. for netcdf variable
     &     , dimid                          ! dimension IDs
     &     , dim1nc                         ! 1st dim. of netcdf var. 
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'
      integer ifail 
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
! get information on variable
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
         write(6, *) 'ERROR: could not find dimension ID'
         stop
      endif
!----------------------------------------
! check that dimensions match
!----------------------------------------
      status=nf_inq_dimlen(ncid, dimid, dim1nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: Could not get 1st dimension from '
     &        //'netcdf file'
         stop
      endif
      if (dim1nc.ne.dim1) then
         write(6, *) 'ERROR: 1st dimension of variable in model '
     &        //'and netcdf file do not match' 
         write(6, *) 'model and netcdf dims are ', dim1, ' and '
     &        , dim1nc
         stop
      endif
!----------------------------------------
! get variable
!----------------------------------------
      status=nf_get_var_int(ncid, varid, arrayout) 
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting variable'
         stop
      endif

      end

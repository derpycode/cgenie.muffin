!========================================
! Marc 15/5/03
! Given the netcdf ID, ncid, this file will read variable from it
! File must first be opened using subroutine open_file
!========================================
      subroutine get3d_data_nc(ncid, varname, dim1, dim2
     &     , dim3, arrayout, ifail)
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
     &     , dim2                           ! Size of array in 2nd dimension
     &     , dim3                           ! Size of array in 3rd dimension
      real, intent(out) :: arrayout(dim1,dim2,dim3) ! the output array 
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status,ifail               ! return code
     &     , varid                          ! ID of variable
!     &     , dim1id                         ! ID for 1st dimension
!     &     , dim2id                         ! ID for 2nd dimension
!     &     , dim3id                         ! ID for 3rd dimension
!----------------------------------------
! local variables
!----------------------------------------
      integer :: ndims                      ! dims. for netcdf variable
     &     , dimids(3)                      ! dimension IDs
     &     , dim1nc                         ! 1st dim. of netcdf var. 
     &     , dim2nc                         ! 2nd dim. of netcdf var.
     &     , dim3nc                         ! 3rd dim. of netcdf var.
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'

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
      if (ndims.ne.3) then
         write(6, *) 'ERROR: variable has ', ndims, 
     &        ' dimensions and we expect 3'
      endif
      status=nf_inq_vardimid(ncid, varid, dimids)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find dimension IDs'
         stop
      endif
!----------------------------------------
! check that dimensions match
!----------------------------------------
      status=nf_inq_dimlen(ncid, dimids(1), dim1nc)
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
      status=nf_inq_dimlen(ncid, dimids(2), dim2nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: Could not get 2nd dimension from '
     &        //'netcdf file'
         stop
      endif
      if (dim2nc.ne.dim2) then
         write(6, *) 'ERROR: 2nd dimension of variable in model '
     &        //'and netcdf file do not match' 
         write(6, *) 'model and netcdf dims are ', dim2, ' and '
     &        , dim2nc
         stop
      endif
      status=nf_inq_dimlen(ncid, dimids(3), dim3nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: Could not get 3rd dimension from '
     &        //'netcdf file'
         stop
      endif
      if (dim3nc.ne.dim3) then
         write(6, *) 'ERROR: 3rd dimension of variable in model '
     &        //'and netcdf file do not match' 
         write(6, *) 'model and netcdf dims are ', dim3, ' and '
     &        , dim3nc
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
        print*,'precision problem in get3d_data_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting variable'
         stop
      endif

      end

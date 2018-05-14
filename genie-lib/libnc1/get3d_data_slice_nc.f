!========================================
! Marc 20/5/03
! Given the netcdf ID, ncid, this file will read 3d variable from it.
! For a variable, say a(x,y,z) this will read and x-y slice at
! kslice in the z dimension, so that arrayout(:,:)=a(:,:,kslice)
! File must first be opened using subroutine open_file
!========================================
      subroutine get3d_data_slice_nc(ncid, varname, dim1, dim2
     &     , kslice, arrayout, ifail)
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
     &     , kslice                         ! place where slice is taken
      real, intent(out) :: arrayout(dim1,dim2) ! the array which is read in
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status                     ! return code
     &     , varid                          ! ID of variable
     &     , dimids(3)                      ! IDs for dimensions
     &     , start(3)                       ! 1st elem. of netcdf var. read in
     &     , count(3)                       ! wanted elements in each direction
!----------------------------------------
! local variables
!----------------------------------------
      integer :: ndims                      ! number of dims. 
     &     , dim1nc                         ! size of 1st dim. in netcdf file
     &     , dim2nc                         ! size of 2nd dim. in netcdf file
     &     , dim3nc                         ! size of 3rd dim. in netcdf file
     &     , ifail
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
         write(6, *) 'ERR: get3d_data_sl: could not find ', varname
         ifail=1
         return
      endif
!----------------------------------------
! find out information on variable
!----------------------------------------
      status=nf_inq_varndims(ncid, varid, ndims)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get3d_data_sl: could not find dims. of '
     :               //'variable'
         stop
      endif
      if (ndims.ne.3) then
         write(6, *) 'ERR: get3d_data_sl: variable has ', ndims, 
     &        ' dimensions and we expect 3'
      endif
      status=nf_inq_vardimid(ncid, varid, dimids)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get3d_data_sl: could not find dimension IDs'
         stop
      endif
!----------------------------------------
! Check size of the dimensions are OK
!----------------------------------------
      status=nf_inq_dimlen(ncid, dimids(1), dim1nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get3d_data_sl: Could not get 1st dimension '
     &        //'from netcdf file'
         stop
      endif
      if (dim1nc.ne.dim1) then 
         write(6, *) 'ERR: get3d_data_sl: 1st dimension of array and '
     &        //' netcdf data do not correspond'
         stop
      endif
      status=nf_inq_dimlen(ncid, dimids(2), dim2nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get3d_data_sl: Could not get 1st dimension '
     &        //'from netcdf file'
         stop
      endif
      if (dim2nc.ne.dim2) then 
         write(6, *) 'ERR: get3d_data_sl: 2nd dimension of array and '
     &        //' netcdf data do not correspond'
         stop
      endif
      status=nf_inq_dimlen(ncid, dimids(3), dim3nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get3d_data_sl: Could not get 3rd dimension '
     &        //'from netcdf file'
         stop
      endif
      if (kslice.gt.dim3nc) then 
         write(6, *) 'ERR: get3d_data_sl: netcdf variable is not '
     :               //'big enough'
         stop
      endif
!----------------------------------------
! read in slice
!----------------------------------------
      start(1)=1
      start(2)=1
      start(3)=kslice
      count(1)=dim1
      count(2)=dim2
      count(3)=1

      if (realkind.eq.4) then
      status=nf_get_vara_real(ncid, varid, start, 
     &        count, arrayout) 
      else if (realkind.eq.8) then
      status=nf_get_vara_double(ncid, varid, start, 
     &        count, arrayout) 
      else
        print*,'precision problem in get3d_data_slice_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get3d_data_sl: getting variable'
         stop
      endif

      end

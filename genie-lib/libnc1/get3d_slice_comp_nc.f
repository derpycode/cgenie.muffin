!========================================
! Marc 30/5/03
! Given the netcdf ID, ncid, this file will read the 
! real and imaginary parts of variable from it.
! File must first be opened using subroutine open_file
!========================================
      subroutine get3d_slice_comp_nc(ncid, varname, dim1, dim2
     &     , count2, kslice, arrayout, ifail)
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
     &     , count2                         ! values wanted from 2nd dimension
     &     , kslice                         ! latitude where slice is taken
      complex, intent(out) :: arrayout(dim1, dim2) ! the array which is read in
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status                     ! return code
     &     , var_reid                       ! ID for real part of var.
     &     , var_imid                       ! ID for imaginary part of var.
!     &     , dim1id                         ! ID for 1st dimension
!     &     , dim2id                         ! ID for 2nd dimension
!     &     , dim3id                         ! ID for 2nd dimension
     &     , start(3)                       ! 1st elem. of netcdf var. read in
     &     , count(3)                       ! wanted elements in each direction
!----------------------------------------
! local variables
!----------------------------------------
      complex, parameter :: ic=(0.0, 1.0)   ! -i in complex number notation
      character(300)     :: varname_re      ! name for real comp. of variable 
     &     , varname_im                     ! name for imag comp. of variable
      real :: array_re(dim1, count2)        ! real part of arrayout
     &     , array_im(dim1, count2)         ! imaginary part of arrayout
      integer :: ndims                      ! dims. for netcdf variable
     &     , dimids(3)                      ! dimension IDs
     &     , dim1nc                         ! 1st dim. of netcdf var. 
     &     , dim2nc                         ! 2nd dim. of netcdf var.
     &     , dim3nc                         ! 3rd dim. of netcdf var.
      integer :: length_of_varname_re  ! length of real (and imag) var. string
      integer, external :: length_of_char_nc ! funct. to calc. length of string
      integer ifail
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'

      realkind=kind(arrayout)

!----------------------------------------
! assume that real and imaginary part of varname are varname'_re' and 
! varname'_im'.
!----------------------------------------
      ifail=0
      varname_re=varname//'_re'
      varname_im=varname//'_im'
      length_of_varname_re=length_of_char_nc(varname_re)
!----------------------------------------
! find the variable ID
!----------------------------------------
      status=nf_inq_varid(ncid, 
     &     varname_re(1:length_of_varname_re), var_reid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find real part of ', varname
         ifail=1
         return
      endif
      status=nf_inq_varid(ncid, 
     &     varname_im(1:length_of_varname_re), var_imid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find imaginary part of ', 
     &        varname
         ifail=1
         return
      endif
!----------------------------------------
! get information on variable
!----------------------------------------
      status=nf_inq_varndims(ncid, var_reid, ndims)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find dims. of variable'
         stop
      endif
      if (ndims.ne.3) then
         write(6, *) 'ERROR: variable has ', ndims, 
     &        ' dimensions and we expect 2'
      endif
      status=nf_inq_vardimid(ncid, var_reid, dimids)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find dimension IDs'
         stop
      endif
!----------------------------------------
! check that dimensions match
! assume if real component is OK then imaginary component
! will also be fine
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
      if (count2.gt.dim2nc) then
         write(6, *) 'ERROR: values wanted in 2nd dimension, '
     &        , count2, ', exceed size of dimension, ', dim2nc
         stop
      endif
      status=nf_inq_dimlen(ncid, dimids(3), dim3nc)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: Could not get 2nd dimension from '
     &        //'netcdf file'
         stop
      endif
      if (kslice.gt.dim3nc) then
         write(6, *) 'ERROR: kslice is too large' 
         write(6, *) 'kslice and size of 3rd dimension are',
     &        kslice, ' and ', dim3nc
         stop
      endif
!----------------------------------------
! read in real and imaginary slice
!----------------------------------------
      start(1)=1
      start(2)=1
      start(3)=kslice
      count(1)=dim1
      count(2)=count2
      count(3)=1

      if (realkind.eq.4) then
        status=nf_get_vara_real(ncid, var_reid, start, 
     &           count, array_re) 
      else if (realkind.eq.8) then
        status=nf_get_vara_double(ncid, var_reid, start, 
     &           count, array_re)
      else
        print*,'precision problem in get3d_slice_comp_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting real part of ', varname
         stop
      endif

      if (realkind.eq.4) then
        status=nf_get_vara_real(ncid, var_imid, start, 
     &           count, array_im) 
      else if (realkind.eq.8) then
        status=nf_get_vara_double(ncid, var_imid, start, 
     &           count, array_im) 
      else
        print*,'precision problem in get3d_slice_comp_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting imaginary part of ', varname
         stop
      endif
!----------------------------------------
! put real and imaginary parts together
!----------------------------------------
      arrayout(1:dim1,1:count2)=array_re+ic*array_im
      end

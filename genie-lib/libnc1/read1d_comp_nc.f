!========================================
! NEED TO ALLOW FOR JUST ONE HEMISPHERE !!!! - marc 27/3/03
! Marc 25/3/03
! Reads a real and imagniary parts of 1D array from a netcdf file
!========================================
      subroutine read1d_comp_nc(filein, varname, arraysize, arrayout)
      implicit none

!----------------------------------------
! For precision
!----------------------------------------
      integer :: realkind
!----------------------------------------
! Define variables in the subroutine header
!----------------------------------------
      character(*), intent(in) :: filein    ! Name of file to read from
      character(*), intent(in) :: varname   ! name of variable to collect
      integer, intent(in)      :: arraysize ! Size of array
      complex, intent(out) :: arrayout(arraysize)  ! the array which is read in
!----------------------------------------
! Define the netcdf variables
!----------------------------------------
      integer :: ncid                       ! netCDF dataset ID
     &     , status                         ! return code
     &     , var_reid                       ! ID of variable (real component)
     &     , var_imid                       ! ID of variable (imag. component)
!----------------------------------------
! Local variables
!----------------------------------------
      complex, parameter :: ic=(0.0, 1.0)   ! -i in complex number notation
      character(300)     :: varname_re      ! name for real comp. of variable 
     &     , varname_im                     ! name for imag comp. of variable
      real :: array_re(arraysize)           ! real part of arrayout
     &     , array_im(arraysize)            ! imaginary part of arrayout
      integer :: length_of_varname_re  ! length of real (and imag) var. string
      integer, external :: length_of_char_nc ! funct. to calc. length of string
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'

      realkind=kind(arrayout)

!----------------------------------------
! open the netcdf file
!----------------------------------------
      status=nf_open(filein, nf_nowrite, ncid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: opening ', filein
         stop
      endif
!----------------------------------------
! assume that real and imaginary part of varname are varname'_re' and 
! varname'_im'.
!----------------------------------------
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
         stop
      endif
      status=nf_inq_varid(ncid, 
     &     varname_im(1:length_of_varname_re), var_imid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: could not find imaginary part of ', 
     &        varname
         stop
      endif
!----------------------------------------
! get variable
!----------------------------------------

      if (realkind.eq.4) then
        status=nf_get_var_real(ncid, var_reid, array_re) 
      else if (realkind.eq.8) then
        status=nf_get_var_double(ncid, var_reid, array_re) 
      else
        print*,'precision problem in read1d_comp_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting real part of variable'
         stop
      endif

      if (realkind.eq.4) then
        status=nf_get_var_real(ncid, var_imid, array_im) 
      else if (realkind.eq.8) then
        status=nf_get_var_double(ncid, var_imid, array_im) 
      else
        print*,'precision problem in read1d_comp_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: getting imaginary part of variable'
         stop
      endif
!----------------------------------------
! put real and imaginary parts together
!----------------------------------------
      arrayout=array_re+ic*array_im
!----------------------------------------
! close file
!----------------------------------------
      status=nf_close(ncid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: closing ', filein
         stop
      endif
      end

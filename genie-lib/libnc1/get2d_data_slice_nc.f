!========================================
! Marc 31/4/03
! Given the netcdf ID, ncid, this file will read variable from it
! File must first be opened using subroutine open_file
!========================================
      subroutine get2d_data_slice_nc(ncid, varname, mg, jslice
     &     , arrayout)
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
      integer, intent(in)      :: mg        ! Size of array in 1st dimension
     &     , jslice                         ! latitude where slice is taken
      real, intent(out) :: arrayout(mg)     ! the array which is read in
!----------------------------------------
! Define other netcdf variables
!----------------------------------------
      integer :: status                     ! return code
     &     , varid                          ! ID of variable
     &     , longid                         ! ID for longitude dimension
     &     , latid                          ! ID for latitude dimension
     &     , start(2)                       ! 1st elem. of netcdf var. read in
     &     , count(2)                       ! wanted elements in each direction
!----------------------------------------
! local variables
!----------------------------------------
      integer :: nlong                      ! number of longitudes
     &     , nlat                           ! number of latitudes
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'

      realkind=kind(arrayout)

!----------------------------------------
! Find the dimension ID
!----------------------------------------
      status=nf_inq_dimid(ncid, 'longitude', longid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get2d_data_sl: could not find longitude ID'
         write(6, *) 'while looking for variable ', varname 
         stop
      endif
      status=nf_inq_dimid(ncid, 'latitude', latid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get2d_data_sl: could not find latitude ID'
         stop
      endif
!----------------------------------------
! Find the size of the dimensions
!----------------------------------------
      status=nf_inq_dimlen(ncid, longid, nlong)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get2d_data_sl: Could not get longitude '
     :               //' dimension'
         stop
      endif
      status=nf_inq_dimlen(ncid, latid, nlat)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get2d_data_sl: Could not get latitude '
     :               //' dimension'
         stop
      endif
!----------------------------------------
! check dimensions are OK
!----------------------------------------
      if (mg.ne.nlong) then 
         write(6, *) 'ERR: get2d_data_sl: longitude dimension of '
     &        //' array and netcdf data do not correspond'
         stop
      endif
      if ( (jslice.le.0).or.(jslice.gt.nlat) ) then
          write(6, *) "ERR: get2d_data_sl: slice is ", jslice, 
     &        " and there are only ", nlat, " latitudes"
         stop
      endif
!----------------------------------------
! find the variable ID
!----------------------------------------
      status=nf_inq_varid(ncid, varname, varid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get2d_data_sl: could not find ', varname
         stop
      endif
!----------------------------------------
! read in slice
!----------------------------------------
      start(1)=1
      start(2)=jslice
      count(1)=mg
      count(2)=1

      if (realkind.eq.4) then
        status=nf_get_vara_real(ncid, varid, start, 
     &         count, arrayout) 
      else if (realkind.eq.8) then
        status=nf_get_vara_double(ncid, varid, start, 
     &         count, arrayout) 
      else
        print*,'precision problem in get2d_data_slice_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: get2d_data_sl: getting variable'
         stop
      endif

      end

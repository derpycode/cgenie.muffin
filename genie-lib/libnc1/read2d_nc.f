!========================================
! NEED TO ALLOW FOR JUST ONE HEMISPHERE !!!! - marc 27/3/03
! Marc 26/3/03
! Reads a 2D array from a netcdf file.
! The transformation of data to the array is complicated, because the IGCM 
! splits the data into hemispheres, i.e. North and South.
! Details:
! For data, arrayin, with nlong longitude and nlat latitudes the 
! output array, arrayout, must be (2*(nlong+2)) by (nlat/2), and 
! nlat must be even.
!   The Northern Hemisphere, arrayin(1:nlong,1:nlat/2) simply maps into
! arrayout(1:nlong,1:nlat/2).
!   The Southern Hemisphere, arrayin(1:nlong,nlat/2+1:nlat) maps into
! arrayout(nlong+3:2*(nlong+2),nlat/2:1). 
!   Hence the latitudes nearest the equator are stored in 
! arrayout(*,nlat/2) and the latitude nearest the Poles are stored in 
! arrayout(*,1) for both Nothern and Southern Hemispheres. 
! Note: arrayout(nlong+1:nlong+2,*) and 
! arrayout(2*(nlong+2)-1:2*(nlong+2),*) are kepy empty.
!========================================
      subroutine read2d_nc(filein, varname, xsize, ysize, arrayout)
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
      integer, intent(in)      :: xsize     ! Size of array in 1st dimension
     &     , ysize                          ! Size of array in 2nd dimension
      real, intent(out) :: arrayout(xsize,ysize)  ! the array which is read in
!----------------------------------------
! Define the netcdf variables
!----------------------------------------
      integer :: ncid                       ! netCDF dataset ID
     &     , status                         ! return code
     &     , varid                          ! ID of variable
     &     , longid                         ! ID for longitude dimension
     &     , latid                          ! ID for latitude dimension
!----------------------------------------
! local variables
!----------------------------------------
      integer :: nlong                      ! number of longitudes
     &     , nlat                           ! number of latitudes
      real :: arrayin((xsize/2-2), 2*ysize) ! array read in from netcdf file
      integer :: nhem                       ! number of hemispheres
      integer :: ilat                       ! loop variable
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
         write(6, *) 'ERROR: read2d: opening ', filein
         stop
      endif
!----------------------------------------
! Find the dimension ID
!----------------------------------------
      status=nf_inq_dimid(ncid, 'longitude', longid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: read2d: could not longitude ID'
         stop
      endif
      status=nf_inq_dimid(ncid, 'latitude', latid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: read2d: could not latitude ID'
         stop
      endif
!----------------------------------------
! Find the size of the dimensions
!----------------------------------------
      status=nf_inq_dimlen(ncid, longid, nlong)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: read2d: Could not get longitude dimension'
         stop
      endif
      status=nf_inq_dimlen(ncid, latid, nlat)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: read2d: Could not get latitude dimension'
         stop
      endif
!----------------------------------------
! Check that input array is of the right size
!----------------------------------------
      if (xsize.eq.(2*(nlong+2))) then
         nhem=2
      elseif (xsize.eq.(nlong+2)) then
         nhem=1
      else
         print*,'ERROR: read2d: array dimension is wrong'
         print*,'number of longitudes is ',nlong, 
     &        ' so array should have ', (2*(nlong+2)), 
     &        ' elements in 1st dimension'
         nhem=0
      endif
      if (ysize.ne.(nlat/2)) then
         print*,'ERROR: read2d: array dimension is wrong'
         print*,'number of latitudes is ',nlat, 
     &        ' so array should have ', (nlat/2), 
     &        ' elements in 2nd dimension'
      endif
!----------------------------------------
! find the variable ID
!----------------------------------------
      status=nf_inq_varid(ncid, varname, varid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: read2d: could not find ', varname
         stop
      endif
!----------------------------------------
! read in variable to arrayin
!----------------------------------------


      if (realkind.eq.4) then
        status=nf_get_var_real(ncid, varid, arrayin) 
      else if (realkind.eq.8) then
        status=nf_get_var_double(ncid, varid, arrayin) 
      else
        print*,'precision problem in read2d_nc'
        stop
      endif
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: read2d: getting variable'
         stop
      endif
!----------------------------------------
! pass information from arrayin to arrayout
!----------------------------------------
! Copy Northern Hemisphere into arrayout
      arrayout(1:nlong,1:nlat/2)=arrayin(1:nlong,1:nlat/2)
! Copy Southern Hemisphere into arrayout
      if (nhem.eq.2) then
         do ilat=1,nlat/2
            arrayout(nlong+3:2*nlong+2,ilat)=
     &           arrayin(1:nlong,nlat+1-ilat)
         enddo
      endif
!----------------------------------------
! close file
!----------------------------------------
      status=nf_close(ncid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERROR: read2d: closing ', filein
         stop
      endif

      end

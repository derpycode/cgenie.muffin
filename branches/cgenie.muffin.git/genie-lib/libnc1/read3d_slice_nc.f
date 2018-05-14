!========================================
! Marc 27/3/03
! Reads a slice of a 3D array from a netcdf file.
!========================================
      subroutine read3d_slice_nc(filein, varname, nl, mg, nhem
     &     , jg, jslice, arrayout)
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
      integer :: nl, mg, nhem, jg
      real :: arrayout(nl, mg, nhem, jg)
      integer :: jslice
!----------------------------------------
! Define the netcdf variables
!----------------------------------------
      integer :: ncid                       ! netCDF dataset ID
     &     , status                         ! return code
     &     , varid                          ! ID of variable
     &     , longid                         ! ID for longitude dimension
     &     , latid                          ! ID for latitude dimension
     &     , nlid                           ! ID for levels 
     &     , start(3)                       ! 1st elem. of netcdf var. read in
     &     , count(3)                       ! wanted elements in each direction
     &     , stride(3)                      ! subsmapling intervals
     &     , imap(3)                        ! internal array inter-elem. dist.
!----------------------------------------
! local variables
!----------------------------------------
      integer :: nlong                      ! number of longitudes
     &     , nlat                           ! number of latitudes
     &     , nl2                            ! number of levels
      real :: arrayin(nl, mg)               ! array used to read in netcdf data
!----------------------------------------
! Include files
!----------------------------------------
      include 'netcdf.inc'

      realkind=kind(arrayout)

!----------------------------------------
! open the netcdf file
!----------------------------------------
      print*,'about to open ---', filein, '---'
      status=nf_open(filein, nf_nowrite, ncid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: opening ', filein
         stop
      endif
!----------------------------------------
! Find the dimension ID
!----------------------------------------
      status=nf_inq_dimid(ncid, 'longitude', longid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: could not find longitude ID'
         stop
      endif
      status=nf_inq_dimid(ncid, 'latitude', latid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: could not find latitude ID'
         stop
      endif
      status=nf_inq_dimid(ncid, 'press', nlid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: could not vertical '
     :               //'dimension ID'
         stop
      endif
!----------------------------------------
! Find the size of the dimensions
!----------------------------------------
      status=nf_inq_dimlen(ncid, longid, nlong)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: Could not get longitude '
     :               //'dimension'
         stop
      endif
      status=nf_inq_dimlen(ncid, latid, nlat)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: Could not get latitude '
     :               //'dimension'
         stop
      endif
      status=nf_inq_dimlen(ncid, nlid, nl2)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: Could not get vertical '
     :               //'dimension'
         stop
      endif
!----------------------------------------
! check dimensions are the right size
!----------------------------------------
      if (nl.ne.nl2) then
         write(6, *) 'ERR: read3d_slice: vertical dimension of array '
     &        //' and netcdf data do not correspond'
         stop
      endif
      if (mg.ne.nlong) then 
         write(6, *) 'ERR: read3d_slice: longitude dimension of array '
     &        //' and netcdf data do not correspond'
         stop
      endif
      if (jg.ne.nlat/2) then 
         write(6, *) 'ERR: read3d_slice: latitude dimension of array '
     &        //' and netcdf data do not correspond'
         stop
      endif
!----------------------------------------
! find the variable ID
!----------------------------------------
      status=nf_inq_varid(ncid, varname, varid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: could not find ', varname
         stop
      endif
!----------------------------------------
! read in slice from the Northen Hemisphere
!----------------------------------------
      start(1)=1
      start(2)=jslice
      start(3)=1
      count(1)=mg
      count(2)=1
      count(3)=nl
      stride(1:3)=1
      imap(1)=nl
      imap(2)=mg*nl
      imap(3)=1

      if (realkind.eq.4) then
        status=nf_get_varm_real(ncid, varid, start, count, stride
     &     , imap, arrayin)
      else if (realkind.eq.8) then
        status=nf_get_varm_double(ncid, varid, start, count, stride
     &     , imap, arrayin)
      else
        print*,'precision problem in read3d_slice_nc'
        stop
      endif

!----------------------------------------
! write to arrayout
!----------------------------------------
      arrayout(1:nl,1:mg,1,jslice)=arrayin(1:nl,1:mg)
!----------------------------------------
! read in slice from the Southen Hemisphere (if required)
!----------------------------------------
      if (nhem.eq.2) then
         start(2)=2*jg+1-jslice


      if (realkind.eq.4) then
         status=nf_get_varm_real(ncid, varid, start, count, stride
     &        , imap, arrayin)
      else if (realkind.eq.8) then
         status=nf_get_varm_double(ncid, varid, start, count, stride
     &        , imap, arrayin)
      else
        print*,'precision problem in read3d_slice_nc'
        stop
      endif

         !----------------------------------------
         ! write to arrayout
         !----------------------------------------
         arrayout(1:nl,1:mg,2,jslice)=arrayin(1:nl,1:mg)
      endif
!----------------------------------------
! close file
!----------------------------------------
      status=nf_close(ncid)
      if (status .ne. nf_noerr) then
         write(6, *) 'ERR: read3d_slice: closing ', filein
         stop
      endif

      end

c ======================================================================
c err_embm.f
c ======================================================================
c
c returns a rms error value for the specified EMBM model field compared
c with the contents of the supplied data file
c
c Revision history:
c =================
c
c Author  Date       Comments
c
c ARP     03/08/06   Created.
c                    line-for-line copy of the error calculation for the
c                    tq(1,:,:,:) field in diagend.f
c
      function err_embm(modeldata, tracerid, imax, jmax, indir_name,
     $     lenin, obsdatafile, lenobsdata, datascaling, dataoffset,
     $     interpolate, varname, missing, lon, lat)

      implicit none

c Error value
      real err_embm, err

c Grid size
      integer imax, jmax

c Model data field
      real modeldata(imax,jmax)

c Data field type
      integer tracerid

c scaling used to convert observational data to make them comparable to
c model data
      real datascaling, dataoffset

c Interpolate: if '.false.', read in observational data of model-grid
c resolution from file; if '.true.', read in observational data from
c NetCDF file and interpolate
      logical interpolate

c EMBM input/output directories
      integer lenin
      character indir_name*100

c EMBM T/S data files
      integer lenobsdata
      character obsdatafile*128

c observation-based dataset
      REAL                                 :: missing
      CHARACTER(25)                        :: varname

c EMBM grid
      REAL,DIMENSION(imax)                 :: lon
      REAL,DIMENSION(jmax)                 :: lat

c Weighting factor (reciprocal of obs. data variance)
      real errw

c Indices
      integer i,j

c Observational data, average and variance
      real obsdata(imax,jmax)
      real obsdata_av, obsdata_var

      call read_embm_target_field(tracerid,imax,jmax,indir_name,lenin
     $     ,obsdatafile,lenobsdata,datascaling,dataoffset,interpolate
     $     ,varname,missing,lon,lat,obsdata)

c calculate weights based on variance of data NB not real spatial but
c computational spatial 
      obsdata_av  = 0.
      obsdata_var = 0.
      do j=1,jmax
          do i=1,imax
              obsdata_av = obsdata_av + obsdata(i,j)
              obsdata_var= obsdata_var + obsdata(i,j)*obsdata(i,j)
          enddo
      enddo
      obsdata_av  = obsdata_av/(imax*jmax)
      obsdata_var = obsdata_var/(imax*jmax) - obsdata_av*obsdata_av
      errw        = 1.0/obsdata_var

c calculate the rms error
      err = 0.
      do j=1,jmax
          do i=1,imax
              err = err + errw * (modeldata(i,j) - obsdata(i,j))**2
          enddo
      enddo
      err_embm = sqrt(err/(imax*jmax))

c print error value
c print 400,tracerid,err_embm
c 400   format (' err_embm: weighted r.m.s. model-data error (',i1,') ',
c     &     g24.16)

      end function err_embm

c reading in of data-based target fields for comparison with the model's
c internal fields

      subroutine read_embm_target_field(tracerid,imax,jmax,indir_name
     $     ,lenin,obsdatafile,lenobsdata,datascaling,dataoffset
     $     ,interpolate,varname,missing,lon,lat,obsdata)

      use genie_util, ONLY: check_unit, check_iostat, die
      use local_netcdf

      implicit none

c Grid size
      integer imax, jmax

c Data field type
      integer tracerid

c scaling used to convert observational data to make them comparable to
c model data
      real datascaling, dataoffset

c Interpolate: if '.false.', read in observational data of model-grid
c resolution from file; if '.true.', read in observational data from
c NetCDF file and interpolate
      logical interpolate

c EMBM input/output directories
      integer lenin
      character indir_name*100

c EMBM T/S data files
      integer lenobsdata
      character obsdatafile*128

c observation-based dataset
      TYPE(real2dVar),DIMENSION(1)         :: tq_obs
      TYPE(real1dVar),DIMENSION(2)         :: tq_obs_axis
      INTEGER                              :: nx_obs,ny_obs
      REAL,POINTER,DIMENSION(:,:)          :: sinlat_obs
      INTEGER                              :: ncid_in, ncstatus
      INTEGER                              :: i_obs,j_obs
      INTEGER                              :: i_obs_min,j_obs_min
      INTEGER                              :: i0,i1,jtmp
      INTEGER                              :: ii,jj,iii,nwidth
      REAL                                 :: missing
      CHARACTER(25)                        :: varname
      REAL                                 :: obstmp

c EMBM grid
      REAL,DIMENSION(imax)                 :: lon
      REAL,DIMENSION(jmax)                 :: lat
      REAL,DIMENSION(2,jmax)               :: sinlat

c interpolation
      INTEGER                              :: n_int,n_ext
      REAL                                 :: distmean,distmax
      REAL                                 :: rlon,rlat
      REAL                                 :: testmask
      REAL                                 :: dist,distmin,distminocean
      REAL                                 :: cosdlon

c miscelaneous variables
      INTEGER                              :: status
      REAL                                 :: pi

c Indices
      integer i,j

c Observational data, average and variance
      real obsdata(imax,jmax)

c For file checks
      integer ios

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      i_obs_min = 0
      j_obs_min = 0
c ------------------------------------------------------------ c

      pi=4.0*atan(1.0)

      if (.not.interpolate) then

c Open the data file
         call check_unit(32,__LINE__,__FILE__)
         open(32,file=indir_name(1:lenin)//obsdatafile(1:lenobsdata),
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         read(32,*,iostat=ios)((obsdata(i,j),i=1,imax),j=1,jmax)
         call check_iostat(ios,__LINE__,__FILE__)
         close(32,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

c Apply a scaling for the humidity data
         if (tracerid.eq.2) then
            do j=1,jmax
               do i=1,imax
                  obsdata(i,j) = obsdata(i,j) * 1.e-3
               enddo
            enddo
         endif

      else
c     Read in 'netCDF' dataset and interpolate onto model grid
         call openNetCDFRead(indir_name(1:lenin)//
     &        obsdatafile(1:lenobsdata),ncid_in)
         tq_obs(1)%name = varname
         call lookupVars(ncid_in,tq_obs)
c     size of observational dataset
         nx_obs=tq_obs(1)%dimLens(1)
         ny_obs=tq_obs(1)%dimLens(2)
         allocate(tq_obs(1)%data(0:nx_obs,
     &        1:ny_obs),stat=status)
         if (status /= 0) call die("Could not allocate memory")
         ncstatus = nf90_get_var(ncid_in,tq_obs(1)%id,
     &        tq_obs(1)%data(1:nx_obs,1:ny_obs))
         if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
         tq_obs_axis(1)%name = tq_obs(1)%dimnames(1)
         tq_obs_axis(2)%name = tq_obs(1)%dimnames(2)
c     Note, the zeroth longitude index represents the same values as for the
c     last value (the actual coordinate is offset by 360 degrees) to
c     facilitate dealing with periodicity of the longitude
         call lookupVars(ncid_in,tq_obs_axis)
         allocate(tq_obs_axis(1)%data(0:nx_obs),
     &        stat=status)
         if (status /= 0) call die("Could not allocate memory")
         allocate(tq_obs_axis(2)%data(1:ny_obs),
     &        stat=status)
         if (status /= 0) call die("Could not allocate memory")
         ncstatus = nf90_get_var(ncid_in,tq_obs_axis(1)%id,
     &        tq_obs_axis(1)%data(1:nx_obs))
         if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
         tq_obs_axis(1)%data(0)=tq_obs_axis(1)%data(nx_obs)-360.
         do j=1,ny_obs
            do i=1,nx_obs
               if (abs((tq_obs(1)%data(i,j)-missing)/missing).lt.1e-5)
     $              then
                  tq_obs(1)%data(i,j)=9.99999e19
               endif
            enddo
            tq_obs(1)%data(0,j)=tq_obs(1)%data(nx_obs,j)
         enddo
         ncstatus = nf90_get_var(ncid_in,tq_obs_axis(2)%id,
     &        tq_obs_axis(2)%data)
         if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
c     prepare auxiliary arrays:
c     first index    function
c     1            sin()
c     2            cos()
         allocate(sinlat_obs(2,ny_obs),stat=status)
         if (status /= 0) call die("Could not allocate memory")
! for grid of observation-based dataset
         do j=1,ny_obs
            sinlat_obs(1,j) = sin(tq_obs_axis(2)%data(j)*pi/180.)
            sinlat_obs(2,j) = cos(tq_obs_axis(2)%data(j)*pi/180.)
         enddo
! for GOLDSTEIN grid
         do j=1,jmax
            sinlat(1,j) = sin(lat(j)*pi/180.)
            sinlat(2,j) = cos(lat(j)*pi/180.)
         enddo
c     flip latitudinal axis if required; test monotonicity of axis of
c     observation-based dataset; convert GENIE's longitudinal axis to
c     same range as that of the observational dataset
         if (tq_obs_axis(2)%data(ny_obs).lt.tq_obs_axis(2)%data(1)) then
            do j=1,int(ny_obs/2+0.5)
               obstmp=tq_obs_axis(2)%data(j)
               tq_obs_axis(2)%data(j)=tq_obs_axis(2)%data(ny_obs+1-j)
               tq_obs_axis(2)%data(ny_obs+1-j)=obstmp
               do i=0,nx_obs
                  obstmp=tq_obs(1)%data(i,j)
                  tq_obs(1)%data(i,j)=tq_obs(1)%data(i,ny_obs+1-j)
                  tq_obs(1)%data(i,ny_obs+1-j)=obstmp
               enddo
            enddo
         endif
         do i=2,nx_obs
            if (tq_obs_axis(1)%data(i).le.tq_obs_axis(1)%data(i-1)) then
               call die("Non-incremental longitudinal axis")
            endif
         enddo
         do j=2,ny_obs
            if (tq_obs_axis(2)%data(j).le.tq_obs_axis(2)%data(j-1)) then
               call die("Non-incremental latitudinal axis")
            endif
         enddo
         do i=1,imax
            do while (lon(i).le.tq_obs_axis(1)%data(0))
               lon(i)=lon(i)+360.
            enddo
            do while (lon(i).gt.tq_obs_axis(1)%data(nx_obs))
               lon(i)=lon(i)-360.
            enddo
         enddo
c     bi-linear interpolation, parts of this code is based on the
c     interpolation routine 'genie-cgoldstein/laz2siz.f' (modified from
c     tri-linear to bi-linear interpolations), the "extrapolation" part
c     has been replaced by a horizontal search for the nearest valid
c     point on the sphere.
         n_int=0
         distmean=0.
         distmax=0.
         n_ext=0
         do j=1,jmax
            do i=1,imax
c     find location of model grid point on observation-based
c     grid.
               i_obs = 0
               do while ((tq_obs_axis(1)%data(i_obs).lt.lon(i)).and.
     &              (i_obs.le.nx_obs))
                  i_obs = i_obs+1
               enddo
c     This could possibly be done more general without the restriction that
c     any model point has to be inside the extremes of the latitude
c     coordinates of the observation-based grid
               j_obs = 1
               do while ((tq_obs_axis(2)%data(j_obs).lt.lat(j)).and.
     &              (j_obs.le.ny_obs))
                  j_obs = j_obs+1
               enddo
               if ((i_obs.eq.0).or. (i_obs.gt.nx_obs).or.
     $              (j_obs.eq.1).or. (j_obs.gt.ny_obs)) then
                  call die("Coordinates or depth outside of the"//
     $                 " boundaries set by observational dataset")
               endif
               rlon = (lon(i)-tq_obs_axis(1)%data(i_obs-1))/
     &              (tq_obs_axis(1)%data(i_obs)-
     &              tq_obs_axis(1)%data(i_obs-1))
               rlat = (lat(j)-tq_obs_axis(2)%data(j_obs-1))/
     &              (tq_obs_axis(2)%data(j_obs)-
     &              tq_obs_axis(2)%data(j_obs-1))
               testmask = max(tq_obs(1)%data(i_obs,j_obs),
     $              tq_obs(1)%data(i_obs-1,j_obs),
     $              tq_obs(1)%data(i_obs,j_obs-1),
     $              tq_obs(1)%data(i_obs-1,j_obs-1))
c     interpolate if no land at corners of cube encompassing the model grid
c     location
               if (testmask.lt.1.e10) then
                  obsdata(i,j) = (1.0-rlon)*((1.0-rlat)*
     $                 tq_obs(1)%data(i_obs-1,j_obs-1)+ rlat
     $                 *tq_obs(1)%data(i_obs-1,j_obs))+ rlon
     $                 *((1.0-rlat)* tq_obs(1)%data(i_obs,j_obs-1
     $                 )+ rlat*tq_obs(1)%data(i_obs,j_obs
     $                 ))
                  n_int=n_int+1
               else
c     find horizonatlly nearest (true distance on sphere) point

c     to compute arc distance dist between two points ((lon1,lat1) and
c     (lon2,lat2)) on a sphere use:
c     dist=arccos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lat2-lat1))
c     Note, this formula is affected by rounding errors for small angles, so
c     resolution of close points is limited, especially if 4-byte
c     arithmetic/trigonometry is used

c     start with rectangle defined by (i_obs-1,j_obs-1), (i_obs,j_obs),
c     find within newly added points both the nearest valid point AND
c     the nearest point
                  distmin = pi
                  distminocean = pi
                  do ii=1,2
                     do jj=1,2
                        cosdlon=cos(pi*(lon(i)-
     &                       tq_obs_axis(1)%data(i_obs+1-ii))/
     &                       180.)
                        jtmp=j_obs+1-jj
                        dist = acos(sinlat(1,j)*
     &                       sinlat_obs(1,jtmp)+
     &                       sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                       cosdlon)
                        distmin=min(distmin,dist)
                        testmask=max(tq_obs(1)%data(i_obs+1-ii,
     &                       jtmp),
     &                       tq_obs(1)%data(i_obs+1-ii,
     &                       jtmp))
                        if ((testmask.lt.1e10).and.
     &                       (distminocean.gt.dist)) then
                           distminocean=dist
                           i_obs_min=i_obs+1-ii
                           j_obs_min=jtmp
                        endif
                     enddo
                  enddo
                  nwidth=1
c     repeat until nearest of the newly added points is farther away
c     than nearest valid point,
                  do while ((distmin.lt.distminocean).and.
     &                 (nwidth.lt.int(nx_obs/2)+1).and.
     &                 (nwidth.lt.ny_obs))
                     distmin = pi
c     add grid-point circumference around rectangle, take into account
c     periodicity in longitudinal direction and also look across northern
c     and southern poles.
c     find nearest valid point AND nearest point within newly added
c     points
                     nwidth=nwidth+1
c     reflect i range if rectangle spreads across northern or southern
c     border
                     if ((j_obs-nwidth).lt.1) then
                        i0=i_obs-nwidth-int(nx_obs/2)
                        i1=i_obs-1+nwidth-int(nx_obs/2)
                        jtmp=abs(j_obs-nwidth-1)
                     else
                        i0=i_obs-nwidth
                        i1=i_obs-1+nwidth
                        jtmp=j_obs-nwidth
                     endif
                     do ii=i0,i1
                        iii=modulo(ii-1,nx_obs)+1
                        cosdlon=cos(pi*(lon(i)-
     &                       tq_obs_axis(1)%data(iii))/
     &                       180.)
                        dist = acos(sinlat(1,j)*
     &                       sinlat_obs(1,jtmp)+
     &                       sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                       cosdlon)
                        distmin=min(distmin,dist)
                        testmask=max(tq_obs(1)%data(iii,j_obs-
     &                       nwidth),tq_obs(1)%data(iii,j_obs-
     &                       nwidth))
                        if ((testmask.lt.1e10).and.
     &                       (distminocean.gt.dist)) then
                           distminocean=dist
                           i_obs_min=iii
                           j_obs_min=jtmp
                        endif
                     enddo
c     reflect i range if rectangle spreads across northern or southern
c     border
                     if ((j_obs-1+nwidth).gt.ny_obs) then
                        i0=i_obs-nwidth-int(nx_obs/2)
                        i1=i_obs-1+nwidth-int(nx_obs/2)
                        jtmp=2*ny_obs-(j_obs+nwidth-2)
                     else
                        i0=i_obs-nwidth
                        i1=i_obs-1+nwidth
                        jtmp=j_obs-1+nwidth
                     endif
                     do ii=i0,i1
                        iii=modulo(ii-1,nx_obs)+1
                        cosdlon=cos(pi*(lon(i)-
     &                       tq_obs_axis(1)%data(iii))/
     &                       180.)
                        dist = acos(sinlat(1,j)*
     &                       sinlat_obs(1,jtmp)+
     &                       sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                       cosdlon)
                        distmin=min(distmin,dist)
                        testmask=max(tq_obs(1)%data(iii,
     &                       j_obs-1+nwidth),
     &                       tq_obs(1)%data(iii,
     &                       j_obs-1+nwidth))
                        if ((testmask.lt.1e10).and.
     &                       (distminocean.gt.dist)) then
                           distminocean=dist
                           i_obs_min=iii
                           j_obs_min=jtmp
                        endif
                     enddo
                     do jj=j_obs-nwidth+1,j_obs-2+nwidth
c     reflect i range if rectangle spreads across northern or southern
c     border
                        if ((jj.lt.1).or.(jj.gt.ny_obs)) then
                           iii=modulo(i_obs-nwidth-1+int(nx_obs/2),
     &                          nx_obs)
                           if (jj.lt.1) then
                              jtmp=abs(jj-1)
                           else
                              jtmp=2*ny_obs-(jj-1)
                           endif
                        else
                           iii=modulo(i_obs-nwidth-1,nx_obs)+1
                           jtmp=jj
                        endif
                        cosdlon=cos(pi*(lon(i)-
     &                       tq_obs_axis(1)%data(iii))/
     &                       180.)
                        dist = acos(sinlat(1,j)*
     &                       sinlat_obs(1,jtmp)+
     &                       sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                       cosdlon)
                        distmin=min(distmin,dist)
                        testmask=max(tq_obs(1)%data(iii,jj),
     &                       tq_obs(1)%data(iii,
     &                       jj))
                        if ((testmask.lt.1e10).and.
     &                       (distminocean.gt.dist)) then
                           distminocean=dist
                           i_obs_min=iii
                           j_obs_min=jtmp
                        endif
                     enddo
                     do jj=j_obs-nwidth+1,j_obs-2+nwidth
c     reflect i range if rectangle spreads across northern or southern
c     border
                        if ((jj.lt.1).or.(jj.gt.ny_obs)) then
                           iii=modulo(i_obs-2+nwidth+int(nx_obs/2),
     &                          nx_obs)
                           if (jj.lt.1) then
                              jtmp=abs(jj-1)
                           else
                              jtmp=2*ny_obs-(jj-1)
                           endif
                        else
                           iii=modulo(i_obs-2+nwidth,nx_obs)+1
                           jtmp=jj
                        endif
                        cosdlon=cos(pi*(lon(i)-
     &                       tq_obs_axis(1)%data(iii))/
     &                       180.)
                        dist = acos(sinlat(1,j)*
     &                       sinlat_obs(1,jtmp)+
     &                       sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                       cosdlon)
                        distmin=min(distmin,dist)
                        testmask=max(tq_obs(1)%data(iii,
     &                       jj),
     &                       tq_obs(1)%data(iii,jj))
                        if ((testmask.lt.1e10).and.
     &                       (distminocean.gt.dist)) then
                           distminocean=dist
                           i_obs_min=iii
                           j_obs_min=jtmp
                        endif
                     enddo
                  enddo
c     vertically interpolate at point with shortest distance from target point
                  obsdata(i,j) = tq_obs(1)%data(i_obs_min,
     $                 j_obs_min)
                  distmean=distmean+distminocean
                  if (distminocean.gt.distmax) then
                     distmax=distminocean
                  endif
                  n_ext=n_ext+1
               endif
            enddo
         enddo
         if (n_ext.gt.0) then
            distmean=distmean/real(n_ext)
         endif
         print *, 'fraction of interpolated points,'
         print *, 'fraction of extrapolated points,'
         print *, 'mean distance of extrapolated points (degrees),'
         print *, 'maximum distance of extrapolated point (degrees)'
         print *, real(n_int)/real(n_int+n_ext),
     &        real(n_ext)/real(n_int+n_ext),
     &        distmean*180./pi,distmax*180./pi
      
! Clean up
         deallocate(sinlat_obs,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         deallocate(tq_obs_axis(1)%data,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         deallocate(tq_obs_axis(2)%data,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         deallocate(tq_obs(1)%data,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         call closeNetCDF(ncid_in)

c Apply a scaling for the humidity data
         do j=1,jmax
            do i=1,imax
               obsdata(i,j) = obsdata(i,j)/datascaling-dataoffset
            enddo
         enddo

      endif

      end

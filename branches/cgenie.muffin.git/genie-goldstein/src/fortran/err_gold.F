c ======================================================================
c err_gold.f
c ======================================================================
c
c returns a rms error value for the specified GOLDSTEIN model field
c compared with the contents of the supplied data file
c
c Revision history:
c =================
c
c Author  Date       Comments
c
c ARP     03/08/06   Created.
c                    line-for-line copy of the error calculation for the
c                    ts(1,:,:,:) field in diagend.f
c
c ARP     09/08/06   Ignore cells where observational data is missing
c                    ie. ignore cells that have a null value (<-1.e10)
c                    Evaluate ntot within the function since this is now
c                    dependent on the number of valid cells
c
      function err_gold(modeldata, tracerid, k1, ntot, imax, jmax, kmax
     $     ,indir_name, lenin, obsdatafile, lenobsdata, datascaling,
     $     dataoffset,interpolate, varname, missing, lon, lat, z)

      implicit none

c Error value
      real err_gold, err

c Grid size
      integer imax, jmax, kmax

c Model data field
      real modeldata(imax,jmax,kmax)

c Data field type
      integer tracerid

c Topography
      integer k1(0:imax+1,0:jmax+1)

c Number of wet grid cells
      integer ntot

c offset used to convert observational data to make them comparable to
c model data
      real datascaling, dataoffset

c Interpolate: if '.false.', read in observational data of model-grid
c resolution from file; if '.true.', read in observational data from
c NetCDF file and interpolate
      logical interpolate

c GOLD input/output directories
      integer lenin
      character indir_name*200

c GOLD T/S data files
      integer lenobsdata
      character obsdatafile*128

c observation-based dataset
      REAL                                 :: missing
      CHARACTER(25)                        :: varname

c GOLDSTEIN grid
      REAL,DIMENSION(imax)                 :: lon
      REAL,DIMENSION(jmax)                 :: lat
      REAL,DIMENSION(kmax)                 :: z

c Weighting factor (reciprocal of obs. data variance)
      real errw

c Indices
      integer i,j,k

c Observational data, average and variance
      real obsdata(imax,jmax,kmax)
      real obsdata_av, obsdata_var

      call read_gold_target_field(tracerid,k1,imax,jmax,kmax ,indir_name
     $     ,lenin,obsdatafile,lenobsdata,datascaling, dataoffset
     $     ,interpolate,varname,missing,lon,lat,z,obsdata)

c calculate weights based on variance of data NB not real spatial but
c computational spatial 
      obsdata_av  = 0.
      obsdata_var = 0.
      ntot = 0
      do k=1,kmax
          do j=1,jmax
              do i=1,imax
                  if(k.ge.k1(i,j).and.obsdata(i,j,k).gt.-1e10)then
                      obsdata_av = obsdata_av + obsdata(i,j,k)
                      obsdata_var= obsdata_var + 
     &                             obsdata(i,j,k)*obsdata(i,j,k)
                      ntot = ntot + 1
                  endif
              enddo
          enddo
      enddo
      obsdata_av  = obsdata_av/ntot
      obsdata_var = obsdata_var/ntot - obsdata_av*obsdata_av
      errw        = 1.0/obsdata_var

c calculate the rms error
      err = 0.
      do j=1,jmax
          do i=1,imax
              do k=1,kmax
                  if(k.ge.k1(i,j).and.obsdata(i,j,k).gt.-1e10)then
                      err = err + errw * (modeldata(i,j,k) - 
     &                                    obsdata(i,j,k))**2
                  endif
              enddo
          enddo
      enddo
      err_gold = sqrt(err/ntot)

c print error value
      print 400,tracerid,err_gold
400   format (' err_gold: weighted r.m.s. model-data error (',i1,') ',
     &     g24.16)

      end function err_gold

c reading in of data-based target fields for comparison with the model's
c internal fields

      subroutine read_gold_target_field(tracerid,k1,imax,jmax,kmax
     $     ,indir_name,lenin,obsdatafile,lenobsdata,datascaling,
     $     dataoffset,interpolate,varname,missing,lon,lat,z,obsdata)

      use genie_util, ONLY: check_unit, check_iostat, die
      use local_netcdf

      implicit none

c Grid size
      integer imax, jmax, kmax

c Data field type
      integer tracerid

c Topography
      integer k1(0:imax+1,0:jmax+1)

c offset used to convert observational data to make them comparable to
c model data
      real datascaling, dataoffset

c Interpolate: if '.false.', read in observational data of model-grid
c resolution from file; if '.true.', read in observational data from
c NetCDF file and interpolate
      logical interpolate

c GOLD input/output directories
      integer lenin
      character indir_name*200

c GOLD T/S data files
      integer lenobsdata
      character obsdatafile*128

c observation-based dataset
      TYPE(real3dVar),DIMENSION(1)         :: ts_obs
      TYPE(real1dVar),DIMENSION(3)         :: ts_obs_axis
      INTEGER                              :: nx_obs,ny_obs,nz_obs
      REAL,POINTER,DIMENSION(:,:)          :: sinlat_obs
      INTEGER                              :: ncid_in, ncstatus
      INTEGER                              :: i_obs,j_obs,k_obs
      INTEGER                              :: i_obs_min,j_obs_min
      INTEGER                              :: i0,i1,jtmp
      INTEGER                              :: ii,jj,iii,nwidth
      REAL                                 :: missing
      CHARACTER(25)                        :: varname
      REAL                                 :: obstmp

c GOLDSTEIN grid
      REAL,DIMENSION(imax)                 :: lon
      REAL,DIMENSION(jmax)                 :: lat
      REAL,DIMENSION(kmax)                 :: z
      REAL,DIMENSION(2,jmax)               :: sinlat

c interpolation
      INTEGER                              :: n_int,n_ext
      REAL                                 :: distmean,distmax
      REAL                                 :: rlon,rlat,rz
      REAL                                 :: testmask
      REAL                                 :: dist,distmin,distminocean
      REAL                                 :: cosdlon

c miscelaneous variables
      INTEGER                              :: status
      REAL                                 :: pi

c Indices
      integer i,j,k

c Salinity scaling factor
      real saln0

c Observational data, average and variance
      real obsdata(imax,jmax,kmax)

c Error checking
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
         call check_unit(30,__LINE__,__FILE__)
         open(30,file=indir_name(1:lenin)//obsdatafile(1:lenobsdata),
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         read(30,*,iostat=ios)(((obsdata(i,j,k),k=1,kmax),i=1,imax),j=1,
     &        jmax)
         call check_iostat(ios,__LINE__,__FILE__)
         close(30,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

c Apply a scaling for the salinity data
         if (tracerid.eq.2) then
            saln0 = 34.9
            do j=1,jmax
               do i=1,imax
                  do k=1,kmax
                     obsdata(i,j,k) = obsdata(i,j,k) - saln0
                  enddo
               enddo
            enddo
         endif

      else
c     Read in 'netCDF' dataset and interpolate onto model grid
         call openNetCDFRead(indir_name(1:lenin)//
     &        obsdatafile(1:lenobsdata),ncid_in)
         ts_obs(1)%name = varname
         call lookupVars(ncid_in,ts_obs)
c     size of observational dataset
         nx_obs=ts_obs(1)%dimLens(1)
         ny_obs=ts_obs(1)%dimLens(2)
         nz_obs=ts_obs(1)%dimLens(3)
         allocate(ts_obs(1)%data(0:nx_obs,
     &        1:ny_obs,1:nz_obs),stat=status)
         if (status /= 0) call die("Could not allocate memory")
         ncstatus = nf90_get_var(ncid_in,ts_obs(1)%id,
     &        ts_obs(1)%data(1:nx_obs,1:ny_obs,1:nz_obs))
         if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
         ts_obs_axis(1)%name = ts_obs(1)%dimnames(1)
         ts_obs_axis(2)%name = ts_obs(1)%dimnames(2)
         ts_obs_axis(3)%name = ts_obs(1)%dimnames(3)
c     Note, the zeroth longitude index represents the same values as for the
c     last value (the actual coordinate is offset by 360 degrees) to
c     facilitate dealing with periodicity of the longitude
         call lookupVars(ncid_in,ts_obs_axis)
         allocate(ts_obs_axis(1)%data(0:nx_obs),
     &        stat=status)
         if (status /= 0) call die("Could not allocate memory")
         allocate(ts_obs_axis(2)%data(1:ny_obs),
     &        stat=status)
         if (status /= 0) call die("Could not allocate memory")
         allocate(ts_obs_axis(3)%data(1:nz_obs),
     &        stat=status)
         if (status /= 0) call die("Could not allocate memory")
         ncstatus = nf90_get_var(ncid_in,ts_obs_axis(1)%id,
     &        ts_obs_axis(1)%data(1:nx_obs))
         if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
         ts_obs_axis(1)%data(0)=ts_obs_axis(1)%data(nx_obs)-360.
         do j=1,ny_obs
            do k=1,nz_obs
               do i=1,nx_obs
                  if (abs((ts_obs(1)%data(i,j,k)-missing)/missing).lt.
     $                 1e-5) then
                     ts_obs(1)%data(i,j,k)=9.99999e19
                  endif
               enddo
               ts_obs(1)%data(0,j,k)=ts_obs(1)%data(nx_obs,j,k)
            enddo
         enddo
         ncstatus = nf90_get_var(ncid_in,ts_obs_axis(2)%id,
     &        ts_obs_axis(2)%data)
         if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
         ncstatus = nf90_get_var(ncid_in,ts_obs_axis(3)%id,
     &        ts_obs_axis(3)%data)
         if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
c     prepare auxiliary arrays:
c     first index    function
c     1            sin()
c     2            cos()
         allocate(sinlat_obs(2,ny_obs),stat=status)
         if (status /= 0) call die("Could not allocate memory")
! for grid of observation-based dataset
         do j=1,ny_obs
            sinlat_obs(1,j) = sin(ts_obs_axis(2)%data(j)*pi/180.)
            sinlat_obs(2,j) = cos(ts_obs_axis(2)%data(j)*pi/180.)
         enddo
! for GOLDSTEIN grid
         do j=1,jmax
            sinlat(1,j) = sin(lat(j)*pi/180.)
            sinlat(2,j) = cos(lat(j)*pi/180.)
         enddo
c     flip latitudinal and depth axes if required; if necessary, convert
c     depth axis to positive depth; test monotonicity of axis of
c     observation-based dataset; convert GENIE's longitudinal axis to
c     same range as that of the observational dataset
         if (ts_obs_axis(2)%data(ny_obs).lt.ts_obs_axis(2)%data(1)) then
            do j=1,int(ny_obs/2+0.5)
               obstmp=ts_obs_axis(2)%data(j)
               ts_obs_axis(2)%data(j)=ts_obs_axis(2)%data(ny_obs+1-j)
               ts_obs_axis(2)%data(ny_obs+1-j)=obstmp
               do i=0,nx_obs
                  do k=1,nz_obs
                     obstmp=ts_obs(1)%data(i,j,k)
                     ts_obs(1)%data(i,j,k)=ts_obs(1)%data(i,ny_obs+1-j
     $                    ,k)
                     ts_obs(1)%data(i,ny_obs+1-j,k)=obstmp
                  enddo
               enddo
            enddo
         endif
         do k=1,nz_obs
            if (ts_obs_axis(3)%data(k).lt.0) ts_obs_axis(3)%data(k)
     $           =abs(ts_obs_axis(3)%data(k))
         enddo
         if (ts_obs_axis(3)%data(ny_obs).lt.ts_obs_axis(3)%data(1)) then
            do k=1,int(nz_obs/2+0.5)
               obstmp=ts_obs_axis(3)%data(k)
               ts_obs_axis(3)%data(k)=ts_obs_axis(3)%data(nz_obs+1-k)
               ts_obs_axis(3)%data(nz_obs+1-k)=obstmp
               do i=1,nx_obs
                  do j=1,ny_obs
                     obstmp=ts_obs(1)%data(i,j,k)
                     ts_obs(1)%data(i,j,k)=ts_obs(1)%data(i,j,nz_obs+1
     $                    -k)
                     ts_obs(1)%data(i,j,nz_obs+1-k)=obstmp
                  enddo
               enddo
            enddo
         endif
         do i=2,nx_obs
            if (ts_obs_axis(1)%data(i).le.ts_obs_axis(1)%data(i-1)) then
               call die("Non-incremental longitudinal axis")
            endif
         enddo
         do j=2,ny_obs
            if (ts_obs_axis(2)%data(j).le.ts_obs_axis(2)%data(j-1)) then
               call die("Non-incremental latitudinal axis")
            endif
         enddo
         do k=2,nz_obs
            if (ts_obs_axis(3)%data(k).le.ts_obs_axis(3)%data(k-1)) then
               call die("Non-incremental depth axis")
            endif
         enddo
         do i=1,imax
            do while (lon(i).le.ts_obs_axis(1)%data(0))
               lon(i)=lon(i)+360.
            enddo
            do while (lon(i).gt.ts_obs_axis(1)%data(nx_obs))
               lon(i)=lon(i)-360.
            enddo
         enddo
c     tri-linear interpolation, parts of this code is based on the
c     interpolation routine 'genie-cgoldstein/laz2siz.f', the
c     "extrapolation" part has been replaced by a horizontal search for the
c     nearest valid point on the sphere.
         n_int=0
         distmean=0.
         distmax=0.
         n_ext=0
         do k=1,kmax
            do j=1,jmax
               do i=1,imax
                  if (k1(i,j).le.(kmax+1-k)) then
c     find location of model grid point on observation-based
c     grid.
                     i_obs = 0
                     do while ((ts_obs_axis(1)%data(i_obs).lt.lon(i))
     &                    .and.(i_obs.le.nx_obs))
                        i_obs = i_obs+1
                     enddo
c     This could possibly be done more general without the restriction that
c     any model point has to be inside the extremes of the latitude
c     coordinates of the observation-based grid
                     j_obs = 1
                     do while ((ts_obs_axis(2)%data(j_obs).lt.lat(j)).and.
     &                    (j_obs.le.ny_obs))
                        j_obs = j_obs+1
                     enddo
                     k_obs = 1
                     do while ((ts_obs_axis(3)%data(k_obs).lt.z(k)).and.
     &                    (k_obs.le.nz_obs))
                        k_obs = k_obs+1
                     enddo
                     if ((i_obs.eq.0).or.
     &                    (i_obs.gt.nx_obs).or.
     &                    (j_obs.eq.1).or.
     &                    (j_obs.gt.ny_obs).or.
     &                    (k_obs.eq.1).or.
     &                    (k_obs.gt.nz_obs)) then
                        call die("Coordinates or depth outside of the"//
     $                       " boundaries set by observational dataset")
                     endif
                     rlon = (lon(i)-ts_obs_axis(1)%data(i_obs-1))/
     &                    (ts_obs_axis(1)%data(i_obs)-
     &                    ts_obs_axis(1)%data(i_obs-1))
                     rlat = (lat(j)-ts_obs_axis(2)%data(j_obs-1))/
     &                    (ts_obs_axis(2)%data(j_obs)-
     &                    ts_obs_axis(2)%data(j_obs-1))
                     rz = (z(k)-ts_obs_axis(3)%data(k_obs-1))/
     &                    (ts_obs_axis(3)%data(k_obs)-
     &                    ts_obs_axis(3)%data(k_obs-1))
                     testmask = max(ts_obs(1)%data(i_obs,j_obs,k_obs),
     &                    ts_obs(1)%data(i_obs-1,j_obs,k_obs),
     &                    ts_obs(1)%data(i_obs,j_obs-1,k_obs),
     &                    ts_obs(1)%data(i_obs,j_obs,k_obs-1),
     &                    ts_obs(1)%data(i_obs-1,j_obs-1,k_obs),
     &                    ts_obs(1)%data(i_obs,j_obs-1,k_obs-1),
     &                    ts_obs(1)%data(i_obs-1,j_obs,k_obs-1),
     &                    ts_obs(1)%data(i_obs-1,j_obs-1,k_obs-1))
c     interpolate if no land at corners of cube encompassing the model grid
c     location
                     if (testmask.lt.1.e10) then
                        obsdata(i,j,kmax+1-k) = (1.0-rz)*((1.0-rlon)*(
     &                       (1.0-rlat)*ts_obs(1)%data(i_obs-1,
     &                       j_obs-1,k_obs-1)+
     &                       rlat*ts_obs(1)%data(i_obs-1,
     &                       j_obs,k_obs-1))+
     &                       rlon*((1.0-rlat)*
     &                       ts_obs(1)%data(i_obs,j_obs-1,k_obs-1)+
     &                       rlat*ts_obs(1)%data(i_obs,j_obs,k_obs-1)))+
     &                       rz*((1.0-rlon)*((1.0-rlat)*
     &                       ts_obs(1)%data(i_obs-1,j_obs-1,k_obs)+
     &                       rlat*ts_obs(1)%data(i_obs-1,j_obs,k_obs))+
     &                       rlon*((1.0-rlat)*
     &                       ts_obs(1)%data(i_obs,j_obs-1,k_obs)+
     &                       rlat*ts_obs(1)%data(i_obs,j_obs,k_obs)))
                        n_int=n_int+1
                     else
c     find horizonatlly nearest (true distance on sphere) vertically
c     adjacant pair of ocean points, BUT retain vertical interpolation
c     (analogous to 'genie-cgoldstein/laz2siz.f')

c     to compute arc distance dist between two points ((lon1,lat1) and
c     (lon2,lat2)) on a sphere use:
c     dist=arccos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lat2-lat1))
c     Note, this formula is affected by rounding errors for small angles, so
c     resolution of close points is limited, especially if 4-byte
c     arithmetic/trigonometry is used

c     start with rectangle defined by (i_obs-1,j_obs-1), (i_obs,j_obs), find
c     within newly added points both the nearest valid vertical pair of
c     points AND the nearest pair of all points,
                        distmin = pi
                        distminocean = pi
                        do ii=1,2
                           do jj=1,2
                              cosdlon=cos(pi*(lon(i)-
     &                             ts_obs_axis(1)%data(i_obs+1-ii))/
     &                             180.)
                              jtmp=j_obs+1-jj
                              dist = acos(sinlat(1,j)*
     &                             sinlat_obs(1,jtmp)+
     &                             sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                             cosdlon)
                              distmin=min(distmin,dist)
                              testmask=max(ts_obs(1)%data(i_obs+1-ii,
     &                             jtmp,k_obs),
     &                             ts_obs(1)%data(i_obs+1-ii,
     &                             jtmp,k_obs-1))
                              if ((testmask.lt.1e10).and.
     &                             (distminocean.gt.dist)) then
                                 distminocean=dist
                                 i_obs_min=i_obs+1-ii
                                 j_obs_min=jtmp
                              endif
                           enddo
                        enddo
                        nwidth=1
c     repeat until nearest pair of newly added points is farther away than
c     nearest valid pair,
                        do while ((distmin.lt.distminocean).and.
     &                       (nwidth.lt.int(nx_obs/2)+1).and.
     &                       (nwidth.lt.ny_obs))
                           distmin = pi
c     add grid-point circumference around rectangle, take into account
c     periodicity in longitudinal direction and also look across northern
c     and southern poles.
c     find nearest valid pair of points AND nearest pair of points within
c     newly added points
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
     &                             ts_obs_axis(1)%data(iii))/
     &                             180.)
                              dist = acos(sinlat(1,j)*
     &                             sinlat_obs(1,jtmp)+
     &                             sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                             cosdlon)
                              distmin=min(distmin,dist)
                              testmask=max(ts_obs(1)%data(iii,j_obs-
     &                             nwidth,
     &                             k_obs),ts_obs(1)%data(iii,j_obs-
     &                             nwidth,
     &                             k_obs-1))
                              if ((testmask.lt.1e10).and.
     &                             (distminocean.gt.dist)) then
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
     &                             ts_obs_axis(1)%data(iii))/
     &                             180.)
                              dist = acos(sinlat(1,j)*
     &                             sinlat_obs(1,jtmp)+
     &                             sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                             cosdlon)
                              distmin=min(distmin,dist)
                              testmask=max(ts_obs(1)%data(iii,
     &                             j_obs-1+nwidth,k_obs),
     &                             ts_obs(1)%data(iii,
     &                             j_obs-1+nwidth,k_obs-1))
                              if ((testmask.lt.1e10).and.
     &                             (distminocean.gt.dist)) then
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
     &                                nx_obs)
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
     &                             ts_obs_axis(1)%data(iii))/
     &                             180.)
                              dist = acos(sinlat(1,j)*
     &                             sinlat_obs(1,jtmp)+
     &                             sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                             cosdlon)
                              distmin=min(distmin,dist)
                              testmask=max(ts_obs(1)%data(iii,jj,
     &                             k_obs),ts_obs(1)%data(iii,
     &                             jj,k_obs-1))
                              if ((testmask.lt.1e10).and.
     &                             (distminocean.gt.dist)) then
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
     &                                nx_obs)
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
     &                             ts_obs_axis(1)%data(iii))/
     &                             180.)
                              dist = acos(sinlat(1,j)*
     &                             sinlat_obs(1,jtmp)+
     &                             sinlat(2,j)*sinlat_obs(2,jtmp)*
     &                             cosdlon)
                              distmin=min(distmin,dist)
                              testmask=max(ts_obs(1)%data(iii,
     &                             jj,k_obs),
     &                             ts_obs(1)%data(iii,jj,
     &                             k_obs-1))
                              if ((testmask.lt.1e10).and.
     &                             (distminocean.gt.dist)) then
                                 distminocean=dist
                                 i_obs_min=iii
                                 j_obs_min=jtmp
                              endif
                           enddo
                        enddo
c     vertically interpolate at point with shortest distance from target point
                        obsdata(i,j,kmax+1-k) = (1.0-rz)*
     &                       ts_obs(1)%data(i_obs_min,
     &                       j_obs_min,k_obs-1)+
     &                       rz*
     &                       ts_obs(1)%data(i_obs_min,
     &                       j_obs_min,k_obs)
                        distmean=distmean+distminocean
                        if (distminocean.gt.distmax) then
                           distmax=distminocean
                        endif
                        n_ext=n_ext+1
                     endif
                  else
                     obsdata(i,j,kmax+1-k)=-99.9999e19
                  endif
               enddo
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
         deallocate(ts_obs_axis(1)%data,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         deallocate(ts_obs_axis(2)%data,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         deallocate(ts_obs_axis(3)%data,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         deallocate(ts_obs(1)%data,stat=status)
         if (status /= 0) call die("Could not allocate memory")
         call closeNetCDF(ncid_in)

         do j=1,jmax
            do i=1,imax
               do k=1,kmax
                  obsdata(i,j,k) = obsdata(i,j,k)/datascaling
     &                 - dataoffset
               enddo
            enddo
         enddo

      endif

      end

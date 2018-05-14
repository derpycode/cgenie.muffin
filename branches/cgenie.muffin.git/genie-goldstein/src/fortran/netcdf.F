* AY (22/03/04)
* These netCDF routines were originally written for c-GOLDSTEIN by
* Paul Valdes, and have been altered to work with the componentised
* versions of GOLDSTEIN, the EMBM and GOLDSTEIN sea-ice.  The
* main changes are :
*
* - array bounds issues addressed in flip_both2 routine
* - GOLDSTEIN's -260E to +100E longitude grid restored 
* - ancillary longitude, latitude and depth arrays calculated in
*   module initialisation routines, not netCDF routines
* - new preparation routines for two dimensional fields (including
*   ocean bathymetry)
* - ordering of the routines in this file altered to reflect
*   call order
*
* Note : missing data flag should be -99999, but appears as another
* number in netCDF files.  As yet this discrepancy has not been
* tracked down and corrected.
*
* AY (30/11/04) : missing data flag issue resolved

* ======================================================================
* ini_netcdf_ocn
* ======================================================================
*
* AY (19/03/04)
* Begins netCDF initialisation process

      SUBROUTINE INI_NETCDF_OCN(istep,imode)
c
c
#include "ocean.cmn"
      include 'netcdf_grid.cmn' 
c
      integer istep,imode
c
c AY (17/03/04) : alon1, etc. excised to ocean.cmn (renamed nclon1, etc.)
c
c      integer i,j,l
      real day,rtime
c AY (17/03/04) : extra declarations
      integer iyear, imonth
c
c AY (17/03/04) : alon1, etc. now calculated in initialise_ocean.F
c
      day=(istep*dt(1)*tsc/86400.0)
c AY (17/03/04) : 365.25 days per GOLDSTEIN year (not 360.0)
c AY (23/03/04) : small constant added to day calculation for round-off
c                 reasons (i.e. 365.2499 vs. 365.25)
c AY (08/04/04) : year length parameterised (in initialise_goldstein.F)
      iyear=int((day + 0.001)/yearlen)
      imonth=int((day-iyear*yearlen)/(yearlen/12.))+1
c AY (24/03/04) : rtime added for netCDF file time
      rtime=iyear + ((day-iyear*yearlen))/yearlen
      if (debug_loop) 
     & print*,'istep',istep,'day',day,'iyear',iyear,'imonth',imonth,
     :     'rtime',rtime
c AY (23/03/04) : removing year standardisation to 2000
c     iyear=iyear+2000
c
c AY (19/03/04) : lout added to argument list to add identifier to
c                 netCDF filenames
      call ini_netcdf_ocn1(outdir_name,lenout,lout,
     :                    imonth,iyear,rtime,
     :                    nclon1,nclat1,
     :                    nclon2,nclat2,nclon3,nclat3,
     :                    ncdepth,ncdepth1,
     :                    imax,jmax,kmax,imode)
c
      return
      end

* ======================================================================
* ini_netcdf_ocn1
* ======================================================================
*
* AY (19/03/04)
* Continues netCDF initialisation process

      SUBROUTINE INI_NETCDF_OCN1(dir_name,ilen,runid,
     :                          imonth,iyear,rtime,
     :                          alon1,alat1,
     :                          alon2,alat2,alon3,alat3,
     :                          depth,depth1,
     :                          mg,jgg,nl,imode)
c
      implicit none
      include 'netcdf.cmn'
c
      integer nmaxdims
      parameter(nmaxdims=4)
      integer ndim,nvar,natts(nall),nattsvar(nall),
     :        vdims(nall),vadims(nmaxdims,nall),
     :        ndims(nall)
      character dimname(nall)*200,varname(nall)*200,
     :          attdimname(2,nmaxdims,nall)*200,
     :          attvarname(2,nmaxdims,nall)*200
c
      integer mg,jgg,nl
      real depth(nl),depth1(nl+1),
     :     alon1(mg),alon2(mg),alon3(mg),
     :     alat1(jgg),alat2(jgg),alat3(jgg)
      real rtime
      integer imonth,iyear,imode
c
      integer imax,jmax,kmax,lmax
      parameter(imax=100,jmax=100,kmax=100,lmax=10000)
      real xcoord(imax),ycoord(jmax),
     :        zcoord(kmax),tcoord(lmax)
      integer i,j,l,itime,ifname1,lnsig
      character dir_name*100
      integer ilen
      character runid*3
      character fname1*200
c AR (16/01/07): extended year string length from 4 to 10 characters
      character cyear*10
c AY (23/03/04) : adding a zero month for year end output
      character cmon(13)*2
      data cmon/'00','01','02','03','04','05','06',
     :          '07','08','09','10','11','12'/
c
      itime=1
c
      call setup_nc_ocn(mg,jgg,nl,itime,
     :                  nmaxdims,nall,
     :                  ndim,nvar,natts,nattsvar,vdims,
     :                  vadims,ndims,
     :                  dimname,varname,attdimname,
     :                  attvarname)
c
      write(cyear,'(i10.10)')iyear
c AY (19/03/04) : run identifier added to netCDF file names
      if (imode.eq.1) then
         fname1=dir_name(1:ilen)//'gold_'//runid(1:3)//'_rs_'
     :        //cyear//'_'//cmon(imonth)//'.nc'
      else if (imode.eq.2) then
         fname1=dir_name(1:ilen)//'gold_'//runid(1:3)//'_av_'
     :        //cyear//'_'//cmon(imonth)//'.nc'
      end if
c
      ifname1=lnsig(fname1)
ccc   print*,' Opening ',fname1(1:ifname1)
c 
      call ininc(fname1(1:ifname1),
     :           nmaxdims,ndim,nvar,
     :           natts,nattsvar,
     :           vdims,vadims,ndims,
     :           dimname,varname,
     :           attdimname,attvarname,
     :           nco(imode),iddimo(1,imode),idvaro(1,imode))
c
c     Longitude coordinates (tracer, u-point, v-point)
      do i=1,mg
         xcoord(i)=alon1(i)
      end do
      call writedim(nco(imode),iddimo(1,imode),xcoord)
      do i=1,mg
         xcoord(i)=alon2(i)
      end do
      call writedim(nco(imode),iddimo(2,imode),xcoord)
      do i=1,mg
         xcoord(i)=alon3(i)
      end do
      call writedim(nco(imode),iddimo(3,imode),xcoord)
c
c     Latitude coordinates (tracer, u-point, v-point)
      do j=1,jgg
         ycoord(j)=alat1(j)
      end do
      call writedim(nco(imode),iddimo(4,imode),ycoord)
      do j=1,jgg
         ycoord(j)=alat2(j)
      end do
      call writedim(nco(imode),iddimo(5,imode),ycoord)
c
c AY (07/10/03) : in the following lines, ycoord is extended (as was
c                 perhaps originally intended by Paul) by one point
c                 so that OPSI, etc. data can be correctly plotted.
c                 note : this doesn't affect the integrity of alat2
c
c AY (24/03/04) : alat3 is only jgg items long
c     do j=1,jgg+1
      do j=1,jgg
         ycoord(j)=alat3(j)
      end do
      ycoord(jgg+1)=90.
      call writedim(nco(imode),iddimo(6,imode),ycoord)
c
c     Depth coordinates (midpoint, box edges)
      do l=1,nl
         zcoord(l)=depth(l)
      end do
      call writedim(nco(imode),iddimo(7,imode),zcoord)
      do l=1,nl+1
         zcoord(l)=depth1(l)
      end do
      call writedim(nco(imode),iddimo(8,imode),zcoord)
c
c     Time
      do i=1,1
c AY (24/03/04) : rtime used as time coordinate
         tcoord(i)=real(rtime)
      end do
      call writedim(nco(imode),iddimo(9,imode),tcoord)
c
      return
      end

* ======================================================================
* setup_nc_ocn
* ======================================================================
*
* AY (19/03/04)
* Sets up netCDF file's array names, units, descriptions, etc.

      subroutine setup_nc_ocn(nlon,nlat,nl,ntime,
     :                 nmaxdims,nall,
     :                 ndim,nvar,natts,nattsvar,vdims,
     :                 vadims,ndims,
     :                 dimname,varname,attdimname,
     :                 attvarname)
      implicit none
      integer nlon,nlat,nl,ntime,nmaxdims,nall
      integer ndim,nvar,natts(nall),nattsvar(nall),
     :        vdims(nall),vadims(nmaxdims,nall),
     :        ndims(nall)
      character dimname(nall)*200,varname(nall)*200,
     :          attdimname(2,nmaxdims,nall)*200,
     :          attvarname(2,nmaxdims,nall)*200
c
      integer loc_dim
c
c     This sets up a file similar to .pc files
c
      ndim=0 
      nvar=0
c
      ndim=ndim+1
      dimname(ndim)='longitude'
      ndims(ndim)=nlon
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='longitude'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='degrees_east'
c
      ndim=ndim+1
      dimname(ndim)='longitude_1'
      ndims(ndim)=nlon
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='longitude'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='degrees_east'
c
      ndim=ndim+1
      dimname(ndim)='longitude_2'
      ndims(ndim)=nlon
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='longitude'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='degrees_east'
c
      ndim=ndim+1
      dimname(ndim)='latitude'
      ndims(ndim)=nlat
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='latitude'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='degrees_north'
c
      ndim=ndim+1
      dimname(ndim)='latitude_1'
      ndims(ndim)=nlat
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='latitude'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='degrees_north'
c
      ndim=ndim+1
      dimname(ndim)='latitude_2'
      ndims(ndim)=nlat+1
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='latitude'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='degrees_north'
c
      ndim=ndim+1
      dimname(ndim)='depth'
      ndims(ndim)=nl
      natts(ndim)=2
      attdimname(1,1,ndim)='units'
      attdimname(2,1,ndim)='m'
      attdimname(1,2,ndim)='positive'
      attdimname(2,2,ndim)='down'
c
      ndim=ndim+1
      dimname(ndim)='depth_1'
      ndims(ndim)=nl+1
      natts(ndim)=2
      attdimname(1,1,ndim)='units'
      attdimname(2,1,ndim)='m'
      attdimname(1,2,ndim)='positive'
      attdimname(2,2,ndim)='down'
c
      ndim=ndim+1
      dimname(ndim)='time'
      ndims(ndim)=ntime
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='time'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='years'
c
      nvar=nvar+1
      varname(nvar)='opsi'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('latitude_2',dimname,nall)
      vadims(2,nvar)=loc_dim('depth_1',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Global Meridional Streamfucntion'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='Sv'
c
      nvar=nvar+1
      varname(nvar)='opsi_a'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('latitude_2',dimname,nall)
      vadims(2,nvar)=loc_dim('depth_1',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Atlantic Meridional Streamfucntion'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='Sv'
c
      nvar=nvar+1
      varname(nvar)='opsi_p'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('latitude_2',dimname,nall)
      vadims(2,nvar)=loc_dim('depth_1',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Pacific Meridional Streamfucntion'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='Sv'
c
      nvar=nvar+1
      varname(nvar)='temp'
      vdims(nvar)=4
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('depth',dimname,nall)
      vadims(4,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Temperature'
      attvarname(1,2,nvar)='units'
c AY (07/10/04) : hacked out to return centigrade temperatures
c     attvarname(2,2,nvar)='K'
      attvarname(2,2,nvar)='C'
c
      nvar=nvar+1
      varname(nvar)='salinity'
      vdims(nvar)=4
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('depth',dimname,nall)
      vadims(4,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Salinity'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='PSU'
c
      nvar=nvar+1
      varname(nvar)='density'
      vdims(nvar)=4
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('depth',dimname,nall)
      vadims(4,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Density'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='ks/m3'
c
      nvar=nvar+1
      varname(nvar)='uvel'
      vdims(nvar)=4
      vadims(1,nvar)=loc_dim('longitude_1',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude_1',dimname,nall)
      vadims(3,nvar)=loc_dim('depth',dimname,nall)
      vadims(4,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Ocean Eastward Current'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='m/s'
c
      nvar=nvar+1
      varname(nvar)='vvel'
      vdims(nvar)=4
      vadims(1,nvar)=loc_dim('longitude_2',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude_2',dimname,nall)
      vadims(3,nvar)=loc_dim('depth',dimname,nall)
      vadims(4,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Ocean Northward Current'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='m/s'
c
      nvar=nvar+1
      varname(nvar)='wvel'
      vdims(nvar)=4
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('depth_1',dimname,nall)
      vadims(4,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Vertical Current'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='m/s'
c
      nvar=nvar+1
      varname(nvar)='latent'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Latent heat flux'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='W/m2'
c
      nvar=nvar+1
      varname(nvar)='sensible'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Sensible heat flux'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='W/m2'
c
      nvar=nvar+1
      varname(nvar)='netsolar'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Net solar heat flux'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='W/m2'
c
      nvar=nvar+1
      varname(nvar)='netlong'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Net longwave heat flux'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='W/m2'
c
      nvar=nvar+1
      varname(nvar)='sic_heat'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Sea-ice heat flux'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='W/m2'
c
      nvar=nvar+1
      varname(nvar)='evap'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Evaporation'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='mm/s'
c
      nvar=nvar+1
      varname(nvar)='pptn'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Precipitation'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='mm/s'
c
      nvar=nvar+1
      varname(nvar)='runoff'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Runoff'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='mm/s'
c
      nvar=nvar+1
      varname(nvar)='sic_fw'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Sea-ice freshwater flux'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='mm/s'
c
      nvar=nvar+1
      varname(nvar)='bathymetry'
      vdims(nvar)=2
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Ocean depth'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='m'
c
      return
      end

* ======================================================================
* write_netcdf
* ======================================================================
*
* AY (19/03/04)
* Writes data to netCDF file

      subroutine write_netcdf_ocn(imax,jmax,kmax,k1,depth1,
     :                        opsi,opsia,opsip,
     :                        ts,u,rho,
     :                        fx0flux, fwflux,
     :                        work,
     :                        dsc,usc,rsc,saln0,
     :                        maxi,maxj,maxk,maxl,imode)
      implicit none 
      include 'netcdf.cmn'
      integer imax,jmax,kmax,maxi,maxj,maxk,maxl
      integer imode,k1(0:maxi+1,0:maxj+1)
      real depth1(maxk+1)
      real opsi(0:maxj,0:maxk),opsia(0:maxj,0:maxk),
     :     opsip(0:maxj,0:maxk),
     :     ts(maxl,0:maxi+1,0:maxj+1,0:maxk+1),
     :     rho(0:maxi+1,0:maxj+1,0:maxk),
     :     fx0flux(5,maxi,maxj),fwflux(4,maxi,maxj),
     :     dsc,usc,rsc
      real u(3,0:maxi,0:maxj,maxk)
      real saln0
      real work((maxi+1)*(maxj+1)*(maxk+1))
c
      integer i
      integer j,k
c
      do k=1,kmax+1
         do j=1,jmax+1
            do i=1,imax+1
               work(i + (j-1)*(imax+1) + (k-1)*(jmax+1)*(imax+1)) = 0.0
            end do
         end do
      end do
c
c     Global streamfunction
      call prep_netcdf_ocn(opsi,0,jmax,kmax,work,k1,depth1,
     :                  dsc*usc*rsc*1e-6,1,1,1)
      call writevar(nco(imode),idvaro(1,imode),work)
c
c     Atlantic streamfunction
      call prep_netcdf_ocn(opsia,0,jmax,kmax,work,k1,depth1,
     :                  dsc*usc*rsc*1e-6,1,1,1)
      call writevar(nco(imode),idvaro(2,imode),work)
c
c     Pacific streamfunction
      call prep_netcdf_ocn(opsip,0,jmax,kmax,work,k1,depth1,
     :                  dsc*usc*rsc*1e-6,1,1,1)
      call writevar(nco(imode),idvaro(3,imode),work)
c
c     Temperature (i.e. final argument = 1)
      call prep_netcdf_ocn(ts,imax,jmax,kmax,work,k1,depth1,
     :                  1.0,2,maxl,1)
c AY (07/10/04) : hacked out to return centigrade temperatures
c     Correct temperature to Kelvin
c     do i=1,((maxi+1)*(maxj+1)*(maxk+1))
c        if (work(i).gt.-99999.0) work(i) = work(i) + 273.15
c     enddo
      call writevar(nco(imode),idvaro(4,imode),work)
c
c     Salinity (i.e. final argument = 2)
      call prep_netcdf_ocn(ts,imax,jmax,kmax,work,k1,depth1,
     :                  1.0,2,maxl,2)
c     Correct salinity to PSU
      do i=1,((maxi+1)*(maxj+1)*(maxk+1))
         if (work(i).gt.-99999.0) work(i) = work(i) + 
     :                            real(saln0)
      enddo
      call writevar(nco(imode),idvaro(5,imode),work)
c
c     Density
      call prep_netcdf_ocn(rho,imax,jmax,kmax,work,k1,depth1,
     :                  1.0,2,1,1)      
      call writevar(nco(imode),idvaro(6,imode),work)
c 
c     Ocean velocity (component 1)
      call prep_netcdf_ocn(u,imax,jmax,kmax,work,k1,depth1,
     :                  usc,3,3,1)
      call writevar(nco(imode),idvaro(7,imode),work)
c
c     Ocean velocity (component 2)
c     AY (18/03/04) : There are potential problems with this output 
c     because of necessary changes to its preparation to stop it 
c     causing a bounds error run failure)
      call prep_netcdf_ocn(u,imax,jmax,kmax,work,k1,depth1,
     :                  usc,4,3,2)
      call writevar(nco(imode),idvaro(8,imode),work)
c
c     Ocean velocity (component 3)
c     AY (18/03/04) : There are potential problems with this output 
c     because of necessary changes to its preparation to stop it 
c     causing a bounds error run failure)
ccc      call prep_netcdf_ocn(u,imax,jmax,kmax,work,k1,depth1,
ccc     :                  usc*dsc/rsc,5,3,3)
      call prep_netcdf_ocn_w(imax,jmax,kmax,u,work,k1,
     :                        usc*dsc/rsc)
      call writevar(nco(imode),idvaro(9,imode),work)
c
c     Latent heat flux
      call prep_netcdf_ocn(fx0flux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,5,4)
      call writevar(nco(imode),idvaro(10,imode),work)
c
c     Sensible heat flux
      call prep_netcdf_ocn(fx0flux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,5,2)
      call writevar(nco(imode),idvaro(11,imode),work)
c
c     Net solar heat flux
      call prep_netcdf_ocn(fx0flux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,5,1)
      call writevar(nco(imode),idvaro(12,imode),work)
c
c     Net longwave heat flux
      call prep_netcdf_ocn(fx0flux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,5,3)
      call writevar(nco(imode),idvaro(13,imode),work)
c      
c     Sea-ice heat flux
      call prep_netcdf_ocn(fx0flux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,5,5)
      call writevar(nco(imode),idvaro(14,imode),work)
c
c     Evaporation
      call prep_netcdf_ocn(fwflux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,4,2)
      call writevar(nco(imode),idvaro(15,imode),work)
c
c     Precipitation
      call prep_netcdf_ocn(fwflux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,4,1)
      call writevar(nco(imode),idvaro(16,imode),work)
c
c     Runoff
      call prep_netcdf_ocn(fwflux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,4,3)
      call writevar(nco(imode),idvaro(17,imode),work)
c
c     Sea-ice freshwater flux
      call prep_netcdf_ocn(fwflux,imax,jmax,0,work,k1,depth1,
     :                 1.0,8,4,4)
      call writevar(nco(imode),idvaro(18,imode),work)
c
c     Ocean bathymetry
      call prep_netcdf_ocn(fwflux,imax,jmax,kmax,work,k1,depth1,
     :                 1.0,7,4,1)
      call writevar(nco(imode),idvaro(19,imode),work)
c
      return
      end

* ======================================================================
* prep_netcdf_ocn
* ======================================================================
*
* AY (19/03/04)
* Reorganises data for netCDF file (e.g. re-orientation)

      subroutine prep_netcdf_ocn(data_i,mg,jgg,nl,data_o,iland,depth1,
     :                        scale,itype,ilev,it)
      implicit none
c
      integer mg,jgg,nl,itype,it,ilev,iland(*)
c      integer i,j,k
      real scale
      real data_i(*)
      real data_o(*), depth1(nl+1)
c    
      if (itype.eq.1) then
         call flip_vert(data_i,data_o,jgg,nl,scale)
      else if (itype.eq.2) then
         call flip_both1(data_i,data_o,mg,jgg,nl,iland,scale,ilev,it)
      else if (itype.eq.3) then
         call flip_both2(data_i,data_o,mg,jgg,nl,mg,jgg,nl,
     :                   iland,scale,ilev,it,1)
      else if (itype.eq.4) then
c AY (18/03/04) : Values of two arguments (jgg+1 and 0 below) force
c                 a loop in the called subroutine to exceed the bounds
c                 of the output array.  This has been unsatisfactorily
c                 solved by setting the last argument to 1.
c        call flip_both2(data_i,data_o,mg,jgg,nl,mg,jgg+1,nl,
c    :                   iland,scale,ilev,it,0)
         call flip_both2(data_i,data_o,mg,jgg,nl,mg,jgg+1,nl,
     :                   iland,scale,ilev,it,1)
      else if (itype.eq.5) then
c AY (18/03/04) : Values of one argument (nl+1 below) forces a loop in
c                 the called subroutine to exceed the bounds of the 
c                 input and output arrays.  This has been unsatisfactorily
c                 solved by setting the argument to nl.
c        call flip_both2(data_i,data_o,mg,jgg,nl,mg,jgg,nl+1,
c    :                   iland,scale,ilev,it,1)
         call flip_both2(data_i,data_o,mg,jgg,nl,mg,jgg,nl,
     :                   iland,scale,ilev,it,1)
      else if (itype.eq.6) then
c AY (19/03/04) : a simple new routine for two-dimensional fields
         call twodee_tracer(data_i,data_o,mg,jgg,mg,jgg,iland,scale)
      else if (itype.eq.7) then
c AY (23/03/04) : a simple routine for ocean bathymetry
c AY (12/07/05) : removed excess argument
         call twodee_bathy(data_o,depth1,mg,jgg,nl,mg,jgg,
     :                     iland)
      else if (itype.eq.8) then
c AY (22/10/04) : a simple new routine for two-dimensional tracers
c                 organised into three-dimensional arrays
         call twodee_tracer2(data_i,data_o,mg,jgg,mg,jgg,iland,scale,
     :        ilev,it)
      end if
c
      return
      end


      subroutine prep_netcdf_ocn_w(imax,jmax,kmax,data_i,data_o,iland,
     :                        scale)
      implicit none
c     
      integer::imax,jmax,kmax
      real data_i(3,0:imax,0:jmax,1:kmax)
      real data_o((imax+1)*(jmax+1)*(kmax+1))
      integer iland(0:imax+1,0:jmax+1)
      real scale
c
      integer i,j,k
c
      do k=1,kmax
         do j=1,jmax
            do i=1,imax
               if (iland(i,j).ge.90.or.iland(i,j).gt.(kmax+1-k)) then
                  data_o(i + (j-1)*imax + (k-1)*jmax*imax) = 
     :                 -99999.0
               else
                  data_o(i + (j-1)*imax + (k-1)*jmax*imax) = 
     :                 real(scale*data_i(3,i,j,kmax+1-k))
               end if
            end do
         end do
      end do
      k = kmax+1
      do j=1,jmax
         do i=1,imax
            data_o(i + (j-1)*imax + (k-1)*jmax*imax) = 
     :           -99999.0
         end do
      end do
c
      return
      end


* ======================================================================
* flip_vert
* ======================================================================
*
* AY (19/03/04)
* Flips 2D arrays so that low k values are shallower

      subroutine flip_vert(data_in,data_out,iy,iz,scale)
      implicit none
c
      integer iy,iz
      real scale
      real data_in(0:iy,0:iz)
      real data_out(0:iy,0:iz)
c         
      integer j,k
c 
      do k=0,iz
         do j=0,iy 
            data_out(j,iz-k)=real(scale*data_in(j,k))
         end do
      end do
c
      return
      end 

* ======================================================================
* flip_both1
* ======================================================================
*
* AY (19/03/04)
* Flips 3D arrays so that low k values are shallower (tracer arrays)

      subroutine flip_both1(temper,temp1,imax,jmax,kmax,
     :                     iland,scale,iter,it)
c
      implicit none
c
      integer imax,jmax,kmax,it,iter
      integer iland(0:imax+1,0:jmax+1)
      real scale,temper(iter,0:imax+1,0:jmax+1,0:kmax+1)
      real temp1(imax,jmax,kmax)
c
      integer i,j,k
c      integer ioff1, ioff2
c
c AY (22/03/04) : ioff1 and ioff2 offset the GOLDSTEIN grid to try
c                 (unsuccessfully as it happens) to start the netCDF
c                 output at longitude 0E.  The code below is modified
c                 to remove this transformation
c      ioff1=imax/4+1
c      ioff2=imax-ioff1
c
      do k=1,kmax
         do j=1,jmax
            do i=1,imax
               if (iland(i,j).ge.90.or.iland(i,j).gt.k) then
                  temp1(i,j,kmax+1-k)=-99999.0
               else
                  temp1(i,j,kmax+1-k)=real(scale*temper(it,i,j,k))
               endif
            enddo
         end do
      end do
c
      return
      end

* ======================================================================
* flip_both2
* ======================================================================
*
* AY (19/03/04)
* Flips 3D arrays so that low k values are shallower (velocity arrays)

      subroutine flip_both2(temper,temp1,
     :                     imax,jmax,kmax,
     :                     ix,iy,iz,  
     :                     iland,scale,iter,it,idom)
c
      implicit none
c
      integer imax,jmax,kmax,it,ix,iy,iz,idom,iter
      integer iland(0:imax+1,0:jmax+1)
      real scale,temper(iter,0:imax,0:jmax,kmax)
      real temp1(ix,iy,iz)
c
      integer i,j,k,jj
c      integer ioff1, ioff2
c
c AY (22/03/04) : ioff1 and ioff2 offset the GOLDSTEIN grid to try
c                 (unsuccessfully as it happens) to start the netCDF
c                 output at longitude 0E.  The code below is modified
c                 to remove this transformation
c      ioff1=imax/4+1
c      ioff2=imax-ioff1
c 
c AY (18/03/04) : The code below appears to contain an error which
c                 allows the loop to reference parts of array temp
c                 outside of its bounds.  The error manifests itself
c                 when iy = jmax+1 and idom = 0.  This allows jj to
c                 take a value of iy+1.
c     
c                 A similar error occurs when iz = kmax+1, since this
c                 references k=0 regions of the temp1 array and k=9
c                 regions of the temper array.  Neither of which
c                 exist.
c
      do k=1,iz
         jj=0
         do j=idom,iy
            jj=jj+1
            do i=1,imax
               if (iland(i,j).ge.90.or.iland(i,j).gt.k) then
                  temp1(i,jj,kmax+1-k)=-99999.0
               else
                  temp1(i,jj,kmax+1-k)=real(scale*temper(it,i,j,k))
               endif
            enddo
         end do
      end do
c
      if (it.eq.3) then
         do j=1,jmax
            do i=1,imax
               temp1(i,j,iz)=0.0
            end do
         end do
      end if
c
      return

      end

* ======================================================================
* twodee_tracer
* ======================================================================
*
* AY (19/03/04)
* Organises a two-dimensional array that's on the tracer grid

      subroutine twodee_tracer(temper, temp1,
     :                         imax,jmax,
     :                         ix,iy,
     :                         iland,scale)
c
      implicit none
c
      integer imax,jmax,ix,iy
      integer iland(0:imax+1,0:jmax+1)
      real    temper(imax,jmax),scale
      real  temp1(ix,iy)
c
      integer i,j
c
      do j=1,jmax
         do i=1,imax
            if (iland(i,j).ge.90) then
               temp1(i,j)=-99999.0
            else
               temp1(i,j)=real(scale*temper(i,j))
            endif
         enddo
      enddo
c
      return
      end

* ======================================================================
* twodee_tracer2
* ======================================================================
*
* AY (22/10/04)
* Organises a two-dimensional array that's on the tracer grid

      subroutine twodee_tracer2(temper, temp1,
     :                         imax,jmax,
     :                         ix,iy,
     :                         iland,scale,iter,it)
      implicit none
c
      integer imax,jmax,ix,iy,iter,it
      integer iland(0:imax+1,0:jmax+1)
      real    temper(iter,imax,jmax),scale
      real temp1(ix,iy)
c
      integer i,j
c
      do j=1,jmax
         do i=1,imax
            if (iland(i,j).ge.90) then
               temp1(i,j)=-99999.0
            else
               temp1(i,j)=real(scale*temper(it,i,j))
            endif
         enddo
      enddo
c
      return
      end

* ======================================================================
* twodee_bathy
* ======================================================================
*
* AY (22/03/04)
* Organises a two-dimensional bathymetry array on the tracer grid

      subroutine twodee_bathy(temp1, depth1,
     :                         imax,jmax,kmax,
     :                         ix,iy,
     :                         iland)
c
      implicit none
c
      integer imax,jmax,kmax,ix,iy
      integer iland(0:imax+1,0:jmax+1)
      real  temp1(ix,iy),depth1(kmax+1)
c
      integer i,j,here_dep
c
      do j=1,jmax
         do i=1,imax
            if (iland(i,j).ge.90) then
               temp1(i,j)=-99999.0
            else
               here_dep=kmax - iland(i,j) + 2
               if (here_dep.lt.1) print*,'bathymetry too shallow'
               if (here_dep.gt.kmax+1) print*,'bathymetry too deep'
               temp1(i,j)=depth1(here_dep)
            endif
         enddo
      enddo
c
      return
      end

* ======================================================================
* end_netcdf_ocn
* ======================================================================
*
* AY (19/03/04)
* Ends netCDF-writing process and closes netCDF file

      SUBROUTINE END_NETCDF_OCN(imode)

      implicit none
      include 'netcdf.cmn'
c
      integer imode
c
      print*,' calling end_netcdf_ocn for file = ',imode
      call closenc(nco(imode))
      print*,' called end_netcdf_ocn for file = ',imode
c
      return
      end

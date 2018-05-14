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
* - new preparation routines for two dimensional fields
* - ordering of the routines in this file altered to reflect
*   call order
*
* Note : missing data flag should be -99999, but appears as another
* number in netCDF files.  As yet this discrepancy has not been
* tracked down and corrected.
*
* AY (23/09/04) : upgraded to handle sea-ice albedo

* ======================================================================
* ini_netcdf_sic
* ======================================================================
*
* AY (19/03/04)
* Begins netCDF initialisation process

      SUBROUTINE INI_NETCDF_SIC(istep,imode)
c
#include "seaice.cmn"
      include 'netcdf_grid.cmn' 
c
      integer istep,imode
c
c AY (17/03/04) : alon1, etc. excised to ocean.cmn (renamed nclon1, etc.)
c
      real day,rtime
c AY (17/03/04) : extra declarations
      integer iyear, imonth
c
c AY (17/03/04) : alon1, etc. now calculated in initialise_ocean.F
c
      day=istep*dtsic*tsc/86400.0
c AY (17/03/04) : 365.25 days per GOLDSTEIN year (not 360.0)
c AY (23/03/04) : small constant added to day calculation for round-off
c                 reasons (i.e. 365.2499 vs. 365.25)
      iyear=int((day + 0.001)/yearlen)
      imonth=int((day-iyear*yearlen)/(yearlen/12.))+1
c AY (24/03/04) : rtime added for netCDF file time
      rtime=iyear + ((day-iyear*yearlen))/yearlen
      if (debug_loop) 
     & print*,'istep',istep,'day',day,'iyear',iyear,'imonth',imonth,
     :     'rtime',rtime
c AY (23/03/04) : leaving year increment as is - though I don't like it
c     iyear=iyear+2000
c
c AY (19/03/04) : lout added to argument list to add identifier to
c                 netCDF filenames
      call ini_netcdf_sic1(outdir_name,lenout,lout,
     :                    imonth,rtime,
     :                    nclon1,nclat1,
     :                    nclon2,nclat2,nclon3,nclat3,
     :                    imax,jmax,imode)
c
      return
      end

* ======================================================================
* ini_netcdf_sic1
* ======================================================================
*
* AY (19/03/04)
* Continues netCDF initialisation process

      SUBROUTINE INI_NETCDF_SIC1(dir_name,ilen,runid,
     :                          imonth,rtime,
     :                          alon1,alat1,
     :                          alon2,alat2,alon3,alat3,
     :                          mg,jgg,imode)
c
      implicit none
      include 'netcdf_sic.cmn'
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
      integer mg,jgg
      real 
     :     alon1(mg),alon2(mg),alon3(mg),
     :     alat1(jgg),alat2(jgg),alat3(jgg)
      real rtime
      integer imonth,imode
c
      integer imax,jmax,kmax,lmax
      parameter(imax=100,jmax=100,kmax=100,lmax=10000)
      real
     :     xcoord(imax),ycoord(jmax),tcoord(lmax)
      integer i,j,itime,ifname1,lnsig
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
      call setup_nc_sic(mg,jgg,itime,
     :                  nmaxdims,nall,
     :                  ndim,nvar,natts,nattsvar,vdims,
     :                  vadims,ndims,
     :                  dimname,varname,attdimname,
     :                  attvarname)
c
ccc write(cyear,'(i10.10)')iyear
c AY (19/03/04) : run identifier added to netCDF file names
      if (imode.eq.1) then
         fname1=dir_name(1:ilen)//'gsic_'//runid(1:3)//'_rs_'
     :        //cyear//'_'//cmon(imonth)//'.nc'
      else if (imode.eq.2) then
         fname1=dir_name(1:ilen)//'gsic_'//runid(1:3)//'_av_'
     :        //cyear//'_'//cmon(imonth)//'.nc'
      end if
c
      ifname1=lnsig(fname1)
ccc print*,' Opening ',fname1(1:ifname1)
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
c AY (24/03/04) : alat3 is only jgg items long
c     do j=1,jgg+1
      do j=1,jgg
         ycoord(j)=alat3(j)
      end do
      call writedim(nco(imode),iddimo(6,imode),ycoord)
c
c     Time
      do i=1,1
c AY (24/03/04) : rtime used as time coordinate
         tcoord(i)=real(rtime)
      end do
      call writedim(nco(imode),iddimo(7,imode),tcoord)
c
      return
      end

* ======================================================================
* setup_nc_sic
* ======================================================================
*
* AY (19/03/04)
* Sets up netCDF file's array names, units, descriptions, etc.

      subroutine setup_nc_sic(nlon,nlat,ntime,
     :                 nmaxdims,nall,
     :                 ndim,nvar,natts,nattsvar,vdims,
     :                 vadims,ndims,
     :                 dimname,varname,attdimname,
     :                 attvarname)
      implicit none
      integer nlon,nlat,ntime,nmaxdims,nall
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
      ndims(ndim)=nlat
      natts(ndim)=2
      attdimname(1,1,ndim)='long_name'
      attdimname(2,1,ndim)='latitude'
      attdimname(1,2,ndim)='units'
      attdimname(2,2,ndim)='degrees_north'
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
      varname(nvar)='sic_height'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Sea-ice height'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='m'
c
      nvar=nvar+1
      varname(nvar)='sic_cover'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Sea-ice fractional cover'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='dimensionless'
c
      nvar=nvar+1
      varname(nvar)='sic_temp'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Sea-ice temperature'
      attvarname(1,2,nvar)='units'
c AY (07/10/04) : hacked out to return centigrade temperatures
c     attvarname(2,2,nvar)='K'
      attvarname(2,2,nvar)='C'
c
      nvar=nvar+1
      varname(nvar)='sic_albedo'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Sea-ice albedo'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='dimensionless'
c
      nvar=nvar+1
      varname(nvar)='delta_height'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Change in sea-ice height'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='m/s'
c
      nvar=nvar+1
      varname(nvar)='delta_cover'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Change in sea-ice fractional cover'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='dimensionless'
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
      return
      end

* ======================================================================
* write_netcdf
* ======================================================================
*
* AY (19/03/04)
* Writes data to netCDF file

      subroutine write_netcdf_sic(imax,jmax,k1,
     :                        varice,tice,albice,
     :                        dtha,fx0_sic,fw_sic,
     :                        work,
     :                        maxi,maxj,imode)
      implicit none 
      include 'netcdf_sic.cmn'

      integer imax,jmax,maxi,maxj,imode,k1(0:maxi+1,0:maxj+1)
      real varice(2,maxi,maxj),tice(maxi,maxj),albice(maxi,maxj),
     :     dtha(2,maxi,maxj),
     :     fx0_sic(maxi,maxj),fw_sic(maxi,maxj)
      real work((maxi+1)*(maxj+1))
c
c     Sea-ice height (i.e. final argument = 1)
      call prep_netcdf_sic(varice,imax,jmax,work,k1,
     :                  1.0,7,2,1)
      call writevar(nco(imode),idvaro(1,imode),work)
c
c     Sea-ice area (i.e. final argument = 2)
      call prep_netcdf_sic(varice,imax,jmax,work,k1,
     :                  1.0,7,2,2)
      call writevar(nco(imode),idvaro(2,imode),work)
c
c     Sea-ice temperature
      call prep_netcdf_sic(tice,imax,jmax,work,k1,
     :                 1.0,6,1,1)
c AY (07/10/04) : hacked out to return centigrade temperatures
c     Correct temperature to Kelvin
c     do i=1,((maxi+1)*(maxj+1))
c        if (work(i).gt.-99999.0) work(i) = work(i) + 273.15
c     enddo
      call writevar(nco(imode),idvaro(3,imode),work)
c
c     Sea-ice albedo
      call prep_netcdf_sic(albice,imax,jmax,work,k1,
     :                 1.0,6,1,1)
      call writevar(nco(imode),idvaro(4,imode),work)
c
c     Change in sea-ice height
      call prep_netcdf_sic(dtha,imax,jmax,work,k1,
     :                 1.0,7,2,1)
      call writevar(nco(imode),idvaro(5,imode),work)
c
c     Change in sea-ice fractional cover
      call prep_netcdf_sic(dtha,imax,jmax,work,k1,
     :                 1.0,7,2,2)
      call writevar(nco(imode),idvaro(6,imode),work)
c      
c     Sea-ice heat flux
      call prep_netcdf_sic(fx0_sic,imax,jmax,work,k1,
     :                 1.0,6,1,1)
      call writevar(nco(imode),idvaro(7,imode),work)
c
c     Sea-ice freshwater flux
      call prep_netcdf_sic(fw_sic,imax,jmax,work,k1,
     :                 1.0,6,1,1)
      call writevar(nco(imode),idvaro(8,imode),work)
c
      return
      end

* ======================================================================
* prep_netcdf_sic
* ======================================================================
*
* AY (19/03/04)
* Reorganises data for netCDF file (e.g. re-orientation)

      subroutine prep_netcdf_sic(data_i,mg,jgg,data_o,iland,
     :                        scale,itype,ilev,it)
      implicit none
c
      integer mg,jgg,itype,it,ilev,iland(*)
      real scale
      real data_i(*)
      real data_o(*)
c
      if (itype.eq.6) then
c AY (19/03/04) : a simple new routine for two-dimensional fields
         call twodee_sic(data_i,data_o,mg,jgg,mg,jgg,iland,scale)
      else if (itype.eq.7) then
c AY (22/03/04) : a simple new routine for two-dimensional tracers
         call twodee_sic2(data_i,data_o,mg,jgg,mg,jgg,iland,scale,
     :        ilev,it)
      end if
c
      return
      end

* ======================================================================
* twodee_sic
* ======================================================================
*
* AY (19/03/04)
* Organises a two-dimensional array that's on the tracer grid

      subroutine twodee_sic(temper, temp1,
     :                         imax,jmax,
     :                         ix,iy,
     :                         iland,scale)
      implicit none
c
      integer imax,jmax,ix,iy
      integer iland(0:imax+1,0:jmax+1)
      real    temper(imax,jmax),scale
      real temp1(ix,iy)
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
* twodee_sic2
* ======================================================================
*
* AY (22/03/04)
* Organises a two-dimensional array that's on the tracer grid

      subroutine twodee_sic2(temper, temp1,
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
* end_netcdf_sic
* ======================================================================
*
* AY (19/03/04)
* Ends netCDF-writing process and closes netCDF file

      SUBROUTINE END_NETCDF_SIC(imode)

      implicit none
      include 'netcdf_sic.cmn'
c
      integer imode
c
ccc print*,' calling end_netcdf_sic for file = ',imode
      call closenc(nco(imode))
ccc print*,' called end_netcdf_sic for file = ',imode
c
      return
      end

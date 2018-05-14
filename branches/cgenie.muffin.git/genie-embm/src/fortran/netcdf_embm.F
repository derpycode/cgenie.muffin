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
* tracked down and corrected

* ======================================================================
* ini_netcdf_embm
* ======================================================================
*
* AY (19/03/04)
* Begins netCDF initialisation process

      SUBROUTINE INI_NETCDF_EMBM(istep,imode)
c
#include "embm.cmn"
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
      day=istep*dtatm*tsc/86400.0
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
c AY (23/03/04) : removing year standardisation to 2000
c     iyear=iyear+2000
c
c AY (19/03/04) : lout added to argument list to add identifier to
c                 netCDF filenames
      call ini_netcdf_embm1(outdir_name,lenout,lout,
     :                    imonth,rtime,
     :                    nclon1,nclat1,
     :                    nclon2,nclat2,nclon3,nclat3,
     :                    imax,jmax,imode)
c
      return
      end

* ======================================================================
* ini_netcdf_embm1
* ======================================================================
*
* AY (19/03/04)
* Continues netCDF initialisation process

      SUBROUTINE INI_NETCDF_EMBM1(dir_name,ilen,runid,
     :                          imonth,rtime,
     :                          alon1,alat1,
     :                          alon2,alat2,alon3,alat3,
     :                          mg,jgg,imode)
c
      implicit none
      include 'netcdf_embm.cmn'
c
      integer nvar,ndim
      integer natts(nall), nattsvar(nall)
      integer vdims(nall), ndims(nall)
      integer vadims(nmaxdims,nall)
      character dimname(nall)*200
      character attdimname(2,nmaxdims,nall)*200
      character attvarname(2,nmaxdims,nall)*200
      character varname(nall)*200
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
c      real zcoord(kmax)
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
      call setup_nc_embm(mg,jgg,itime,
     :                  nmaxdims,nall,
     :                  ndim,nvar,natts,nattsvar,vdims,
     :                  vadims,ndims,
     :                  dimname,varname,attdimname,
     :                  attvarname)
c
ccc write(cyear,'(i10.10)')iyear
c AY (19/03/04) : run identifier added to netCDF file names
      if (imode.eq.1) then
         fname1=dir_name(1:ilen)//'embm_'//runid(1:3)//'_rs_'
     :        //cyear//'_'//cmon(imonth)//'.nc'
      else if (imode.eq.2) then
         fname1=dir_name(1:ilen)//'embm_'//runid(1:3)//'_av_'
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
* setup_nc_embm
* ======================================================================
*
* AY (19/03/04)
* Sets up netCDF file's array names, units, descriptions, etc.

      subroutine setup_nc_embm(nlon,nlat,ntime,
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
      varname(nvar)='air_temp'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Air temperature'
      attvarname(1,2,nvar)='units'
c AY (07/10/04) : hacked out to return centigrade temperatures
c     attvarname(2,2,nvar)='K'
      attvarname(2,2,nvar)='C'
c
      nvar=nvar+1
      varname(nvar)='humidity'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Specific humidity'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='g/kg'
c
c AP (03/08/06) : addition of specific humidity after precipitation
c
      nvar=nvar+1
      varname(nvar)='dry_air_humidity'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Specific humidity after precipitation'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='g/kg'
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
      varname(nvar)='landmask'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Land mask'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='dimensionless'
c
      nvar=nvar+1
      varname(nvar)='dry_air_relative_humidity'
      vdims(nvar)=3
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)=
     :   'Relative humidity after precipitation'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='1'
c
      return
      end

* ======================================================================
* write_netcdf
* ======================================================================
*
* AY (19/03/04)
* Writes data to netCDF file
*
* AY (12/07/05) : removed surplus input arguments to subroutine
*
* AP (03/08/06) : addition of specific humidity after precipitation

      subroutine write_netcdf_embm(imax,jmax,k1,
     :                        tq,
c qdry
     :                        q_pa, rq_pa,
     :                        fx0flux,fwflux,
     :                        work,
     :                        maxi,maxj,imode)
      implicit none 
      include 'netcdf_embm.cmn'

      integer imax,jmax,maxi,maxj,imode,k1(0:maxi+1,0:maxj+1)
      real tq(2,maxi,maxj),
c qdry(maxi,maxj)
     &     q_pa(maxi,maxj), rq_pa(maxi,maxj),
     :     fx0flux(4,maxi,maxj),fwflux(2,maxi,maxj)
      real work((maxi+1)*(maxj+1))
c
c AY (12/07/05) : calls to prep_netcdf_embm have had surplus argument
c                 removed
c
c     Temperature (i.e. final argument = 1)
      call prep_netcdf_embm(tq,imax,jmax,work,k1,
     :                  1.0,7,2,1)
c AY (07/10/04) : hacked out to return centigrade temperatures
c     Correct temperature to Kelvin
c     do i=1,((maxi+1)*(maxj+1))
c        if (work(i).gt.-99999.0) work(i) = work(i) + 273.15
c     enddo
      call writevar(nco(imode),idvaro(1,imode),work)
c
c     Specific humidity (i.e. final argument = 2)
c     (note : the 1000.0 below converts kg/kg to g/kg)
      call prep_netcdf_embm(tq,imax,jmax,work,k1,
     :                  1000.0,7,2,2)
      call writevar(nco(imode),idvaro(2,imode),work)
c
c AP (03/08/06) : Specific humidity after precipitation
c     (note : the 1000.0 below converts kg/kg to g/kg)
c     (note : last two arguments are not used for itype=6)
      call prep_netcdf_embm(
c qdry,
     &     q_pa,imax,jmax,work,k1,
     :                  1000.0,6,99,99)
      call writevar(nco(imode),idvaro(3,imode),work)
c
c     Latent heat flux
      call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,
     :                 1.0,7,4,4)
      call writevar(nco(imode),idvaro(4,imode),work)
c
c     Sensible heat flux
      call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,
     :                 1.0,7,4,2)
      call writevar(nco(imode),idvaro(5,imode),work)
c
c     Net solar heat flux
      call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,
     :                 1.0,7,4,1)
      call writevar(nco(imode),idvaro(6,imode),work)
c
c     Net longwave heat flux
      call prep_netcdf_embm(fx0flux,imax,jmax,work,k1,
     :                 1.0,7,4,3)
      call writevar(nco(imode),idvaro(7,imode),work)
c
c     Evaporation
      call prep_netcdf_embm(fwflux,imax,jmax,work,k1,
     :                 1.0,7,2,2)
      call writevar(nco(imode),idvaro(8,imode),work)
c
c     Precipitation
      call prep_netcdf_embm(fwflux,imax,jmax,work,k1,
     :                 1.0,7,2,1)
      call writevar(nco(imode),idvaro(9,imode),work)
c
c     Land mask
      call prep_netcdf_embm(fwflux,imax,jmax,work,k1,
     :                 1.0,8,2,1)
      call writevar(nco(imode),idvaro(10,imode),work)
c
c Precipitation-adjusted relative humidity
      call prep_netcdf_embm(rq_pa,imax,jmax,work,k1,
     :                  1.0,6,99,99)
      call writevar(nco(imode),idvaro(11,imode),work)
c
      return
      end

* ======================================================================
* prep_netcdf_embm
* ======================================================================
*
* AY (19/03/04)
* Reorganises data for netCDF file (e.g. re-orientation)
*
* AY (12/07/05) : removed surplus input arguments to subroutine

      subroutine prep_netcdf_embm(data_i,mg,jgg,data_o,iland,
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
c AY (12/07/05) : removed surplus input argument to subroutine
         call twodee_embm(data_i,data_o,mg,jgg,mg,jgg,scale)
      else if (itype.eq.7) then
c AY (22/03/04) : a simple new routine for two-dimensional tracers
c AY (12/07/05) : removed surplus input argument to subroutine
         call twodee_embm2(data_i,data_o,mg,jgg,mg,jgg,scale,
     :        ilev,it)
      else if (itype.eq.8) then
c AY (22/03/04) : a simple new routine to produce a land mask field
c AY (12/07/05) : removed surplus input arguments to subroutine
         call twodee_embm3(data_o,mg,jgg,mg,jgg,iland)
      end if
c
      return
      end

* ======================================================================
* twodee_embm
* ======================================================================
*
* AY (19/03/04)
* Organises a two-dimensional array that's on the tracer grid
*
* AY (12/07/05) : removed surplus input argument to subroutine

      subroutine twodee_embm(temper, temp1,
     :                         imax,jmax,
     :                         ix,iy,
     :                         scale)
      implicit none
c
      integer imax,jmax,ix,iy
      real    temper(imax,jmax),scale
      real temp1(ix,iy)
c
      integer i,j
c
      do j=1,jmax
         do i=1,imax
            temp1(i,j)=real(scale*temper(i,j))
         enddo
      enddo
c
      return
      end

* ======================================================================
* twodee_embm2
* ======================================================================
*
* AY (22/03/04)
* Organises a two-dimensional array that's on the tracer grid
*
* AY (12/07/05) : removed surplus input argument to subroutine

      subroutine twodee_embm2(temper, temp1,
     :                         imax,jmax,
     :                         ix,iy,
     :                         scale,iter,it)
      implicit none
c
      integer imax,jmax,ix,iy,iter,it
      real    temper(iter,imax,jmax),scale
      real temp1(ix,iy)
c
      integer i,j
c
      do j=1,jmax
         do i=1,imax
            temp1(i,j)=real(scale*temper(it,i,j))
         enddo
      enddo
c
      return
      end

* ======================================================================
* twodee_embm3
* ======================================================================
*
* AY (22/03/04)
* Organises a two-dimensional land mask array that's on the tracer grid
*
* AY (12/07/05) : removed surplus input arguments to subroutine

      subroutine twodee_embm3(temp1,
     :                        imax,jmax,
     :                        ix,iy,
     :                        iland)
      implicit none
c
      integer imax,jmax,ix,iy
      integer iland(0:imax+1,0:jmax+1)
      real temp1(ix,iy)
c
      integer i,j
c
      do j=1,jmax
         do i=1,imax
            if (iland(i,j).ge.90) then
               temp1(i,j)=-99999.0
            else
               temp1(i,j)=0.0
            endif
         enddo
      enddo
c
      return
      end

* ======================================================================
* end_netcdf_embm
* ======================================================================
*
* AY (19/03/04)
* Ends netCDF-writing process and closes netCDF file

      SUBROUTINE END_NETCDF_EMBM(imode)

      implicit none
      include 'netcdf_embm.cmn'
c
      integer imode
c
ccc print*,' calling end_netcdf_embm for file = ',imode
      call closenc(nco(imode))
ccc print*,' called end_netcdf_embm for file = ',imode
c
      return
      end

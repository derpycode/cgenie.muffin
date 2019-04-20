* ======================================================================
* initialise_embm.F
* ======================================================================
*
* subroutine gseta, sets up  atmosphere and sea-ice
* introduced 11/2/02 (Bob)
*
* AY (03/12/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
* 
*                 this code takes pieces from mains.F, gseta.F
*	          and gseto.F
*
* AY (11/12/03) : sea-ice and surflux code excised
*
* RM (13/5/05) : edited for variable sin(lat) resolution (from NRE, 6/12/04)
*                ldeg=true/false switches degree/sin lat grid
*                igrid=0,1,2... switches between grids
*
* AR (14/10/14) : altered input format for orbits

      subroutine initialise_embm(alon1,alat1,alon2,alat2,
     :     alon3,alat3,
     :     aboxedge1_lon,aboxedge1_lat,
     :     aboxedge2_lon,aboxedge2_lat,
     :     aboxedge3_lon,aboxedge3_lat,
     :     ilandmask1,ilandmask2,ilandmask3,
     :     ias,iaf,ips,ipf,jsf,
     :     tstar_ocn,
     :     totsteps,
     :     co2_out,ch4_out,n2o_out,
     :     stressxu_atm,stressyu_atm,
     :     stressxv_atm,stressyv_atm,
     :     tstar_atm,qstar_atm,atmos_dt_tim,
     :     solconst,
     :     eb_rmax,eb_dphi,eb_rdtdim,eb_ca,
     :     gn_daysperyear,
     :     torog_atm,                        !< surface air temperature adjusted for orography
     :     surf_orog_atm,                    !< orography (m)
     :     landice_slicemask_lic,            !< land ice sheet mask
     :     syr,                              !< seconds per year
     :     flag_ents,                        !< flag for ENTS functionality
     :     lowestlu2_atm,                    !< zonal component of wind speed on u grid
     :     lowestlv3_atm
     :     )

      use genie_util, ONLY: check_unit, check_iostat

#include "embm.cmn"
      include 'netcdf_grid.cmn'

      REAL,INTENT(out) :: atmos_dt_tim ! EMBM timestep (s)
c ======================================================================
c Declarations
c ======================================================================

c AY (04/12/03) : these declarations necessary for coupling grids
c      integer ilon1_atm, ilat1_atm
c AY (04/03/04) : third grid interpolation position (= v point)
c                 added to incoming array list
      real
     :     alon1(maxi),alat1(maxj),
     :     alon2(maxi),alat2(maxj),
     :     alon3(maxi),alat3(maxj),
     :     aboxedge1_lon(maxi+1),aboxedge1_lat(maxj+1),
     :     aboxedge2_lon(maxi+1),aboxedge2_lat(maxj+1),
     :     aboxedge3_lon(maxi+1),aboxedge3_lat(maxj+1)
      real
     :     tstar_ocn(maxi,maxj),
     :     co2_out(maxi,maxj),ch4_out(maxi,maxj),n2o_out(maxi,maxj)
      real
     :     stressxu_atm(maxi,maxj),
     :     stressyu_atm(maxi,maxj),
     :     stressxv_atm(maxi,maxj),
     :     stressyv_atm(maxi,maxj),
     :     tstar_atm(maxi,maxj),
     :     qstar_atm(maxi,maxj)

c AY (05/10/04) : no longer needed, commented out
crma extra explicit declarations (rma, 22/12/03)
c     real haavg(maxl,maxi,maxj)

      integer ilandmask1(maxi,maxj),ilandmask2(maxi,maxj),
     :        ilandmask3(maxi,maxj)
      integer totsteps

crma extra explicit declarations (rma, 22/12/03)
      integer ias(maxj),iaf(maxj),ips(maxj),ipf(maxj),jsf
      integer bmask(maxi,maxj)

      real z1, tv, tv1, tv2, tv3, tv4, tv5, tatm, relh0_ocean,
     +     relh0_land, sigma, diffamp(2), diffwid, difflin,
     +     diffend, zro(maxk), zw(0:maxk)
      real radfor_scl_co2,radfor_pc_co2_rise
      real radfor_scl_ch4,radfor_pc_ch4_rise
      real radfor_scl_n2o,radfor_pc_n2o_rise
      real u_tau_ice,ch_ice,cpo_ice
crma      real  asurf(maxj)

crma extra explicit declarations (rma, 22/12/03)
      real th0, th1, s0, s1, phix, scl_fwf

crma extras for general grid (rma, 2/6/05)
      real theta,thv,dth,dscon
      real deg_to_rad
      real area_atl1a, area_atl1b, area_atl1c
      real area_pac1a, area_pac1b, area_pac1c

      integer i, j, k, l, m, natl1a, npac1a, natl1b, npac1b
      integer natl1c, npac1c
      integer n

crma extra explicit declarations (rma, 22/12/03)
      integer lnsig1

      character(len=8) world
      integer          lenworld
c AY (02/02/05) : now read in from GOIN information
c     parameter(world='worbe2')

c AY (02/02/05) : add filename variables for wind stress and wind speed
c                 files
      character xu_wstress*127, yu_wstress*127
      character xv_wstress*127, yv_wstress*127
      integer   len_xu, len_yu, len_xv, len_yv

      character u_wspeed*127, v_wspeed*127
      integer   len_uws, len_vws

c AY (09/01/04) : local variables reinstated
      character ans, lin*13

c AY (05/10/04) : for netCDF restarts (following Dan)
      character netin*1,netout*1,ascout*1

c AJR value of solar constant to be passed to radfor
      real solconst
      
c SG > Dummy variables to be passed to/from ENTS via genie.F    
      real eb_rmax
      real eb_dphi
      real eb_rdtdim
      real eb_ca(maxi,maxj)
c      real eb_co2(maxi,maxj)
      real gn_daysperyear
c SG <      

      real,intent(out)::torog_atm(maxi,maxj)
      real,intent(out)::surf_orog_atm(maxi,maxj)

c land ice sheet mask
      real,dimension(maxi,maxj),
     &     intent(out)::landice_slicemask_lic

      real::syr

c ENTS functionality
      logical flag_ents
      real deltq

      real qsat

c     file access checks
      integer :: ios
      logical :: ioex

cAJR Antarctic atmospheric diffusivity fudge
cAJR diffusivity scaling factor
      real diffa_scl
cAJR grid point distance over which scalar is applied (j direction)
      integer diffa_len
cAJR planetary albedo modifications
      real albedop_offs
      real albedop_amp
      real albedop_skew
      integer albedop_skewp
      real albedop_mod2
      real albedop_mod4
      real albedop_mod6
cAJR planetary albedo modifications: local
      real albedop_scl
cAJR filename for zonal planetary albedo
      character par_albedo1d_name*127
cAJR filename for 2D planetary albedo
      character par_albedo2d_name*127
cAJR zonal albedo array
      real albz(maxj)

c seasonal fields
      real uatml1(2,maxi,maxj,nmth+1)
      real usurfl1(maxi,maxj,nmth+1)
      real tncep1(maxi,maxj,nmth+1)
      real pncep1(maxi,maxj,nmth+1)
      real rhncep1(maxi,maxj,nmth+1)
      real atm_alb1(maxi,maxj,nmth+1)

c precipitation timescale and land radiation
      real timepptn
      real hld,cld

c southern boundaries of regions for FW flux adjustment
      integer j1as,j1bs,j1cs

c zonal and meridional wind speed components
      real,dimension(maxi,maxj),intent(inout)::lowestlu2_atm,
     &     lowestlv3_atm

c this flag indicates if external wind forcing from 'genie-wind' module is active
      logical flag_wind

c declare a namelist to store EMBM's initialisation vars
      NAMELIST /ini_embm_nml/indir_name,outdir_name
      NAMELIST /ini_embm_nml/rstdir_name
      NAMELIST /ini_embm_nml/igrid,world
      NAMELIST /ini_embm_nml/xu_wstress,yu_wstress,xv_wstress
      NAMELIST /ini_embm_nml/yv_wstress,u_wspeed,v_wspeed
      NAMELIST /ini_embm_nml/npstp,iwstp,itstp,ianav,ans
      NAMELIST /ini_embm_nml/yearlen,nyear,ndta,scf
      NAMELIST /ini_embm_nml/diffamp,diffwid,difflin
      NAMELIST /ini_embm_nml/betaz,betam
      NAMELIST /ini_embm_nml/radfor_scl_co2,radfor_pc_co2_rise
      NAMELIST /ini_embm_nml/radfor_scl_ch4,radfor_pc_ch4_rise
      NAMELIST /ini_embm_nml/radfor_scl_n2o,radfor_pc_n2o_rise
      NAMELIST /ini_embm_nml/tatm,relh0_ocean,relh0_land
      NAMELIST /ini_embm_nml/extra1a,extra1b,extra1c,scl_fwf
      NAMELIST /ini_embm_nml/z1_embm,tdatafile,qdatafile
      NAMELIST /ini_embm_nml/tdata_varname,qdata_varname
      NAMELIST /ini_embm_nml/tdata_missing,qdata_missing
      NAMELIST /ini_embm_nml/tdata_scaling,qdata_scaling
      NAMELIST /ini_embm_nml/tdata_offset,qdata_offset
      NAMELIST /ini_embm_nml/qdata_rhum
      NAMELIST /ini_embm_nml/tqinterp
      NAMELIST /ini_embm_nml/lout,netin,netout,ascout
      NAMELIST /ini_embm_nml/rmax
      NAMELIST /ini_embm_nml/filenetin,dirnetout
      NAMELIST /ini_embm_nml/lin,atchem_radfor
      NAMELIST /ini_embm_nml/orbit_radfor
      NAMELIST /ini_embm_nml/t_orbit,norbit,orbitsteps,filenameorbit
      NAMELIST /ini_embm_nml/t_co2,nco2,co2steps,filenameco2
      NAMELIST /ini_embm_nml/diffa_scl,diffa_len
      NAMELIST /ini_embm_nml/dosc,delf2x
      NAMELIST /ini_embm_nml/olr_adj0,olr_adj,t_eqm
      NAMELIST /ini_embm_nml/aerofac,volfac,solfac
      NAMELIST /ini_embm_nml/useforc,forcname
      NAMELIST /ini_embm_nml/albedop_offs,albedop_amp
      NAMELIST /ini_embm_nml/albedop_skew,albedop_skewp
      NAMELIST /ini_embm_nml/albedop_mod2,albedop_mod4,albedop_mod6
      NAMELIST /ini_embm_nml/lapse_rate
      NAMELIST /ini_embm_nml/orogswitch
      NAMELIST /ini_embm_nml/t_orog
      NAMELIST /ini_embm_nml/filenameorog
      NAMELIST /ini_embm_nml/norog
      NAMELIST /ini_embm_nml/orogsteps
      NAMELIST /ini_embm_nml/t_lice
      NAMELIST /ini_embm_nml/filenamelice
      NAMELIST /ini_embm_nml/nlice
      NAMELIST /ini_embm_nml/licesteps
      NAMELIST /ini_embm_nml/lice_k9
      NAMELIST /ini_embm_nml/t_d18o
      NAMELIST /ini_embm_nml/nd18o
      NAMELIST /ini_embm_nml/d18osteps
      NAMELIST /ini_embm_nml/d18o_k,scale_mwfx
      NAMELIST /ini_embm_nml/filenamed18o
      NAMELIST /ini_embm_nml/filenamed18oicethresh
      NAMELIST /ini_embm_nml/filenamed18oorogmin
      NAMELIST /ini_embm_nml/filenamed18ooroggrad
      NAMELIST /ini_embm_nml/ents_seasonswitch
      NAMELIST /ini_embm_nml/ents_offlineswitch
      NAMELIST /ini_embm_nml/timepptn
      NAMELIST /ini_embm_nml/par_runoff_scheme
      NAMELIST /ini_embm_nml/par_runoff_b
      NAMELIST /ini_embm_nml/par_runoff_tau
      NAMELIST /ini_embm_nml/debug_init,debug_end,debug_loop
      NAMELIST /ini_embm_nml/par_wind_polar_avg
      NAMELIST /ini_embm_nml/unify_winds
      NAMELIST /ini_embm_nml/par_sich_max
      NAMELIST /ini_embm_nml/par_albsic_min,par_albsic_max
      NAMELIST /ini_embm_nml/opt_freeze
      NAMELIST /ini_embm_nml/par_albedo1d_name,par_albedo2d_name
      NAMELIST /ini_embm_nml/par_orbit_osce,par_orbit_oscsob
      NAMELIST /ini_embm_nml/par_orbit_oscgam,par_orbit_osctau0

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      area_atl1a = 0.0
      area_atl1b = 0.0
      area_atl1c = 0.0
      area_pac1a = 0.0
      area_pac1b = 0.0
      area_pac1c = 0.0
      j1as = 0
      j1bs = 0
      j1cs = 0
      len_xu = 0
      len_yu = 0
      len_xv = 0
      len_yv = 0
      len_uws = 0
      len_vws = 0
      flag_wind=.false.
c ------------------------------------------------------------ c

c ======================================================================
c Setting up EMBM
c ======================================================================

      print*,'======================================================='
      print*,' >>> Initialising EMBM atmosphere module ...'

      if (debug_init) print*

cccc-DJL 21/9/05 (for reading IGCM grid), inserted by RMA
ccc      dx_igcm=360.0/real(nx_igcm)
ccc      do i=1,nx_igcm
ccc         igcm_lons(i)=real((i-1.0)*dx_igcm)
ccc         igcm_lons_edge(i)=real((i-1.5)*dx_igcm)
ccc      end do
ccc      igcm_lons_edge(nx_igcm+1)=real((nx_igcm-0.5)*dx_igcm)
ccc      call gwtcnr(igcm_lats,ny_igcm/2)
ccc      call gwtbox(igcm_lats_edge,ny_igcm/2)
ccc
ccc      if (debug_init) print*,'These are the igcm latitude edges:'
ccc      if (debug_init) print*,igcm_lats_edge
ccc      if (debug_init) print*,'These are the igcm longitude edges:'
ccc      if (debug_init) print*,igcm_lons_edge

c AY (01/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------

c read DATA (i.e. namelist) file
      call check_unit(56,__LINE__,__FILE__)
      open(unit=56,file='data_EMBM',status='old',iostat=ios)
      if (ios /= 0) then
         print*,'ERROR: could not open EMBM namelist file'
         print*,"ERROR on line ", __LINE__, " in file ", __FILE__
         stop
      end if

c read in namelist
      read(UNIT=56,NML=ini_embm_nml,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read EMBM namelist'
         print*,"ERROR on line ", __LINE__, " in file ", __FILE__
         stop
      else
         close(56,iostat=ios)
         if (ios /= 0) then
            print*,'ERROR: could not close EMBM namelist file'
            print*,"ERROR on line ", __LINE__, " in file ", __FILE__
            stop
         end if
      end if

c      stop
      
c     Input directory name
      lenin = lnsig1(indir_name)
      if (indir_name(lenin:lenin).ne.'/') then
c        This 'if' checks for a terminal slash symbol
         lenin = lenin + 1
         indir_name(lenin:lenin) = '/'
      end if
      if (debug_init) print*,'Input dir. name ',indir_name(1:lenin)

c     Output directory name
      lenout = lnsig1(outdir_name)
      if (outdir_name(lenout:lenout).ne.'/') then
c        This 'if' checks for a terminal slash symbol
         lenout = lenout + 1
         outdir_name(lenout:lenout+1) = '/'
      end if
      if (debug_init) print*,'Output dir. name ',outdir_name(1:lenout)

c     Restart (input) directory name
      lenrst = lnsig1(rstdir_name)
      if (rstdir_name(lenrst:lenrst).ne.'/') then
c        This 'if' checks for a terminal slash symbol
         lenrst = lenrst + 1
         rstdir_name(lenrst:lenrst+1) = '/'
      end if
      if (debug_init) print*,'Restart dir. name ',rstdir_name(1:lenrst)

crma (15/09/05) : read in grid type
crma (0 = const dsinlat; 1 = const dlat; 2 = IGCM)
crma      if(igrid.eq.0) ldeg=.false.
crma      if(igrid.eq.1) ldeg=.true.
c AY (02/02/05) : read in topography filename
      lenworld = lnsig1(world)
      if (debug_init) print*,'Topography name ',world(1:lenworld)

c AY (02/02/05) : read in wind stress filenames
      if (.not.flag_wind) then
      len_xu = lnsig1(xu_wstress)
      len_yu = lnsig1(yu_wstress)
      len_xv = lnsig1(xv_wstress)
      len_yv = lnsig1(yv_wstress)
      if (debug_init) print*,
     & 'x windstress at u point ',xu_wstress(1:len_xu)
      if (debug_init) print*,
     & 'y windstress at u point ',yu_wstress(1:len_yu)
      if (debug_init) print*,
     & 'x windstress at v point ',xv_wstress(1:len_xv)
      if (debug_init) print*, 
     & 'y windstress at v point ',yv_wstress(1:len_yv)
      endif

c AY (02/02/05) : read in wind speed filenames
      if (.not.flag_wind) then
      len_uws = lnsig1(u_wspeed)
      len_vws = lnsig1(v_wspeed)
      if (debug_init) print*,'u wind speed ',u_wspeed(1:len_uws)
      if (debug_init) print*,'v wind speed ',v_wspeed(1:len_vws)
      endif

c AY (20/03/17) : read in albedo filenames
      if (debug_init) print*,
     & '1D albedo file ',trim(par_albedo1d_name)
      if (debug_init) print*,
     & '2D albedo file ',trim(par_albedo2d_name)

c     Time-steps information
c AY (08/12/03) : have total steps passed into initialise_ocean.F
      nsteps = totsteps
      if (debug_init) print*,'npstp iwstp itstp ianav'
      if (debug_init) print*,npstp,iwstp,itstp,ianav
c     New or continuing run
      if (debug_init) print*,'new or continuing run ?'
      if (debug_init) print*,ans

c AY (05/05/04) : number of days per EMBM year (usually 365.25)
c     yearlen = 365.25
      if (debug_init) print*,'number of days per EMBM year'
      if (debug_init) print*,yearlen

c AR (18/01/08) : choice of whether seasonality enabled
      if (debug_init) print*,'seasonality enabled =',dosc

c AY (01/12/03) : previously in gseto.F ...
c ----------------------------------------------------------------------

      pi = 4*atan(1.0)

c dimensional scale values

      usc = 0.05
      rsc = 6.37e6
      dsc = 5e3
      fsc = 2*7.2921e-5
      gsc = 9.81
      rh0sc = 1e3
      rhosc = rh0sc*fsc*usc*rsc/gsc/dsc
      cpsc = 3981.1
      tsc = rsc/usc

c EMBM scaling for heat forcing of ocean
      rfluxsc = rsc/(dsc*usc*rh0sc*cpsc)
c EMBM reference salinity
      saln0 = 34.9
c EMBM scaling for freshwater forcing of ocean
      rpmesco = rsc*saln0/(dsc*usc)

c parameters for setting up grid
c th is latitude, coords are sin(th), longitude phi, and z

      th0 = - pi/2    
      th1 = pi/2 
c     th0 = - pi*7/18
c     th1 = pi*7/18
      s0 = sin(th0)    
      s1 = sin(th1)     
c     phix = pi/3
      phix = 2*pi
      deg_to_rad = pi/180.

c grid dimensions must be no greater than array dimensions in var.cmn

      imax = maxi
      jmax = maxj 
      kmax = maxk   
      lmax = 2

      dphi = phix/imax
      if (igrid.lt.2) then
         phi0 = -260.0*deg_to_rad
      else
ccc         phi0 = igcm_lons_edge(1)*deg_to_rad
      endif
crma      ds = (s1-s0)/jmax
crma      dphi2 = dphi*2
crma      ds2 = ds*2
      rdphi = 1.0/dphi
crma      rds = 1.0/ds

c set up horizontal grid: sin and cos factors at rho and v points (c grid)
c fix for global domain although only cv and cv2 are referred to at or beyond
c limits 24/6/2 if no flow out of N + S boundaries.

      sv(0) = s0
      cv(0) = cos(th0)
crma      if(ldeg)then
      if(igrid.eq.1)then
crma set up const dlat grid
         dth = (th1 - th0)/jmax
         do j=1,jmax
            thv = th0 + j*dth
            theta = thv - 0.5*dth
            sv(j) = sin(thv)
            s(j) = sin(theta)
            cv(j) = cos(thv)
         enddo
      elseif(igrid.eq.0)then
crma set up const dsinlat grid
         dscon = (s1-s0)/jmax
         do j=1,jmax
            sv(j) = s0 + j*dscon
            cv(j) = sqrt(1 - sv(j)*sv(j))
            s(j) = sv(j) - 0.5*dscon
         enddo
ccc      elseif(igrid.eq.2)then
ccccrma set up IGCM grid (21/9/05)
ccccrma note igcm orders latitude north-to-south!
ccc         do j=1,jmax
ccc            thv = deg_to_rad*igcm_lats_edge(jmax+1-j)
ccc            dth = deg_to_rad*(igcm_lats_edge(jmax+2-j)
ccc     1                      - igcm_lats_edge(jmax+1-j))
ccc            theta = thv - 0.5*dth
ccc            sv(j) = sin(thv)
ccc            s(j) = sin(theta)
ccc            cv(j) = cos(thv)
ccc         enddo
      endif
      if (debug_init) print*,'EMBM latitudes: velocity; tracers'
      if (debug_init) print*,'j, 180/pi*asin(sv(j)), 180/pi*asin(s(j))'
      do j=1,jmax
         ds(j) = sv(j) - sv(j-1)
         rds(j) = 1.0/ds(j)
         c(j) = sqrt(1 - s(j)*s(j))
         rc(j) = 1.0/c(j)
         rc2(j) = rc(j)*rc(j)*rdphi
         if(j.lt.jmax)then
            dsv(j) = s(j+1) - s(j)
            rdsv(j) = 1.0/dsv(j)
            rcv(j) = 1.0/cv(j)
            cv2(j) = cv(j)*cv(j)*rdsv(j)
            if(j.gt.1)rds2(j) = 2.0/(dsv(j)+dsv(j-1))
         endif
        if (debug_init) print*,j,180/pi*asin(sv(j)),180/pi*asin(s(j))
c     &,180/pi*asin(ds(j)),sv(j),s(j),ds(j)
      enddo

c AY (29/11/04) : area of grid cell (assumes sine(lat) grid)
      do j=1,jmax
      asurf(j) = rsc*rsc*ds(j)*dphi
      if (debug_init) print*,
     & 'j = ',j,'EMBM grid cell area is',asurf(j),'m2'
      enddo

c v2 seasonality
      if (debug_init) print*,'timesteps per year and A/O dt ratio'
      if (debug_init) print*,nyear,ndta
      if(nyear.gt.maxnyr)stop 'embm : nyear > maxnyr' 
      tv = 86400.0*yearlen/(nyear*tsc)
      ryear = 1.0/(yearlen*86400)

      dtatm = tv/ndta
      if (debug_init) print*, 'embm timestep (s) =',dtatm*tsc
      atmos_dt_tim = REAL(dtatm*tsc)

c AY (13/01/04) : oops!  left this out
c variable timestep option not recommended
      do k=1,kmax
         dt(k) = tv  
c initialize
c        dzu(1,k) = 0
c        dzu(2,k) = 0
      enddo
      if (debug_init) print*,'dimensional ocean timestep',tv*tsc/86400
      if (debug_init) print*,'dimensionless O/A timesteps',tv,dtatm

      rdtdim = 1.0/(tsc*dt(kmax))
      if (debug_init) print*,'rdtdim = ',rdtdim

c AY (17/12/03) : reinstated after erroneous deletion

c set up grid
c For variable (exponential) dz use ez0 > 0, else use ez0 < 0

      ez0 = 0.1
c     ez0 = - 1.0
      z1 = ez0*((1.0 + 1/ez0)**(1.0/kmax) - 1.0)
      if (debug_init)print*,'z1',z1
      tv4 = ez0*((z1/ez0+1)**0.5-1)
      tv2 = 0
      tv1 = 0
      zro(kmax) = -tv4
      zw(kmax) = tv2
      do k=1,kmax
         if(ez0.gt.0)then
            tv3 = ez0*((z1/ez0+1)**k-1)
            dz(kmax-k+1) = tv3 - tv2
            tv2 = tv3
            tv5 = ez0*((z1/ez0+1)**(k+0.5)-1)
            if(k.lt.kmax)dza(kmax-k) = tv5 - tv4
            tv4 = tv5
            tv1 = tv1 + dz(kmax-k+1)
c tv3 is the depth of the kth w level from the top
c tv5 is the depth of the k+1th density level from the top
         else
            dz(k) = real(1d0/kmax)
            dza(k) = real(1d0/kmax)
         endif
      enddo

      do k=kmax,1,-1
         if(k.gt.1)zro(k-1) = zro(k) - dza(k-1)
         zw(k-1) = zw(k) - dz(k)
      enddo
      if (debug_init)
     & write(6,'(i4,3e12.4)')(k,dsc*zw(k),dsc*zro(k),dsc*dz(k)
     1        ,k=kmax,1,-1)
         
      if (debug_init) print*,'dzz'
      dzz = dz(kmax)*dza(kmax-1)/2
      if (debug_init) print*,dzz

c efficiency array

      do k=1,kmax-1
         rdz(k) = 1.0/dz(k)
         rdza(k) = 1.0/dza(k)
      enddo
      rdz(kmax) = 1.0/dz(kmax)

c dza(kmax) never referenced, set to 0 for Andy's biogeo-code

      dza(kmax) = 0.

c set up sin and cos factors at rho and v points (c grid)
c fix for global domain although only cv and cv2 are referred to at or beyond
c limits 24/6/2 if no flow out of N + S boundaries.

crma      do j=0,jmax
crma         sv(j) = s0 + j*ds
crma         if(abs(1.0 - abs(sv(j))).lt.1e-12)then
crma            cv(j) = 0.
crma            rcv(j) = 1e12
crma         else
crma            cv(j) = sqrt(1 - sv(j)*sv(j))
crma            rcv(j) = 1.0/cv(j)
crma         endif
crma         cv2(j) = cv(j)*cv(j)*rds
crma         s(j) = sv(j) - 0.5*ds
crma         if(s(j).lt.-1.0)s(j) = -2.0 - s(j)
crma         c(j) = sqrt(1 - s(j)*s(j))
crma         rc(j) = 1.0/c(j)
crma         rc2(j) = rc(j)*rc(j)*rdphi
crma      enddo   

c AY (16/01/04) : read in scf from input file (this should have the
c                 same value used by GOLDSTEIN)
      if (debug_init) print*,'scf'
      if (debug_init) print*,scf

c AY (11/12/03) : note to self - these wind fields refer to wind
c	          stress, and their values need to be passed out
c	          to surflux, sea-ice and, of course, goldstein.

c define forcing
c
c read wind data

      if (.not.flag_wind) then
c taux,tauy at u-points
      call check_unit(96,__LINE__,__FILE__)
      open(96,file=indir_name(1:lenin)//xu_wstress(1:len_xu),iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
         
      call check_unit(97,__LINE__,__FILE__)
      open(97,file=indir_name(1:lenin)//yu_wstress(1:len_yu),iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
         
c taux,tauy at v-points
      call check_unit(98,__LINE__,__FILE__)
      open(98,file=indir_name(1:lenin)//xv_wstress(1:len_xv),iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      call check_unit(99,__LINE__,__FILE__)
      open(99,file=indir_name(1:lenin)//yv_wstress(1:len_yv),iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      
      do j=1,jmax
         do i=1,imax
c rotate grid to check b.c.s
c           do idum=1,imax
c              i=1+mod(idum+18-1,36)
               
c AY (19/12/03) : Variables dztau and dztav are renamed here to
c                 separate the unscaled (hence 'us') versions read
c	          in from files, from those scaled versions used
c	          in surflux and GOLDSTEIN.

            read(96,*,iostat=ios)us_dztau(1,i,j)
            call check_iostat(ios,__LINE__,__FILE__)

            read(97,*,iostat=ios)us_dztau(2,i,j)
            call check_iostat(ios,__LINE__,__FILE__)

            read(98,*,iostat=ios)us_dztav(1,i,j)
            call check_iostat(ios,__LINE__,__FILE__)

            read(99,*,iostat=ios)us_dztav(2,i,j)
            call check_iostat(ios,__LINE__,__FILE__)

c AY (11/12/03) : not clear yet where best to do this scaling

c AY (04/12/03) : following scaling excised - may be done instead in
c                 other modules, e.g. goldstein.F
c multiply by scaling factor scf (to drive correct gyre strengths)
c
c           dztau(1,i,j) = scf*dztau(1,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c           dztau(2,i,j) = scf*dztau(2,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c           dztav(1,i,j) = scf*dztav(1,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c           dztav(2,i,j) = scf*dztav(2,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c
c           tau(1,i,j) = dztau(1,i,j)*dzz
c           tau(2,i,j) = dztav(2,i,j)*dzz
         enddo
      enddo

      close(96,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      close(97,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      close(98,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      close(99,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      else
         do j=1,jmax
            do i=1,imax
               us_dztau(1,i,j) = 0.0
               us_dztau(2,i,j) = 0.0
               us_dztav(1,i,j) = 0.0
               us_dztav(2,i,j) = 0.0
            enddo
         enddo
      endif

crma dzz from initialise_ocean.f
c     dzz = 7.09513476982982024E-4
c     scf = 2.0
c     print*, '*** WARNING: hard-wired dzz, scf ***'

c AY (11/12/03) : neither bathymetry nor runoff required here - moved
c                 to surflux instead.

c AY (04/12/03) : reading in ocean bathymetry/runoff patterns - not
c                 sure yet if this is entirely necessary.
c seabed depth h needed BEFORE forcing if coastlines are non-trivial
c note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
c k1(1,j) - k1(imax+1,j) = 0

      ntot = 0
      intot = 0

c AY (06/01/04) : reinstate bathymetry/runoff read-in
c AY (01/12/03) : line modified to point to input directory
c     open(13,file=world//'.k1')
      if (debug_init) print*,'Land runoff being read in'
      
      call check_unit(13,__LINE__,__FILE__)
      open(13,file=indir_name(1:lenin)//world(1:lenworld)//'.k1',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
c k1(1,j) - k1(imax+1,j) = 0, as enforced below;
c
c AY (07/01/04) : write out contents of k1 array prior to filling it
c     do j=jmax+1,0,-1
c        write(6,'(i4,40i3)')j,(k1(i,j),i=0,imax+1)
c        do i=0,imax+1
c           k1(i,j) = 100
c        enddo
c     enddo

      do j=jmax+1,0,-1
         read(13,*,iostat=ios)(k1(i,j),i=0,imax+1)
         call check_iostat(ios,__LINE__,__FILE__)

c rotate grid to check b.c.s
c        read(13,*)xxx,(k1(i,j),i=19,36),(k1(i,j),i=1,18),xxx
         k1(0,j) = k1(imax,j)
         k1(imax+1,j) = k1(1,j)
c AY (07/01/04) : ocean depth variables - excised
c        do i=0,imax+1
c           do k=1,3
c              h(k,i,j) = 0
c              rh(k,i,j) = 0
c           enddo
c        enddo
         if (debug_init.and.(j.ne.0.and.j.ne.jmax+1))
     1    write(6,'(i4,32i3)')j,(k1(i,j),i=1,32)
      enddo

c read ips etc if possible

      close(13,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      if (debug_init) print*,'Land runoff successfully read in'

c AY (01/12/03) : previously in gseta.F ...
c ----------------------------------------------------------------------

c read parameters

      if (debug_init) print*,'diffamp(1),diffamp(2), diffwid, difflin'
      if (debug_init) print*,diffamp(1),diffamp(2), diffwid, difflin
c parameter beta relates vertically-averaged advective transport
c to surface advective transport

      if (debug_init) print*,'betaz(1),betam(1),betaz(2),betam(2)'
      if (debug_init) print*,betaz(1),betam(1),betaz(2),betam(2)

c AY (11/12/03) : a lot of the following needs removed to surflux
c constants used in SSW parameterization

      sigma = 5.67e-8
      emo = 0.94 * sigma
      eml = 0.94 * sigma
      ema = 0.85 * sigma
c v2 more logical to set solar constant in oscsol.f
c I can't help but notice but solconst is actually set in radfor.F ...
c     solconst = 1368.
      tfreez = 0.

c constants used in OLW parameterization

      b00 = 2.43414e2
      b10 = -3.47968e1
      b20 = 1.02790e1
      b01 = 2.60065
      b11 = -1.62064
      b21 = 6.34856e-1
      b02 = 4.40272e-3
      b12 = -2.26092e-2
      b22 = 1.12265e-2
      b03 = -2.05237e-5
      b13 = -9.67000e-5
      b23 = 5.62925e-5

cc EMBM stuff follows...

crma need to sort out extra1, etc.

crma      asurf = rsc*rsc*ds*dphi
crma      print*,'EMBM grid cell area is',asurf,'m2'

c climatological albedo (similar to Weaver et al. 2001)
c AR/080930 : modified climatological albedo for greater flexibility
c             and to fit HadCM3 predictions
c AR/170320 : added option of reading-from-file
      if (debug_init) print*,'climatological albedo, by latitude'
      if (par_albedo2d_name.ne.'NONE') then   
c 2D ABLEDO    
c          ccc NOTHING SO FAR cccccccccccccccccccccccccccccccccccccccc
      elseif (par_albedo1d_name.ne.'NONE') then    
c ZONAL (1D) ABLEDO   
c         read zonal abledo from file 
c         NOTE: count down in j value:
c               N pole is the first element in file
          open(35,file=indir_name(1:lenin)//
     &     trim(par_albedo1d_name),iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          do j=jmax,1,-1
              read(35,*) albz(j)
          enddo
          close(35,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
c populate albedo array     
          do j=1,jmax
              do i=1,imax
                  albcl(i,j) = albz(j)
              enddo
              if (debug_init) print*,j,albcl(1,j)
          enddo
      else
c ORIGINAL CODE
          do j=1,jmax
              albedop_scl = ((albedop_skew - s(j))/2.0)**albedop_skewp
              tv = asin(s(j))          
c              tv2 = 0.2 + 0.36*0.5*(1.0 - cos(2.0*tv))
              tv2 = albedop_offs + albedop_amp*0.5*(1.0 - 
     &          cos(2.0*tv) + 
     &          albedop_scl*albedop_mod2*cos(2.0*tv) +
     &          albedop_scl*albedop_mod4*cos(4.0*tv) +
     &          albedop_scl*albedop_mod6*cos(6.0*tv)
     &          )
              do i=1,imax
                  albcl(i,j) = tv2
              enddo
              if (debug_init) print*,j,albcl(1,j)
           enddo
       endif

c atmospheric SSW absorption coefficient, value over land purely diagnostic

      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax)then
               ca(i,j)=0.3
            else
               ca(i,j)=1.0
            endif
         enddo
      enddo

c     read some scalings
      if (debug_init) print*,'radfor_scl_co2 =',radfor_scl_co2
      if (debug_init) print*,'radfor_pc_co2_rise =',radfor_pc_co2_rise
      if (debug_init) print*,'radfor_scl_ch4 =',radfor_scl_ch4
      if (debug_init) print*,'radfor_pc_ch4_rise =',radfor_pc_ch4_rise
      if (debug_init) print*,'radfor_scl_n2o =',radfor_scl_n2o
      if (debug_init) print*,'radfor_pc_n2o_rise =',radfor_pc_n2o_rise

c factor corresponding to radiative forcing of 4 W/m**2
c per doubling of atmospheric CO2
c NOTE: value now set via a namelist
c      delf2x = 5.77
      if (debug_init) print*,'delf2x =',delf2x
      if (debug_init) print*,
     & '=> climate sensitivity =',delf2x*log(2.0),' Wm-2'

c     Reference greenhouse gas concentrations
      co20 = 278.0e-6
      ch40 = 700.e-9
      n2o0 = 275.e-9
c     gas equation alpha values (as per IPCC [2001])
      alphach4 = 0.036
      alphan2o = 0.12
c     initialize greenhouse gas concentrations
      do j = 1,jmax
         do i = 1,imax
            co2(i,j) = radfor_scl_co2*co20
            ch4(i,j) = radfor_scl_ch4*ch40
            n2o(i,j) = radfor_scl_n2o*n2o0
            co2_out(i,j) = co2(i,j)
            ch4_out(i,j) = ch4(i,j)
            n2o_out(i,j) = n2o(i,j)
         enddo
      enddo
c     rate of increase of greenhouse gas concentrations
      rate_co2 = radfor_pc_co2_rise*0.01*tsc*dtatm*ndta*ryear
      rate_ch4 = radfor_pc_ch4_rise*0.01*tsc*dtatm*ndta*ryear
      rate_n2o = radfor_pc_n2o_rise*0.01*tsc*dtatm*ndta*ryear

c alternative method for defining time varying co2 from an input time series
c read in time varying co2
      if(t_co2.gt.0) then
        if (debug_init) print*, 'co2 defined from ',trim(filenameco2)
        if (debug_init) print*,'t_co2,nco2,co2steps',t_co2,nco2,co2steps
        open(729,file=trim(filenameco2))
        do i=1,nco2
          read(729,*) co2_vect(i)
        enddo
        close(729)
        co2(:,:)=co2_vect(1)
        co2_out(:,:)=co2_vect(1)
      endif
c more constants

      rhoair = 1.25
      rho0 = 1e3
      rhoao = rhoair/rho0
c depth scale for atmospheric thermal b.l. used by Weaver et al. (2001)
      hatmbl(1) = 8400.
      cpa = 1004.
c latent heat of vapourization (J/kg)
      hlv = 2.501e6
c latent heat of fusion of ice (J/kg)
      hlf = 3.34e5
c latent heat of sublimation (J/kg)
c     hls = 2.835e6
c for conservation of heat, require
      hls = hlv + hlf 

c scaling for heat forcing of atmosphere

      rfluxsca = rsc/(hatmbl(1)*usc*rhoair*cpa)   

c atmospheric winds
      if (.not.flag_wind) then

c AY (11/12/03) : note to self - these winds are the advective transport
c	          winds used in tstepa/tstipa

c AY (04/12/03) : file location altered to include input directory
c     open(35,file='uncep.silo')

      call check_unit(35,__LINE__,__FILE__)
      open(35,file=indir_name(1:lenin)//u_wspeed(1:len_uws),
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(35,*,iostat=ios)((uatm(1,i,j),i=1,imax),j=1,jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(35,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c     open(35,file='vncep.silo')
      open(35,file=indir_name(1:lenin)//v_wspeed(1:len_vws),iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(35,*,iostat=ios)((uatm(2,i,j),i=1,imax),j=1,jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(35,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c conditional zonal average
cLJG only when par_wind_polar_avg is 0 (default)
cLJG 1 makes the code consistent with genie-winds
cLJG 2 removes all conditional zonal averages
      if (par_wind_polar_avg.ne.1.and.par_wind_polar_avg.ne.2)  then
         if (debug_init) print*,"Averaging advective winds near poles"
         do j=1,jmax
            if(j.le.2.or.j.ge.jmax-1)then
               do l=1,2
                  tv = 0.
                  do i=1,imax
                     tv = tv + uatm(l,i,j)
                  enddo
                  tv = tv / imax
                  do i=1,imax
                     uatm(l,i,j) = tv
                  enddo
               enddo
            endif
         enddo
      endif 
      
c remove zonal average of v else fail mass conservation (may not be 
c disastrous).

      do i=1,imax
         do j=1,jmax
            uatm(1,i,j) = uatm(1,i,j)/usc
            uatm(2,i,j) = uatm(2,i,j)/usc
c           uatm(2,i,j) = uatm(2,i,j)*0.0
         enddo
         if (par_wind_polar_avg.ne.1.and.par_wind_polar_avg.ne.2) then 
            uatm(2,i,jmax) = 0.
         endif
      enddo
      else
         do j=1,jmax
            do i=1,imax
               do l=1,2
                  uatm(l,i,j) = 0.0
               enddo
            enddo
         enddo
      endif
c [endif of: if (.not.flag_wind) then]

c parameters for extra heat diffusion where pptn high

c     diffmod0 = 60e6
      diffmod0 = 0.
      ppmin = 2./(yearlen*86400.)
      ppmax = 4./(yearlen*86400.)

c nre simpler diffusivity

      diffend = exp(-(0.5*pi/diffwid)**2)
c     open(47,file='diff_ADVDIF.dat')
      if (debug_init) print*,
     & 'atm stability numbers, need to be small for explicit dt'
      do j=1,jmax
         tv = asin(s(j))
         tv2 = asin(sv(j))
c Weaver diffusivities as interpolated by Bob
c        read(47,'(4x,5e15.5)')tv3,diffa(1,1,j)
c    1   ,diffa(2,1,j),diffa(1,2,j),diffa(2,2,j)
         diffa(2,1,j) = diffamp(2)
         diffa(2,2,j) = diffamp(2)
c Weaver-type diffusivities but analytical  
c        diffa(1,1,j) = diffamp(1)*(0.02 + 0.2*(tv+0.5*pi)/pi      
c    &                + 0.8*0.5*(1 + cos(2*tv)))
c        diffa(1,2,j) = diffamp(1)*(0.02 + 0.2*(tv2+0.5*pi)/pi      
c    &                + 0.8*0.5*(1 + cos(2*tv2)))
c Weaver-type diffusivities, symmetrical 
c        diffa(1,1,j) = diffamp(1)*(0.2 + 0.8*0.5*(1 + cos(2*tv)))
c        diffa(1,2,j) = diffamp(1)*(0.2 + 0.8*0.5*(1 + cos(2*tv2)))
c Variable-width and asymmetry (or slope) thermal diffusivity
         diffa(1,1,j) = diffamp(1)*(difflin*2.0*(tv+0.5*pi)/pi
     &              + (1.0-difflin)*(exp(-(tv/diffwid)**2) - diffend)
     &              /(1.0 - diffend))
         diffa(1,2,j) = diffamp(1)*(difflin*2.0*(tv2+0.5*pi)/pi
     &              + (1.0-difflin)*(exp(-(tv2/diffwid)**2) - diffend)
     &              /(1.0 - diffend))

cAJR ADJUST ATMOSPHERIC DIFFUSIVITY OVER S. OCEAN AND ANTARCTICA
cAJR Scale meridional heat diffusion by a factor of diffa_scl
cAJR as far as diffa_len in the j direction
         if (diffa_len.lt.0) then
            if(sin(pi*real(diffa_len)/180.).gt.sv(j)) then
               diffa(1,2,j) = diffa_scl*diffa(1,2,j)
            endif
         else
            if(j.le.diffa_len) then
               diffa(1,2,j) = diffa_scl*diffa(1,2,j)
            endif
         endif

c non-dimensionalise diffusivities

         diffa(1,1,j) = diffa(1,1,j)/(rsc*usc)
         diffa(1,2,j) = diffa(1,2,j)/(rsc*usc)
         diffa(2,1,j) = diffa(2,1,j)/(rsc*usc)
         diffa(2,2,j) = diffa(2,2,j)/(rsc*usc)
c        write(47,'(i4,5e15.5)')j,asin(s(j)),diffa(1,1,j)*rsc*usc
c    1   ,diffa(2,1,j)*rsc*usc,diffa(1,2,j)*rsc*usc,diffa(2,2,j)*rsc*usc

c nre ???? TEST for const lat, very unstable diffusivities so control
c T diffusion by input params and make sure q diff is no bigger
c only if constant lat grid (i.e. allowing compatibility with 36x36 version)

crma      if(ldeg)then
      if((igrid.eq.1).or.(igrid.eq.2))then
         diffa(2,1,j) = min(diffa(2,1,j),diffa(1,1,j))
      endif
         if (debug_init.and.(j.gt.1))write(6,'(i4,4e15.5)')j,
     &       diffa(1,1,j)*dtatm*rc(j)*rc(j)*rdphi*rdphi
     &      ,diffa(2,1,j)*dtatm*rc(j)*rc(j)*rdphi*rdphi
     &      ,diffa(1,2,j-1)*dtatm*cv(j-1)*cv(j-1)*rdsv(j-1)*rds(j)
     &      ,diffa(2,2,j-1)*dtatm*cv(j-1)*cv(j-1)*rdsv(j-1)*rds(j)
      enddo
c     close(47)

c scale height for specific humidity (Peixoto and Oort 1992)
      hatmbl(2) = 1800.

c consts for saturation specific humidity (Bolton 1980)

      const1 = 3.80*1e-3
      const2 = 21.87
      const3 = 265.5
      const4 = 17.67
      const5 = 243.5

c threshold relative humidity
c pbh now a user input
c      rmax = 0.85

c scaling for P-E forcing of atmosphere

c     rpmesca = rsc*rho0/(dsc*usc*rhoair)
      rpmesca = rsc*rho0/(hatmbl(2)*usc*rhoair)

c reconstruct surface wind field for bulk turbulent transfer and
c zonally average near poles as for uatm for stability

c LG: I thik the following is useless here 
c because tau hasn't been assigned yet
c this piece of code is also in usurf.F after tau has been assigned
c     open(55,file='usurf.dat')

      cd = 0.0013

      do j=1,jmax
         tv3 = 0.
         do i=1,imax
            if(i.eq.1) then
               tv = (tau(1,i,j)+tau(1,imax,j))/2
            else
               tv = (tau(1,i,j)+tau(1,i-1,j))/2
            endif
            if(j.eq.1) then
               tv2 = tau(2,i,j)/2
            else
               tv2 = (tau(2,i,j)+tau(2,i,j-1))/2
            endif
            usurf(i,j) = sqrt((sqrt(tv**2 + tv2**2))
     1          *rh0sc*dsc*usc*fsc/(rhoair*cd*scf))
c    1          *rh0sc*dsc*usc*fsc/(rhoair*cd))
            tv3 = tv3 + usurf(i,j)
         enddo
c          LG :  added wind_polar_avg that undoes the 
c          zonal average of usurf near poles when eq 2
         if (par_wind_polar_avg.ne.2) then
            do i=1,imax
               if(j.le.2.or.j.ge.jmax-1)usurf(i,j) = tv3/imax
c           write(55,*)usurf(i,j)
            enddo
         endif
      enddo
c     close(55)


c AY (06/01/04) : sea-ice parameter definitions
c                 reinstated from sea-ice section of gseta.f
      tsic = -1.8
c constant ice conductivity (W/m/K)
      consic = 2.166
      if (debug_init) print*,
     & 'constant ice conductivity, consic =',consic
c in parameterization of heat flux at base of sea ice:
c empirical constant
      ch_ice = 0.0058
      if (debug_init) print*,
     & 'base of sea-ice empirical constant, ch_ice =',ch_ice
c skin friction velocity (m/s)
      u_tau_ice = 0.02
      if (debug_init) print*,
     & 'skin friction velocity, u_tau_ice =',u_tau_ice
c specific heat of sea water under ice at constant pressure (J/kg/K)
      cpo_ice = 4044
      if (debug_init) print*,
     & 'specific heat of seawater under ice, cpo_ice =',cpo_ice
c representative ice density (kg/m**3)
      rhoice = 913.
      if (debug_init) print*,
     & 'representative ice density, rhoice =',rhoice
c representative under-seaice water density (kg/m**3)
c     rho0sea = 1035.
c useful constant proportional to inverse timscale for surface freezing
c     rsictscsf = ch_ice*u_tau_ice*rho0sea*cpo_ice
      rsictscsf = ch_ice*u_tau_ice*rho0*cpo_ice
      if (debug_init) print*,'rsictscsf = ',rsictscsf
      rsictscsf = dsc*dz(kmax)*rho0*cpo_ice/(17.5*86400.0)
      if (debug_init) print*,'rsictscsf = ',rsictscsf
c minimum average sea-ice thickness over a grid cell
      hmin = 0.01
      if (debug_init) print*,
     & 'minimum average sea-ice thickness, hmin =',hmin
      rhmin = 1.0/hmin
c density ratios
      rhooi = rho0/rhoice 
      if (debug_init) print*,'density ratios, rhooi =',rhooi
      rhoio = rhoice/rho0 
c melting factor
      rrholf = 1.0/(rhoice*hlf)
      if (debug_init) print*,'rrholf = ',rrholf
c FW flux conversion parameters
      m2mm = 1000.
      mm2m = 1.0/(m2mm)
      if (debug_init) print*,'m to mm conversion factor, m2mm =',m2mm
      if (debug_init) print*,'mm to m conversion factor, mm2m =',mm2m

c read initial atmos state
      if (debug_init) print*,'tatm relh0_ocean relh0_land'
      if (debug_init) print*,tatm,relh0_ocean,relh0_land

c AY (19/12/03) : these lines were accidentally deleted - reinstated
c read freshwater flux perturbation data
      if (debug_init) print*,'extra1a range1b nsteps_extra1c'
      if (debug_init) print*,extra1a,extra1b,extra1c

ccc read scaling factor for extra1a, extra1b, extra1c
      if (debug_init) print*,'scl_fwf'
      if (debug_init) print*,scl_fwf

! Read the EMBM reference height
      if (debug_init) print*,'EMBM reference height, z1 (m)'
      if (debug_init) print*,z1_embm

ccc apply scl_fwf
      extra1a = scl_fwf*extra1a
      extra1b = scl_fwf*extra1b
      extra1c = scl_fwf*extra1c

ccc use extra1a, extra1b, extra1c, basins data to set up P-E adjustments

c find total no. of Pac/Atl gridboxes

c find southern boundaries of regions for FW flux adjustment
c region 1a: 'jsf+1' to 20S
c region 1b: 20S to 24N
c region 1c: 24N to 90N

c southern boundary of region 1a
      j1as = jsf+1
      tv=sin(-20.0*pi/180.0)
      tv2=sin(24.0*pi/180.0)
      do j=1,jmax
c southern boundary of region 1b
         if ((tv.ge.sv(j-1)).and.
     &        (tv.le.sv(j))) then
c at least half of box area has to be in region
            if (((sv(j)-tv)/ds(j)).ge.0.5) then
               j1bs = j
            else
               j1bs = j+1
            endif
         endif
c southern boundary of region 1c
         if ((tv2.ge.sv(j-1)).and.
     &        (tv2.le.sv(j))) then
c at least half of box area has to be in region
            if (((sv(j)-tv2)/ds(j)).ge.0.5) then
               j1cs = j
            else
               j1cs = j+1
            endif
         endif
      enddo

      if(igrid.eq.0) then

c in south Atlantic (to 20 deg S)
      npac1a = 0
      natl1a = 0
      do j=j1as,(j1bs-1)
         npac1a = npac1a + ipf(j) - ips(j) + 1
         natl1a = natl1a + iaf(j) - ias(j) + 1
      enddo

c in tropical Atlantic (20 deg S to 24 deg N)
      npac1b = 0
      natl1b = 0
      do j=j1bs,(j1cs-1)
         npac1b = npac1b + ipf(j) - ips(j) + 1
         natl1b = natl1b + iaf(j) - ias(j) + 1
      enddo

c in north Atlantic (north of 24 deg N) NB INCLUDES DRY POINTS
      npac1c = 0
      natl1c = 0
      do j=j1cs,jmax
         do i=ips(j),ipf(j)
            if(k1(i,j).le.kmax)npac1c = npac1c + 1
         enddo
         do i=ias(j),iaf(j)
            if(k1(i,j).le.kmax)natl1c = natl1c + 1
         enddo
      enddo

      endif

      if((igrid.eq.1).or.(igrid.eq.2))then

      area_pac1a = 0.
      area_atl1a = 0.
      do j=j1as,(j1bs-1)
         do i=ips(j),ipf(j)
            area_pac1a = area_pac1a + asurf(j)
         enddo
c conditionality to take care of "split Atlantic" (rma, 5/10/05)
         if(ias(j).gt.iaf(j)) then
            do i=ias(j),imax
               area_atl1a = area_atl1a + asurf(j)
            enddo
            do i=1,iaf(j)
               area_atl1a = area_atl1a + asurf(j)
            enddo
         else
            do i=ias(j),iaf(j)
               area_atl1a = area_atl1a + asurf(j)
            enddo
         endif
      enddo

      area_pac1b = 0.
      area_atl1b = 0.
      do j=j1bs,(j1cs-1)
         do i=ips(j),ipf(j)
               area_pac1b = area_pac1b + asurf(j)
         enddo
c conditionality to take care of "split Atlantic" (rma, 5/10/05)
         if(ias(j).gt.iaf(j)) then
            do i=ias(j),imax
               area_atl1b = area_atl1b + asurf(j)
            enddo
            do i=1,iaf(j)
               area_atl1b = area_atl1b + asurf(j)
            enddo
         else
            do i=ias(j),iaf(j)
               area_atl1b = area_atl1b + asurf(j)
            enddo
         endif
      enddo

      area_pac1c = 0.
      area_atl1c = 0.
      do j=j1cs,jmax
         do i=ips(j),ipf(j)
            if(k1(i,j).le.kmax)
     1         area_pac1c = area_pac1c + asurf(j)  
         enddo
c conditionality to take care of "split Atlantic" (rma, 5/10/05)
         if(ias(j).gt.iaf(j)) then
            do i=ias(j),imax
               if(k1(i,j).le.kmax)
     1         area_atl1c = area_atl1c + asurf(j)  
            enddo
            do i=1,iaf(j)
               if(k1(i,j).le.kmax)
     1         area_atl1c = area_atl1c + asurf(j)  
            enddo
         else
            do i=ias(j),iaf(j)
               if(k1(i,j).le.kmax)
     1         area_atl1c = area_atl1c + asurf(j)  
            enddo
         endif
      enddo

      endif

      if (debug_init) print*,
     & natl1a, npac1a, natl1b, npac1b, natl1c, npac1c 
      if (debug_init) print*,
     & 'natl1a, npac1a, natl1b, npac1b, natl1c, npac1c '

ccc increase/decrease P-E in Pacific/Atlantic as in Broecker (1991)
ccc [after Oort 1983]: net freshwater loss by Atlantic = 0.32 Sv
ccc here add/remove total extra1a, extra1b, extra1c Sv of freshwater
ccc equally by area in Pac/Atl resp.

      do j=1,jmax
         do i=1,imax
            pmeadj(i,j) = 0.
         enddo
      enddo

crma If 36x36s ...

      if(igrid.eq.0)then

      do j=j1as,(j1bs-1)
         do i=ips(j),ipf(j)
            pmeadj(i,j) = 1e6*extra1a/(npac1a*asurf(j))
         enddo
         do i=ias(j),iaf(j)
            pmeadj(i,j) = -1e6*extra1a/(natl1a*asurf(j))
         enddo
      enddo

      do j=j1bs,(j1cs-1)
         do i=ips(j),ipf(j)
            pmeadj(i,j) = 1e6*extra1b/(npac1b*asurf(j))
         enddo
         do i=ias(j),iaf(j)
            pmeadj(i,j) = -1e6*extra1b/(natl1b*asurf(j))
         enddo
      enddo

      do j=j1cs,jmax
         do i=ips(j),ipf(j)
            if(k1(i,j).le.kmax)
     1         pmeadj(i,j) = 1e6*extra1c/(npac1c*asurf(j))
         enddo
         do i=ias(j),iaf(j)
            if(k1(i,j).le.kmax)
     1         pmeadj(i,j) = -1e6*extra1c/(natl1c*asurf(j))
         enddo
      enddo

      endif

ccc If 64x32 or 64x32l ...

      if((igrid.eq.1).or.(igrid.eq.2))then

      do j=j1as,(j1bs-1)
         do i=ips(j),ipf(j)
            pmeadj(i,j) = 1e6*extra1a/area_pac1a
         enddo
c conditionality to take care of "split Atlantic" (rma, 5/10/05)
         if(ias(j).gt.iaf(j)) then
            do i=ias(j),imax
               pmeadj(i,j) = -1e6*extra1a/area_atl1a
            enddo
            do i=1,iaf(j)
               pmeadj(i,j) = -1e6*extra1a/area_atl1a
            enddo
         else
            do i=ias(j),iaf(j)
               pmeadj(i,j) = -1e6*extra1a/area_atl1a
            enddo
         endif
      enddo

      do j=j1bs,(j1cs-1)
         do i=ips(j),ipf(j)
            pmeadj(i,j) = 1e6*extra1b/area_pac1b
         enddo
c conditionality to take care of "split Atlantic" (rma, 5/10/05)
         if(ias(j).gt.iaf(j)) then
            do i=ias(j),imax
               pmeadj(i,j) = -1e6*extra1b/area_atl1b
            enddo
            do i=1,iaf(j)
               pmeadj(i,j) = -1e6*extra1b/area_atl1b
            enddo
         else
            do i=ias(j),iaf(j)
               pmeadj(i,j) = -1e6*extra1b/area_atl1b
            enddo
         endif
      enddo

      do j=j1cs,jmax
         do i=ips(j),ipf(j)
            if(k1(i,j).le.kmax)
     1         pmeadj(i,j) = 1e6*extra1c/area_pac1c
         enddo
c conditionality to take care of "split Atlantic" (rma, 5/10/05)
         if(ias(j).gt.iaf(j)) then
            do i=ias(j),imax
               if(k1(i,j).le.kmax)
     1          pmeadj(i,j) = -1e6*extra1c/area_atl1c
            enddo
            do i=1,iaf(j)
               if(k1(i,j).le.kmax)
     1          pmeadj(i,j) = -1e6*extra1c/area_atl1c
            enddo
         else
            do i=ias(j),iaf(j)
               if(k1(i,j).le.kmax)
     1          pmeadj(i,j) = -1e6*extra1c/area_atl1c
            enddo
         endif
      enddo

      endif

c *********************************************************************
c *** alternaive mask-based methodology *******************************
c *********************************************************************

c read in mask
c basin mask: 3: Atlantic
c             2: Pacific
c             1: remaining ocean grid cells
c             0: land
c sub-region 1 (a): 'jsf+1' to 20S
c sub-region 2 (b): 20S to 24N
c sub-region 3 (c): 24N to 90N

      inquire(file=indir_name(1:lenin)//world(1:lenworld)//
     &'.bmask',exist=ioex)
      if (ioex) then
         if (debug_init) print *, 'reading in basin mask file'
         call check_unit(13,__LINE__,__FILE__)
         open(13,file=indir_name(1:lenin)//world(1:lenworld)//
     &'.bmask',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         do j=jmax,1,-1
            read(13,*,iostat=ios)(bmask(i,j),i=1,imax)
            call check_iostat(ios,__LINE__,__FILE__)
            if (debug_init) write(6,'(i4,66i3)')j,(bmask(i,j),i=1,imax)
         enddo
         close(13,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

c find total no. of Pac/Atl gridboxes

      natl1a = 0
      npac1a = 0
      natl1b = 0
      npac1b = 0
      natl1c = 0
      npac1c = 0
      do j=1,jmax
         do i=1,imax
c south Pacific, Atlantic (to 20 deg S)
         if (bmask(i,j).eq.31) natl1a = natl1a + 1
         if (bmask(i,j).eq.21) npac1a = npac1a + 1
c tropical Pacific, Atlantic (20 deg S to 24 deg N)
         if (bmask(i,j).eq.32) natl1b = natl1b + 1
         if (bmask(i,j).eq.22) npac1b = npac1b + 1
c north Pacific, Atlantic (north of 24 deg N)
         if (bmask(i,j).eq.33) natl1c = natl1c + 1
         if (bmask(i,j).eq.23) npac1c = npac1c + 1
         enddo
      enddo

ccc increase/decrease P-E in Pacific/Atlantic as in Broecker (1991)
ccc [after Oort 1983]: net freshwater loss by Atlantic = 0.32 Sv
ccc here add/remove total extra1a, extra1b, extra1c Sv of freshwater
ccc equally by area in Pac/Atl resp.

      do j=1,jmax
         do i=1,imax
            pmeadj(i,j) = 0.
         enddo
      enddo
      do j=1,jmax
         do i=1,imax
         if (bmask(i,j).eq.31) 
     1         pmeadj(i,j) = -1e6*extra1a/(natl1a*asurf(j))
         if (bmask(i,j).eq.21) 
     1         pmeadj(i,j) = 1e6*extra1a/(npac1a*asurf(j))
         if (bmask(i,j).eq.32) 
     1         pmeadj(i,j) = -1e6*extra1b/(natl1b*asurf(j))
         if (bmask(i,j).eq.22) 
     1         pmeadj(i,j) = 1e6*extra1b/(npac1b*asurf(j))
         if (bmask(i,j).eq.33) 
     1         pmeadj(i,j) = -1e6*extra1c/(natl1c*asurf(j))
         if (bmask(i,j).eq.23) 
     1         pmeadj(i,j) = 1e6*extra1c/(npac1c*asurf(j))
         enddo
      enddo

      endif

c *********************************************************************

ccc initialize atmosphere

      do j=1,jmax
         do i=1,imax

c initial air temperatures

            tq(1,i,j) = tatm
            tq1(1,i,j) = tq(1,i,j)

c initial specific humidities
c set to relh0_ocean*qsat_ocean over ocean and relh0_land*qsat_atmos over land

            if(k1(i,j).le.kmax)then
               if(tstar_ocn(i,j).gt.tsic) then
                  tq(2,i,j) = relh0_ocean*const1*
     1            exp(const2*tstar_ocn(i,j)/(tstar_ocn(i,j)+const3))
               else
                  tq(2,i,j) = relh0_ocean*const1*
     1            exp(const4*tstar_ocn(i,j)/(tstar_ocn(i,j)+const5))
               endif
            else
               if(tq1(1,i,j).gt.0.0) then
                  tq(2,i,j) = relh0_land*const1*exp(const2
     1               *tq1(1,i,j)/(tq1(1,i,j)+const3))
               else
                  tq(2,i,j) = relh0_land*const1*exp(const4
     1               *tq1(1,i,j)/(tq1(1,i,j)+const5))
               endif
            endif

            tq1(2,i,j) = tq(2,i,j)

            if (dosc) then
c v2 seasonal. Annual averages

            do l=1,2
               tqavg(l,i,j) = 0.
c AY (09/01/04) : sea-ice properties excised
c              haavg(l,i,j) = 0.
            enddo
c AP (03/08/06) : precipitated atm. humidity
c variable 'qdryavg' has been superseded by 'q_pa_avg'
c            qdryavg(i,j) = 0.
c Annual average fields for diagnostic of precipitation-adjusted
c humidity (i.e., humidity after precipitation)
            q_pa_avg(i,j) = 0.
            rq_pa_avg(i,j) = 0.
            do l=1,2
               fx0avg(l,i,j)  = 0.
               fwavg(l,i,j)   = 0.
            enddo
            do l=3,4
               fx0avg(l,i,j)  = 0.
            enddo
c           ticeavg(i,j) = 0.
c SG > For ENTS
            palbavg(i,j)=0.
c SG <
            endif
         enddo
      enddo

c AY (06/01/04) : reinstate runoff catchment data
      call readroff

c AY (11/12/03) : calculated in surflux - excised from here
c diagnostic calculation of global heat source
c
c     ghs = 0.

c AY (02/02/05) : read in observational data filenames
      lentdata = lnsig1(tdatafile)
      if (debug_init) print*,'Temperature observations filename, ',
     :     tdatafile(1:lentdata)
      lenqdata = lnsig1(qdatafile)
      if (debug_init) print*,'Humidity observations filename, ',
     :     qdatafile(1:lenqdata)
      if (tqinterp) then
         print*,'Interpoate observational dataset'
         lentvar = lnsig1(tdata_varname)
         print*,'Temperature observations variable name, ',
     :        tdata_varname(1:lentvar)
         print*,'Temperature observations scaling factor, '
     $        ,tdata_scaling
         print*,'Temperature observations offset, ' ,tdata_offset
         print*,'Temperature observations missing value, '
     $        ,tdata_missing
         lenqvar = lnsig1(qdata_varname)
         print*,'Humidity observations variable name, ',
     :        qdata_varname(1:lenqvar)
         print*,'Humidity observations scaling factor, '
     $        ,qdata_scaling
         print*,'Humidity observations offset, ' ,qdata_offset
         print*,'Humidity observations missing value, '
     $        ,qdata_missing
      endif
      if (debug_init) print*

c pbh read in time varying orbital forcing
c AR (14/10/08) -- input format changed to 5 column format (time first)
c AR (14/10/08) -- also re-order time(!)
      if(orbit_radfor.eq."y".or.orbit_radfor.eq."Y") then
        open(unit=729,file=indir_name(1:lenin)// 
     $ trim(filenameorbit),iostat=ios)
        do i=1,norbit
           read(729,*,iostat=ios)(orbitall_vect(i,n),n=1,5)
        enddo
        close(729)
        do i=1,norbit
           orbitecc_vect(norbit-i+1) = orbitall_vect(i,2)
           orbitobl_vect(norbit-i+1) = orbitall_vect(i,3)
           orbitpre_vect(norbit-i+1) = orbitall_vect(i,4)
           orbittau_vect(norbit-i+1) = orbitall_vect(i,5)
        enddo
      endif
c end pbh

c AY (04/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------
c v2 seasonal. Calculate radiative forcing
c NOTE: this is where the solar constant is set
        if (debug_init) print *,'going to radfor'
        call radfor (0,gn_daysperyear,solconst,flag_ents)
        if (debug_init) print*,'file extension for output (a3) ?'
      if (debug_init) print*,lout

c AY (04/10/04) : for netCDF restarts (following Dan)
c ----------------------------------------
      if (debug_init) print*,'netCDF restart input ?'
      if (debug_init) print*,netin
      if ((netin.eq.'n').or.(netin.eq.'N')) then
        lnetin=.false.
      else
        lnetin=.true.
      endif
c      
      if (debug_init) print*,'netCDF restart output ?'
      if (debug_init) print*,netout
      if ((netout.eq.'n').or.(netout.eq.'N')) then
        lnetout=.false.
      else
        lnetout=.true.
      endif
c
      if (debug_init) print*,'ASCII restart output ?'
      if (debug_init) print*,ascout
      if ((ascout.eq.'n').or.(ascout.eq.'N')) then
        lascout=.false.
      else
        lascout=.true.
      endif
c
      if (debug_init) print*,'filename for netCDF restart input ?'
      if (debug_init) print*,filenetin 
c
      if (debug_init) print*,
     & 'directory name for netCDF restart output ?'
      if (debug_init) print*,dirnetout 
c ----------------------------------------

      if (debug_init) print*,
     & 'Force EMBM from ATCHEM atmostpheric tracer cons?'
      if (debug_init) print*,atchem_radfor

c Is this a new or continuing run?
      if(ans.eq.'n'.or.ans.eq.'N')then
         if (debug_init) print*,
     & 'this is a new run, initial conditions already set up'
c        But set up initial default time and date....
         iyear_rest=2000
         imonth_rest=1
         ioffset_rest=0
c AY (15/09/04) : why is this 2.0?  Is this some default time-step?
c        day_rest=2.0
         day_rest=yearlen / (nyear*ndta)
         if (debug_init) print*,'day_rest = ',day_rest
      else
c        This is a continuing run, read in end state filename
         if (debug_init) print*,'input file extension for input (a6)'
         if (debug_init) print*,lin

c AY (04/12/03)
         if (debug_init) print*,'Reading EMBM restart file'
         if(lnetin) then
            call inm_netcdf_embm
         else
            call check_unit(1,__LINE__,__FILE__)
            open(1,file=rstdir_name(1:lenrst)//lin,iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)
            call inm_embm(1)
            close(1,iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)

c     But set up initial default time and date....
            iyear_rest=2000
            imonth_rest=1
            ioffset_rest=0
c AY (15/09/04) : why is this 2.0?  Is this some default time-step?
c           day_rest=2.0
            day_rest=yearlen / (nyear*ndta)
            if (debug_init) print*,'day_rest = ',day_rest
         endif

c EMBM atm
         do j=1,jmax
            do i=1,imax
               tq1(1,i,j) = tq(1,i,j)
               tq1(2,i,j) = tq(2,i,j)
            enddo
         enddo
      endif

c ======================================================================
c Open output files
c ======================================================================

c AY (08/12/03) : this section creates output files that have data
c                 put into them throughout model runs (time series)

c 110  format(4e14.6,2i5,1e14.6,2i5)

c Average, etc. values of surface air temperature
      call check_unit(41,__LINE__,__FILE__)
      open(41,file=outdir_name(1:lenout)//lout//'.'//'airt',
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      write(41,*) '% EMBM atmosphere model, air temperature'
      write(41,'(4a15,a10,a15,a10)') '% time        ','N hem air temp',
     +     'S hem air temp','Max air temp',' Location','Min air temp',
     +     ' Location'
      write(41,'(4a15,2a5,a15,2a5)') '%             ','degrees C',
     +     'degrees C','degrees C','i','j','degrees C','i','j'
      close(41,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c Average, etc. values of surface specific humidity
      call check_unit(42,__LINE__,__FILE__)
      open(42,file=outdir_name(1:lenout)//lout//'.'//'q',
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      write(42,*) '% EMBM atmosphere model, specific humidity'
      write(42,'(4a15,a10,a15,a10)') '% time        ','N hem spec hum',
     +     'S hem spec hum','Max spec hum',' Location','Min spec hum',
     +     ' Location'
      write(42,'(4a15,2a5,a15,2a5)') '%             ','g / kg','g / kg',
     +     'g / kg','i','j','g / kg','i','j'
      close(42,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      if (dosc) then
c Seasonal averages
c
c AY (30/04/04) : don't need to call this here anymore
c
c At present only N. hemisphere air temperature is output
c     open(50,file=outdir_name(1:lenout)//lout//'.osc')
c     write(50,'(a26)') '%time      N. hemisphere air temp'
c     close(50)
      endif

c AY (06/01/04) : count variable for output file numbers
      iw  = 1
      iav = 1

c ======================================================================
c Setting up grid structure (D. Lunt code)
c ======================================================================

c AY (04/12/03) : This code is copied from an earlier incarnation of
c                 c-GOLDSTEIN and IGCM coupling.  It calculates grid
c                 structure for use in conversion between atmosphere
c                 and ocean grids.  Previously it was inserted just
c                 after gseta.F was executed, but I've moved it here
c                 instead.
c
c                 EMBM grid overlaps that of GOLDSTEIN, so am using
c                 code from initialise_ocean.F here.
c
c AY (04/03/04) : Grid definitions extended to include third grid
c                 position (= v points).  Original code contains 
c                 bounds error in lat3 and boxedge3_lat arrays.  An 
c                 if statement has been used here to stop this from 
c                 happening.

 313  format(i3,6f8.2)

      if (debug_init) print*
      if (debug_init) print*,'EMBM/GENIE grid interpolation variables :'
c
      if (debug_init) print*,
     & '* Longitude : alon1, alon2, alon3, abox1, abox2, abox3 *'
      do i=1,imax
         alon1(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
         alon2(i)=real(360.0*i/real(imax)+phi0/deg_to_rad)
         alon3(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
c AY (22/03/04) : extra embm.cmn arguments for netCDF writing
         nclon1(i) = alon1(i)
         nclon2(i) = real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
         nclon3(i) = alon3(i)
      end do
      do i=1,imax+1
        aboxedge1_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
        aboxedge2_lon(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
        aboxedge3_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
      end do
c
c AY (09/03/04) : write this information out
      do i=1,imax+1
         if(i.lt.imax+1) then
            if (debug_init) write(*,313) i,alon1(i),alon2(i),alon3(i),
     +           aboxedge1_lon(i),aboxedge2_lon(i),aboxedge3_lon(i)
         else
            if (debug_init) write(*,313) i,-999.99,-999.99,-999.99,
     +           aboxedge1_lon(i),aboxedge2_lon(i),aboxedge3_lon(i)
         endif
      enddo
c
      if (debug_init) print*,
     & '* Latitude : alat1, alat2, alat3, abox1, abox2, abox3 *'
      nclat3(1)=real(asin(sv(0))*180.0/pi)
      do j=1,jmax
         alat1(j)=real(asin(s(j))*180.0/pi)
         alat2(j)=real(asin(s(j))*180.0/pi)
         alat3(j)=real(asin(sv(j))*180.0/pi)
c AY (22/03/04) : extra embm.cmn arguments for netCDF writing
         nclat1(j) = alat1(j)
         nclat2(j) = alat2(j)
         if (j.lt.jmax) nclat3(j+1) =real(asin(sv(j))*180.0/pi)
      end do
      do j=1,jmax+1
         aboxedge1_lat(j)=real(asin(sv(j-1))*180.0/pi)
         aboxedge2_lat(j)=real(asin(sv(j-1))*180.0/pi)
c        AY (04/03/04) : following if statement stops bounds error
         if (j.le.jmax) aboxedge3_lat(j)=real(asin(s(j))*180.0/pi)
      end do
      aboxedge3_lat(jmax+1)=real(asin(sv(jmax))*180.0/pi)
c
c AY (09/03/04) : write this information out
      do j=1,jmax+1
         if(j.lt.jmax+1) then
            if (debug_init) write(*,313) j,alat1(j),alat2(j),alat3(j),
     +           aboxedge1_lat(j),aboxedge2_lat(j),aboxedge3_lat(j)
         else
            if (debug_init) write(*,313) j,-999.99,-999.99,-999.99,
     +           aboxedge1_lat(j),aboxedge2_lat(j),aboxedge3_lat(j)
         endif
      enddo
c
c     This bit is to make the land-sea mask on the genie grid.
c     The genie grid is offset from the goldstein grid by imax/4
c     in the longitudinal direction.
c
      do j=1,jmax
         do i=1,imax         
            if (k1(i,j).ge.90) then 
               ilandmask1(i,j)=1
               ilandmask2(i,j)=1
               ilandmask3(i,j)=1
            else
               ilandmask1(i,j)=0
               ilandmask2(i,j)=0
               ilandmask3(i,j)=0
            end if
         end do
      end do  
      if (debug_init) print*

c Output arguments
c ----------------------------------------------------------------------

c AY (17/12/03) : should perhaps pass out initial values for the
c	          fields that are passed as arguments between model
c	          modules

c AY (07/01/04) : These arguments are required in other modules

         do j=1,jmax
            do i=1,imax
c              Surface air temperature [-> surface fluxes]
               tstar_atm(i,j) = real(tq1(1,i,j))
c              Surface specific humidity [-> surface fluxes]
               qstar_atm(i,j) = real(tq1(2,i,j))
c AY (19/12/03) : Variables dztau and dztav are renamed here to
c                 separate the unscaled (hence 'us') versions read
c                 in from files, from those scaled versions used
c                 in surflux and GOLDSTEIN.
c              Wind stress x components [-> ocean, surface fluxes]
               stressxu_atm(i,j) = real(us_dztau(1,i,j))
               stressxv_atm(i,j) = real(us_dztav(1,i,j))
c              Wind stress y components [-> ocean, surface fluxes]
               stressyu_atm(i,j) = real(us_dztau(2,i,j))
               stressyv_atm(i,j) = real(us_dztav(2,i,j))
c SG > Dummy variables for ENTS
               eb_ca(i,j) = real(ca(i,j))
c SG <
               lowestlu2_atm(i,j)=real(uatm(1,i,j)*usc)
               lowestlv3_atm(i,j)=real(uatm(2,i,j)*usc)
            enddo
         enddo

c Set values of dummy variables destined for BIOGEM
       
c SG > Dummy variables for ENTS
       eb_rmax = real(rmax)
       eb_dphi = real(dphi)
       eb_rdtdim = real(rdtdim)
c SG <

c pbh read in orography even if orogswitch is off (so orography
c can be used for ice sheet melwater as well as climatology)
       if (t_orog.eq.1) then
          if(t_d18o.eq.1) then
            print*,"ERROR: Both transient orography switches are on"
            stop
          endif
          open(77,file=trim(filenameorog))
          do l=1,norog
             do i=1,imax
                do j=1,jmax
                   read(77,*) orog_vect(i,j,l)
                enddo
             enddo
          enddo
          close(77)
          surf_orog_atm(:,:)=orog_vect(:,:,1)
       else if (t_d18o.ge.1) then
c pbh read in benthic d18o time series and fields to construct transient ice sheets
c see holden et al 2009 Clim Past Disc
c d18o time series
          open(77,file=trim(filenamed18o))
          do l=1,nd18o
              read(77,*) d18o_vect(l)
          enddo
          close(77)
c threshold value of d18o at which cell is ice covered
          open(77,file=trim(filenamed18oicethresh))
          do i=1,imax
            do j=1,jmax
              read(77,*) d18o_ice_thresh(i,j)
            enddo
          enddo
          close(77)
c minimum (modern) orography
          open(77,file=trim(filenamed18oorogmin))
          do i=1,imax
            do j=1,jmax
              read(77,*) d18o_orog_min(i,j)
            enddo
          enddo
c "gradient" of orography wrt d180 
          open(77,file=trim(filenamed18ooroggrad))
          do i=1,imax
            do j=1,jmax
              read(77,*) d18o_orog_grad(i,j)
            enddo
          enddo
          close(77)
c pbh initialise orography
c pbh LOOK need to initialise this correctly for restarts
c (when ENTS restarts are enabled which they are not as of 2/2/10)
          do i=1,imax
            do j=1,jmax
              if(d18o_ice_thresh(i,j).lt.1.0e-5) then
c ocean (not used but initialised for completeness)
                surf_orog_atm(i,j)=0.0
              else if(d18o_vect(1).gt.d18o_ice_thresh(i,j)) then
c ice sheet
                surf_orog_atm(i,j)=d18o_orog_min(i,j)+
     &          d18o_orog_grad(i,j)*
     &          (d18o_vect(1)-d18o_ice_thresh(i,j))/
     &          (d18o_vect(1)-d18o_ice_thresh(i,j)+d18o_k)
              else
c land (no ice)
                surf_orog_atm(i,j)=d18o_orog_min(i,j)
              endif
            enddo
          enddo
       else
          if (orogswitch.ge.1) then
             open(77,file=trim(filenameorog))
             do i=1,imax
                do j=1,jmax
                   read(77,*) surf_orog_atm(i,j)
                enddo
             enddo
             close(77)
          else
             do i=1,imax
                do j=1,jmax
                   surf_orog_atm(i,j)=0.0
                enddo
             enddo
          endif
          if (debug_init) print*,'orog:',
     &         sum(surf_orog_atm)/size(surf_orog_atm)
       endif

       do i=1,imax
          do j=1,jmax
             torog_atm(i,j) = tstar_atm(i,j)
             if (orogswitch.eq.1) then
                torog_atm(i,j) = torog_atm(i,j)
     &               + lapse_rate*surf_orog_atm(i,j)
             endif
          enddo
       enddo
               
cmsw Read in where ice sheets are (0=ocean, 1=land, 2=ice sheet)
cdjl If t_lice is on (i.e. time-varying ice-sheets) then file is big.

      if (t_lice.eq.1) then
        if(t_d18o.eq.1) then
          print*,"ERROR: Both transient icemask switches are on"
          stop
        endif
        open(77,file=trim(filenamelice))
          do l=1,nlice
            do i=1,imax
              do j=1,jmax
                read(77,*) lice_vect(i,j,l)
              enddo
            enddo
          enddo
        close(77)
        landice_slicemask_lic(:,:)=nint(lice_vect(:,:,1))
      else if (t_d18o.ge.1) then
c pbh calculate ice mask from benthic d18o
c pbh LOOK need to initialise this correctly for restarts
c (when ENTS restarts are enabled which they are not as of 2/2/10)
        do i=1,imax
          do j=1,jmax
            if(d18o_ice_thresh(i,j).lt.1.0e-5) then
c ocean
              landice_slicemask_lic(i,j)=0.0
            else if (d18o_vect(1).gt.d18o_ice_thresh(i,j)) then
c ice sheets
              landice_slicemask_lic(i,j)=2.0
            else
c land 
              landice_slicemask_lic(i,j)=1.0
            endif
          enddo
        enddo
      else
         if (flag_ents) then
            open(77,file=trim(filenamelice))
            do i=1,imax
               do j=1,jmax
                  read(77,*) landice_slicemask_lic(i,j)
               enddo
            enddo
            close(77)
         else
            do i=1,imax
               do j=1,jmax
                  if (k1(i,j).le.kmax) then
                     landice_slicemask_lic(i,j)=0.
                  else
                     landice_slicemask_lic(i,j)=1.
                  endif
               enddo
            enddo
         endif
         if (debug_init) print*,'lice:',
     &        sum(landice_slicemask_lic)/size(landice_slicemask_lic)
      endif

cmsw Read in offline NCEP fields if offline version selected

      if (flag_ents) then
         if(ents_offlineswitch.eq.1)then
            open(77,file=indir_name(1:lenin)//'NCEP_airt_monthly.dat')
            open(78,file=indir_name(1:lenin)//'NCEP_pptn_monthly.dat')
            open(79,file=indir_name(1:lenin)//'NCEP_RH_monthly.dat')
            do l=1,nmth+1
               do i=1,imax
                  do j=1,jmax
                     read(77,*) tncep1(i,j,l)
                     read(78,*) pncep1(i,j,l)
                     read(79,*) rhncep1(i,j,l)
                  enddo
               enddo
            enddo
            close(77)
            close(78)
            close(79)

         endif

cmsw Create atmospheric albedo fields

         open(77,file=indir_name(1:lenin)//'atm_albedo_monthly.dat')
         do l=1,nmth+1
            do i=1,imax
               do j=1,jmax
                  read(77,*) atm_alb1(i,j,l)
               enddo
            enddo
         enddo
         close(77)

cmsw Read in and initialise monthly winds
  
            open(35,file=indir_name(1:lenin)//'uvic_windx.silo')
            read(35,*)(((uatml1(1,i,j,l),i=1,imax),j=1,jmax)
     1           ,l=1,nmth+1)
            close(35)
            
            open(35,file=indir_name(1:lenin)//'uvic_windy.silo')
            read(35,*)(((uatml1(2,i,j,l),i=1,imax),j=1,jmax)
     1           ,l=1,nmth+1)
            close(35)
            
cmsw read in wind speeds for use in radiation/evap calc.
            
            open(35,file=indir_name(1:lenin)//'monthly_windspd.silo')
            read(35,*)(((usurfl1(i,j,l),i=1,imax),j=1,jmax)
     1           ,l=1,nmth+1)
            close(35)

cmsw preprocess for use in tsetpa/tstipa
c conditional zonal average
cLG Step avoided if par_wind_polar_avg is 2
            if (par_wind_polar_avg.ne.2)  then
               do m=1,nmth+1
                  do j=1,jmax
                     if(j.le.2.or.j.ge.jmax-1)then
                        do l=1,2
                           tv = 0.
                           tv1= 0.
                           do i=1,imax
                              tv = tv + uatml1(l,i,j,m)
                              tv1= tv1+ usurfl1(i,j,m)
                           enddo
                           tv = tv / imax
                           tv1= tv1/ imax
c     tv = tv * imax_real
c     tv1= tv1 * imax_real
                           
                           do i=1,imax
                              uatml1(l,i,j,m) = tv
                              usurfl1(i,j,m) = tv1
                           enddo
                        enddo
                     endif
                  enddo
               enddo
            endif
            
c remove zonal average of v else fail mass conservation (may not be
c disastrous).
cLG Step avoided if par_wind_polar_avg is 2

            do l=1,nmth+1
               do i=1,imax
                  do j=1,jmax
                     uatml1(1,i,j,l) = uatml1(1,i,j,l)/usc
                     uatml1(2,i,j,l) = uatml1(2,i,j,l)/usc
                  enddo
                  if (par_wind_polar_avg.ne.2)  then
                     uatml1(2,i,jmax,l) = 0.
                  endif
               enddo
            enddo
cmsw Interpolate prescribed model fields
         call field_interp(uatml1,usurfl1,tncep1,pncep1,rhncep1,
     &        atm_alb1)
cc LG 23/11/12 I removed this part so that we keep ents
c  winds separate from embm winds even with unify_winds
         if (unify_winds .ge. 1) then
c     Replace ents uatm with embm fields
            do l=1,nyear
               do i=1,imax
                  do j=1,jmax
                     uatml(1,i,j,l)=uatm(1,i,j)
                     uatml(2,i,j,l)=uatm(2,i,j)
                  enddo
               enddo
            enddo
        endif
        if (unify_winds .eq. 1) then
c     Replace ents usurf by embm fields
            do l=1,nyear
               do i=1,imax
                  do j=1,jmax
                     usurfl(i,j,l)=usurf(i,j)
                  enddo
               enddo
            enddo
        endif
      else
         do l=1,nyear
            do i=1,imax
               do j=1,jmax
                  uatml(1,i,j,l)=0.
                  uatml(2,i,j,l)=0.
                  usurfl(i,j,l)=0.
                  tncep(i,j,l)=0.
                  pncep(i,j,l)=0.
                  rhncep(i,j,l)=0.
                  atm_alb(i,j,l)=0.
               enddo
            enddo
         enddo
      endif

c precipitation timescale and land radiation

cmsw Define lambda i.e. atm timestep/pptn timescale converted to secs
      if (debug_init) print*,'timepptn=',timepptn
      lambdapptn = min(1.,gn_daysperyear/(real(nyear)*timepptn))
      if (debug_init) print*,'lambdapptn=',lambdapptn
cmsw typical land depth scale 
      hld = 1.
cmsw typical land heat capacity (J/m3/K)
      cld = 3.3e5
      rhcld = syr/(nyear*hld*cld)
      if (debug_init) print*,'rhcld ',rhcld,nyear,hld,cld

c     Diagnostic fields of precipitation-adjusted humidity (i.e.,
c     humidity after precipitation)
c     calculate saturation humidity in line with 'surflux.F' for
c     diagnostics ('surflux.F' will update it again) and calculate
c     specific and relative humidity after adjustment for precipitation
c     has been made
      do j=1,jmax
         do i=1,imax
            if((orogswitch.lt.2).and.(flag_ents)) then
               qsat = const1*exp(const4*torog_atm(i,j)
     1              /(torog_atm(i,j)+const5))
            else
               qsat = const1*exp(const4*tstar_atm(i,j)
     1              /(tstar_atm(i,j)+const5))
            endif
            if (flag_ents) then
               deltq = lambdapptn*(qstar_atm(i,j)-(rmax*qsat))
               q_pa(i,j) = min(qstar_atm(i,j),
     &              real(qstar_atm(i,j)-deltq))
            else
               q_pa(i,j) = min(qstar_atm(i,j),
     &              real(rmax*qsat))
            endif
            rq_pa(i,j) = q_pa(i,j)/qsat
         enddo
      enddo

cghc scheme to allocate land runoff: 0 - bucket overflow (default); 1 - Leaky bucket (Meissner et al '03); 2 - soil moisture 'half-life'      
      if (debug_init) print*,'runoff scheme ',par_runoff_scheme
      if (debug_init) print*,
     & 'Clapp-Hornberger exponent  (for runoff scheme = 1) = ',
     1 par_runoff_b
      if (debug_init) print*,
     & 'soil moisture half-life (for runoff scheme = 2) = ',
     1 par_runoff_tau
     
!      conv_yr_s=3.15576E7
      if (debug_init) print*,'syr = ',syr
      
      runoff_factor_1 = 2*par_runoff_b+3
      ! dtatm*ndta*tsc = timestep in seconds
      runoff_factor_2 = 1-exp(-0.693*dtatm*ndta*tsc/
     1                  (par_runoff_tau*syr/12.0))
      if (debug_init) print*,'runoff factor 1 = ',runoff_factor_1
      if (debug_init) print*,'runoff factor 2 = ',runoff_factor_2

      print*,' <<< Initialisation complete'
      print*,'======================================================='

* ======================================================================
* end of initialise_embm.F
* ======================================================================
      return
      end

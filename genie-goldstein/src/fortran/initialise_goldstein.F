* ======================================================================
* initialise_goldstein.F
* ======================================================================
* 
* AY (28/11/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
* 
*                 this code takes pieces from mains.F, gseto.F
*
* AY (23/06/04) : code now includes parameter declarations for the
*	          ocean and sea-ice only surflux routine
*
* AY (28/09/04) : finally renamed this routine to avoid problems
*                 should we ever have another ocean module
*
* RM (13/5/05) : edited for variable sin(lat) resolution (from NRE, 6/12/04)
*                ldeg=true/false switches degree/sin lat grid
*                igrid=0,1,2... switches between grids
*
* RM (3/5/05) : edited to set up arrays for freshwater hosing 
* RM (10/8/05) : edited to set up arrays for FW flux anomalies from CMIP models

      subroutine initialise_goldstein(
     :     olon1,olat1,olon2,olat2,olon3,olat3,
     :     oboxedge1_lon,oboxedge1_lat,
     :     oboxedge2_lon,oboxedge2_lat,
     :     oboxedge3_lon,oboxedge3_lat,
     :     depth,depth1,
     :     ilandmask1,ilandmask2,ilandmask3,
     :     totsteps,
     :     tstar_ocn,sstar_ocn,ustar_ocn,vstar_ocn,
     :     albedo_ocn,
     :     ias_out,iaf_out,ips_out,ipf_out,jsf_out,
     :     lrestart_genie, 
     :     go_saln0,go_rhoair,go_cd,go_ds,go_dphi,go_ips,go_ipf,
     :     go_usc,go_dsc,go_fsc,go_rh0sc,
     :     go_rhosc,go_cpsc,go_scf,
     :     go_k1,go_dz,go_dza,
     :     go_ias,go_iaf,go_jsf,go_c,go_cv,go_s,go_sv,
     :     go_ts,go_ts1,
     :     go_rsc,go_syr,go_nyear,go_lin,go_ec,go_istep0)

      use genie_util, ONLY:check_unit,check_iostat,message,die

#include "ocean.cmn"

      include 'netcdf_grid.cmn'

c ======================================================================
c Declarations
c ======================================================================

c AY (01/12/03) : these declarations necessary for coupling grids
c      integer ilon1_ocn, ilat1_ocn, inl1_ocn
c AY (17/03/04) : commented variables excised to ocean.cmn for later use
      real olon1(maxi),olat1(maxj),
     :     olon2(maxi),olat2(maxj),
     :     olon3(maxi),olat3(maxj)
      real oboxedge1_lon(maxi+1),oboxedge1_lat(maxj+1),
     :     oboxedge2_lon(maxi+1),oboxedge2_lat(maxj+1),
     :     oboxedge3_lon(maxi+1),oboxedge3_lat(maxj+1)
      real depth(maxk),depth1(maxk+1)
      real tstar_ocn(maxi,maxj),sstar_ocn(maxi,maxj),
     :     ustar_ocn(maxi,maxj),vstar_ocn(maxi,maxj),
c     :     tavg_ocn(maxi,maxj),albedo_ocn(maxi,maxj)
     :     albedo_ocn(maxi,maxj)

c      real ocean_tracers(maxl,maxi,maxj,maxk)
c      real ref_saln

      integer ilandmask1(maxi,maxj),ilandmask2(maxi,maxj),
     :        ilandmask3(maxi,maxj)
      integer totsteps
      integer ias_out(maxj),iaf_out(maxj),
     :        ips_out(maxj),ipf_out(maxj)
      integer jsf_out
      integer bmask(maxi,maxj)

c AY (02/12/03)
c Local variables (some from gseto.F)
      integer i,j,k,l,kmxdrg,jeb,isol,isl,kk

c AY (11/12/03) : variables copied from IGCM code
      integer lnsig1
c      integer ioff1, ioff2

      real phix, th0, th1, z1, tv, tv1, tv2, tv3, tv4, tv5,
crma     1     zro(maxk), zw(0:maxk), temp0, temp1, adrag,
     1     temp0, temp1, adrag,
     2     drgf, s0, s1
cAR
crma      real ta0

      real h(3,0:maxi+1,0:maxj+1)

crma extras for general grid (rma, 2/6/05)
      real theta,thv,dth,dscon
      real deg_to_rad

crma extras for hosing (rma, 3/5/06)
      real area_hosing, syr
      integer nyears_hosing
      integer j_hosing(2)

crma extra for freshwater flux anomalies
      real fw_anom_in(maxi,maxj)

c SG > Remove hard coding of year length.
c     parameter(syr = 365*86400)
c SG <

      character ans, lin*13
      character fwanomin

crma grid type (rma, 2/6/05)
      integer igrid

      logical getj(maxi,maxj)
crma      logical ldeg
c choose between grids (ldeg=.true. gives const dlat; ldeg=.false. gives const dsinlat)
c      parameter(ldeg=.true. )
c      parameter(ldeg=.false. )

      common /lars/getj

      character(len=8) world
      character(len=3) cmip_model
      integer          lenworld
      integer          lencmip_model
c AY (02/02/05) : now read in from GOIN information
c     parameter(world='worbe2')

c+DJL 31/8/2004
c     For netcdf restarts....
      character netin*1,netout*1,ascout*1
c-DJL 31/8/2004

      logical lrestart_genie

c-DJL 21/9/05 (for reading IGCM grid), inserted by RMA

      integer nx_igcm
      parameter(nx_igcm=64)
      integer ny_igcm
      parameter(ny_igcm=32)

      real igcm_lons(nx_igcm)
      real igcm_lats(ny_igcm)
      real igcm_lons_edge(nx_igcm+1)
      real igcm_lats_edge(ny_igcm+1)

      real dx_igcm

c KICO ssmax parameters
      real ssmaxmid, ssmaxdiff, ssmaxtanhefold, ssmaxtanh0dep, zssmax

c Dummy variables to be passed to/from BIOGEM via genie.F
      real :: go_saln0
      real :: go_rhoair
      real :: go_cd
      real :: go_ds(maxj)
      real :: go_dphi
      real :: go_usc
      real :: go_dsc
      real :: go_fsc
      real :: go_rh0sc
      real :: go_rhosc
      real :: go_cpsc
      real :: go_scf
      integer,dimension(maxj) :: go_ips
      integer,dimension(maxj) :: go_ipf
      integer,dimension(maxj) :: go_ias
      integer,dimension(maxj) :: go_iaf
      integer :: go_jsf
      integer,dimension(maxi,maxj) :: go_k1
      real :: go_dz(maxk)
      real :: go_dza(maxk)
      real :: go_c(0:maxj)
      real :: go_cv(0:maxj)
      real :: go_s(0:maxj)
      real :: go_sv(0:maxj)
      real :: go_ts(1:maxl,1:maxi,1:maxj,1:maxk)
      real :: go_ts1(1:maxl,1:maxi,1:maxj,1:maxk)
      
c SG > Dummy variables to be passed to/from ENTS via genie.F
      real :: go_rsc
      real :: go_syr
      integer :: go_nyear
      character go_lin*13
      real go_ec(5)
      integer go_istep0
      integer lenrst

c SG <

c     file access checks
      integer :: ios
      logical :: ioex
      character(len=200) :: msgStr

      integer iatl

c     namelist for reading initialisation data
      NAMELIST /ini_gold_nml/indir_name,outdir_name,rstdir_name
      NAMELIST /ini_gold_nml/igrid,world
      NAMELIST /ini_gold_nml/npstp,iwstp,itstp,ianav
      NAMELIST /ini_gold_nml/conserv_per,ans,yearlen,nyear
      NAMELIST /ini_gold_nml/temp0,temp1,rel,scf,diff,adrag
      NAMELIST /ini_gold_nml/hosing,hosing_trend,nyears_hosing
      NAMELIST /ini_gold_nml/fwanomin,cmip_model,albocn
      NAMELIST /ini_gold_nml/gust,ene_tune,iconv
      NAMELIST /ini_gold_nml/imld,mldpebuoycoeff,mldketaucoeff
      NAMELIST /ini_gold_nml/mldwindkedec
      NAMELIST /ini_gold_nml/iediff,ediff0,ediffpow1,ediffpow2
      NAMELIST /ini_gold_nml/ediffvar
      NAMELIST /ini_gold_nml/ieos
      NAMELIST /ini_gold_nml/ssmaxsurf,ssmaxdeep
      NAMELIST /ini_gold_nml/tdatafile,sdatafile
      NAMELIST /ini_gold_nml/tdata_varname,sdata_varname
      NAMELIST /ini_gold_nml/tdata_missing,sdata_missing
      NAMELIST /ini_gold_nml/tdata_scaling,sdata_scaling
      NAMELIST /ini_gold_nml/tdata_offset,sdata_offset
      NAMELIST /ini_gold_nml/tsinterp,lout,netin
      NAMELIST /ini_gold_nml/netout,ascout,filenetin,dirnetout,lin
      NAMELIST /ini_gold_nml/dosc,diso,debug_init,debug_end,debug_loop
      NAMELIST /ini_gold_nml/ctrl_diagend,ctrl_diagmld
      NAMELIST /ini_gold_nml/saln0
      NAMELIST /ini_gold_nml/rst_reset_T
      NAMELIST /ini_gold_nml/par_dsc

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      j_hosing(1) = 0
      j_hosing(2) = 0
c ------------------------------------------------------------ c

c ======================================================================
c Setting up GOLDSTEIN
c ======================================================================

      print*,'======================================================='
      print*,' >>> Initialising GOLDSTEIN ocean module ...'

      if (debug_init) print*

c-DJL 21/9/05 (for reading IGCM grid), inserted by RMA 
      dx_igcm=360.0/real(nx_igcm)
      do i=1,nx_igcm
         igcm_lons(i)=real((i-1.0)*dx_igcm)
         igcm_lons_edge(i)=real((i-1.5)*dx_igcm)
      end do
      igcm_lons_edge(nx_igcm+1)=real((nx_igcm-0.5)*dx_igcm)
      call gwtcnr(igcm_lats,ny_igcm/2)
      call gwtbox(igcm_lats_edge,ny_igcm/2)
ccc
ccc      if (debug_init) print*,'These are the igcm latitude edges:'
ccc      if (debug_init) print*,igcm_lats_edge
ccc      if (debug_init) print*,'These are the igcm longitude edges:'
ccc      if (debug_init) print*,igcm_lons_edge
ccc
ccc      if (debug_init) print*,'These are the igcm latitude increments:'
ccc      do j=1,ny_igcm
ccc      if (debug_init) print*, igcm_lats_edge(j+1)-igcm_lats_edge(j)
ccc      enddo
ccc
crma      stop


c AY (01/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------
      
c read DATA (i.e. namelist) file
      call check_unit(56,__LINE__,__FILE__)
      open(unit=56,file='data_GOLD',status='old',iostat=ios)
      if (ios /= 0) then
         call die('could not open GOLDSTEIN namelist file',
     :        __LINE__,__FILE__)
      end if

c read in namelist
      read(UNIT=56,NML=ini_gold_nml,IOSTAT=ios)
      if (ios /= 0) then
         call die('could not read GOLDSTEIN namelist',
     :        __LINE__,__FILE__)
      else
         close(56,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
      end if

c SG > syr re-defined here
      syr = yearlen * 86400
      if (debug_init) print*, 'syr = ',syr
c SG <

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

c     Time-steps information
c AY (08/12/03) : have total steps passed into initialise_goldstein.F
      nsteps = totsteps
      if (debug_init) print*,'nsteps npstp iwstp itstp ianav'
      if (debug_init) print*,nsteps,npstp,iwstp,itstp,ianav
c SG
c     period of energy+water diagnostics:
      if (debug_init) print*,'period of water and energy checks:'
      if (debug_init) print*,conserv_per
c     New or continuing run
      if (debug_init) print*,'new or continuing run ?'
      if (debug_init) print*,ans

c AY (05/05/04) : number of days per GOLDSTEIN year (usually 365.25)
c     yearlen = 365.25
      if (debug_init) print*,'number of days per GOLDSTEIN year'
      if (debug_init) print*,yearlen

c AR (18/01/08) : read in choice of whether seasonality enabled
      if (debug_init) print*,'seasonality enabled =',dosc

c AR (13/06/08) : read in choice of whether to CALL diagend
      if (debug_init) print*,'CALL diagend? =',ctrl_diagend

c AR (17/11/30) : read in choice of whether to diagnose MLD
      if (debug_init) print*,'diagnose MLD? =',ctrl_diagmld
      
c AY (01/12/03) : previously in gseto.F ...
c ----------------------------------------------------------------------

      pi = 4*atan(1.0)

c dimensional scale values

      usc = 0.05
      rsc = 6.37e6
c      dsc = 5e3
      dsc = par_dsc
      fsc = 2*7.2921e-5
      gsc = 9.81
      rh0sc = 1e3
      rhosc = rh0sc*fsc*usc*rsc/gsc/dsc
      cpsc = 3981.1
      tsc = rsc/usc

c AY (26/02/04) : OPSIT scaling parameter
      opsisc = dsc * usc * rsc * 1e-6

c AY (08/12/03) : these parameters reinstated for flux scaling calcs
c EMBM scaling for heat forcing of ocean
      rfluxsc = rsc/(dsc*usc*rh0sc*cpsc)

c AR (21/08/08) : read in reference salinity
      if (debug_init) print*,'Reference salinity =',saln0

c EMBM scaling for freshwater forcing of ocean
      rpmesco = rsc*saln0/(dsc*usc)

c parameters for setting up grid
c th is latitude, coords are sin(th), longitude phi, and z

      th0 = - pi/2    
      th1 = pi/2 
      s0 = sin(th0)    
      s1 = sin(th1)     
      phix = 2*pi
      deg_to_rad = pi/180.

c grid dimensions must be no greater than array dimensions in var.cmn

      imax = maxi
      jmax = maxj
      kmax = maxk
      lmax = maxl

      dphi = phix/imax
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
      elseif(igrid.eq.2)then
crma set up IGCM grid (21/9/05)
crma note igcm orders latitude north-to-south!
         do j=1,jmax
            thv = deg_to_rad*igcm_lats_edge(jmax+1-j)
            dth = deg_to_rad*(igcm_lats_edge(jmax+2-j)
     1                      - igcm_lats_edge(jmax+1-j))
            theta = thv + 0.5*dth
            sv(j) = sin(thv)
            s(j) = sin(theta)
            cv(j) = cos(thv)
         enddo
      endif
      if (debug_init) print*,'GOLDSTEIN latitudes: velocity; tracers'
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
crma specify "undefined" values (bug spotted by Dan, 6/10/05)
crma      dsv(0) = 0.
crma      rdsv(0) = 0.
crma      rds2(1) = 0.
crma      rds2(jmax) = 0.

c AY (29/11/04) : area of grid cell (assumes sine(lat) grid)
      do j=1,jmax
      asurf(j) = rsc*rsc*ds(j)*dphi
      if (debug_init) print*,
     & 'j = ',j,'GOLDSTEIN grid cell area is',asurf(j),'m2'
      enddo

c set time to zero (may be overwritten if continuing run)

c timet0 = 0
c timet = t0

c v2 seasonality
c AY (01/12/03)
c     Number of GOLDSTEIN time steps per year
c     read(5,*)nyear,ndta
      if (debug_init) print*,'timesteps per year'
c AY (28/04/04) : on Andrew Price's recommendation, altered the
c                 descriptor in the line below from (i) to (i10)
      if(nyear.gt.maxnyr)stop 'goldstein : nyear > maxnyr' 
      if (debug_init) print*,nyear

      tv = 86400.0*yearlen/(nyear*tsc)

c     dtatm = tv/ndta

c variable timestep option not recommended
      do k=1,kmax
         dt(k) = tv  
c initialize
         dzu(1,k) = 0
         dzu(2,k) = 0
      enddo
      if (debug_init) print*,'dimensional ocean timestep',tv*tsc/86400

c set up grid
c For variable (exponential) dz use ez0 > 0, else use ez0 < 0
      ez0 = 0.1
      z1 = ez0*((1.0 + 1/ez0)**(1.0/kmax) - 1.0)
      if (debug_init) print*,'z1',z1
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

c KICO 2D "depth" grids of difference between each pair of levels
c or difference between squares (needed for PE calculation)
      do k=kmax,1,-1
         do kk=kmax,1,-1
           dzg(k,kk) = zw(k)-zw(kk-1)
           z2dzg(k,kk) = -zw(k)*zw(k) + zw(kk-1)*zw(kk-1)
           if(k.ne.(kk-1))then
             rdzg(k,kk) = 1.0/dzg(k,kk)
           else
c This number should be inifinite. Large number used instead
             rdzg(k,kk) = 1e10
           endif
         enddo
      enddo

c write dimensional vertical grid 
      if (debug_init) print*,
     & 'layer #, layer top, layer mid, layer thick'
      if (debug_init) write(6,'(i4,3e12.4)')
     & (k,dsc*zw(k),dsc*zro(k),dsc*dz(k)
     1        ,k=kmax,1,-1)
         
      dzz = dz(kmax)*dza(kmax-1)/2  

c AY (05/05/04) : when max/min overturning are calculated for the OPSIT
c                 file, they are usually full water column and include
c                 surface, wind-driven circulation
c
c                 here, a variable is set to control the level below which
c                 max/min overturning is calculated - currently it is set
c                 for overturning values on levels > 500 m deep
      overdep = 8
      do k=kmax,1,-1
         tv1 = dsc*zw(k)
         if (tv1.gt.-500.) then
            overdep = k - 1
         endif
      enddo
      if (debug_init) print*,
     & 'depth levels for OPSIT max/min overturning : 1 to',
     +     overdep

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

c set up coeffs for state equation following WS 1993

crma error: rhosc value in coeffs assumed dsc=4 km, corrected offline for IPCC runs with GENIE-1, 23/8/5 nre
crma bug remained during wider GENIE-2 testing & subsequent 'grand challenge' with earlier tuned version
crma      ec(1) = - 0.0559    /1.18376
crma      ec(2) =   0.7968    /1.18376
crma      ec(3) = - 0.0063    /1.18376
crma      ec(4) =   3.7315e-5 /1.18376

crma correct coeffs commented into CVS version, 14/6/6 rma
      ec(1) = - 0.0559 /rhosc
      ec(2) =   0.7968   /rhosc
      ec(3) = - 0.0063 /rhosc
      ec(4) =   3.7315e-5/rhosc
c KICO 04/08/08. Thermobaricity (T*z) term added as option. Optimised for -1<deep T<6, S=34.9,
c but is an order of magnitude improvement even in other parts of parameter space. It does not
c change surface densities which are fine anyway.
      if(ieos.eq.1)then
        ec(5) =  (2.5e-5*dsc) /rhosc
      else
        ec(5) = 0.0 
      endif

C SG > Passing coefficients of eqn of state to ents
      do i=1,5
         go_ec(i) = real(ec(i))
      enddo
C SG >

c read parameters 

      if (debug_init) print*,'temp0, temp1, rel, scf'
      if (debug_init) print*,temp0,temp1,rel,scf
      if (debug_init) print*,'diff(1), diff(2)'
      if (debug_init) print*,diff(1),diff(2)
      if (debug_init) print*,'inverse minimum drag in days'
      if (debug_init) print*,adrag

crma initialise arrays for freshwater hosing (rma, 3/5/06)

c FW flux conversion parameters (brought forward to where needed, rma 10/8/08)
      m2mm = 1000.
      mm2m = 1.0/(m2mm)
 
c read the initial/constant value of hosing flux (Sv) 
      if (debug_init) print*,'initial hosing =',hosing
 
c read the rate at which hosing flux is increased/decreased (Sv/kyr)
c (for "classic" hysteresis experiments)
      if (debug_init) print*,'hosing_trend =',hosing_trend
 
c read the time period of hosing (yr)
      if (debug_init) print*,'nyears_hosing =',nyears_hosing
 
c convert hosing_trend from Sv/ky to Sv/s
      hosing_trend = hosing_trend/(1e3*syr)

c convert hosing_period from yr to ocean timesteps
      nsteps_hosing = nyears_hosing*nyear

crma end of hosing extras

c define forcing

c AY (01/12/03) : this code will need to migrate to EMBM
c read wind data
c taux,tauy at u-points
c     open(96,file='taux_u.interp')
c     open(97,file='tauy_u.interp')
c taux,tauy at v-points
c     open(98,file='taux_v.interp')
c     open(99,file='tauy_v.interp')
c     do j=1,jmax
c        do i=1,imax
c rotate grid to check b.c.s
c        do idum=1,imax
c           i=1+mod(idum+18-1,36)
c           read(96,*)dztau(1,i,j)
c           read(97,*)dztau(2,i,j)
c           read(98,*)dztav(1,i,j)
c           read(99,*)dztav(2,i,j)
c multiply by scaling factor scf (to drive correct gyre strengths)
c           dztau(1,i,j) = scf*dztau(1,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c           dztau(2,i,j) = scf*dztau(2,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c           dztav(1,i,j) = scf*dztav(1,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c           dztav(2,i,j) = scf*dztav(2,i,j)/(rh0sc*dsc*usc*fsc)/dzz
c           tau(1,i,j) = dztau(1,i,j)*dzz
c           tau(2,i,j) = dztav(2,i,j)*dzz
c        enddo
c     enddo
c     close(96)
c     close(97)
c     close(98)
c     close(99)

c parameters for (restricted) time-dependent forcing 
c set sda1 < 1e05 for steady forcing 

      sda1 = 0.000
      sdomg = 2*pi/10.0

c seabed depth h needed BEFORE forcing if coastlines are non-trivial
c note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
c k1(1,j) - k1(imax+1,j) = 0

      ntot = 0
      intot = 0

c AY (01/12/03) : line modified to point to input directory
c     open(13,file=world//'.k1')
      if (debug_init) print*,'ocean bathymetry being read in'
      call check_unit(13,__LINE__,__FILE__)
      open(13,file=indir_name(1:lenin)//world(1:lenworld)//'.k1',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
c k1(1,j) - k1(imax+1,j) = 0, as enforced below;

      do j=jmax+1,0,-1
         read(13,*,iostat=ios)(k1(i,j),i=0,imax+1)
         call check_iostat(ios,__LINE__,__FILE__)
c rotate grid to check b.c.s
c        read(13,*)xxx,(k1(i,j),i=19,36),(k1(i,j),i=1,18),xxx
         k1(0,j) = k1(imax,j)
         k1(imax+1,j) = k1(1,j)
         do i=0,imax+1
c boundary condition
            do k=1,3
               h(k,i,j) = 0
               rh(k,i,j) = 0
            enddo
         enddo
         if (debug_init) write(6,'(i4,66i3)')j,(k1(i,j),i=0,imax+1)
      enddo

c read ips etc if possible

      close(13,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c count wet cells

      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax)then
               ntot = ntot + kmax - k1(i,j) + 1
               intot = intot + kmax - k1(i,j)
            endif
         enddo
      enddo

      inquire(file=indir_name(1:lenin)//world(1:lenworld)//
     & '.bmask',exist=ioex)
      if (ioex) then
         if (debug_init) print *, 'reading in basin mask file'
         call check_unit(13,__LINE__,__FILE__)
         open(13,file=indir_name(1:lenin)//world(1:lenworld)//
     & '.bmask',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         do j=jmax,1,-1
            read(13,*,iostat=ios)(bmask(i,j),i=1,imax)
            call check_iostat(ios,__LINE__,__FILE__)
            if (debug_init) write(6,'(i4,66i3)')j,(bmask(i,j),i=1,imax)
         enddo
c basin mask: 0: land
c             2: Pacific
c             3: Atlantic
c             1: remaining ocean grid cells
c for now, derive ias(:), iaf(:), ips(:), ipf(:) north of j=jsf from
c the basin mask, jsf is set to be the first grid row south of the
c southern extent of the Atlantic basin
c allow Atlantic basin to extend across western/eastern model grid boundary
         ips(:)=0
         ipf(:)=0
         ias(:)=0
         iaf(:)=0
         jsf=1
         do j=1,jmax
            iatl=0
            do i=1,imax
               if ((ips(j).eq.0).and.(bmask(i,j).eq.2)) ips(j)=i
               if (bmask(i,j).eq.2) ipf(j)=i
               if( 
     & (ias(j).ne.0).and.(bmask(i,j).gt.0).and.(bmask(i,j).ne.3) 
     & ) iatl=1
               if (((ias(j).eq.0).or.(iatl.eq.1)).and.(bmask(i,j).eq.3)) 
     & then
                 ias(j)=i
                 if(iatl.eq.1) iatl=2
               endif
               if ((bmask(i,j).eq.3).and.(iatl.eq.0)) iaf(j)=i
               if ((jsf.eq.1).and.(bmask(i,j).eq.3)) jsf=j-1
            enddo
            if (ips(j).eq.0) ips(j)=1
            if (ias(j).eq.0) ias(j)=1
         enddo
         close(13,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
      else
         if (debug_init) print *,
     & 'no basin mask available - trying to detect '//
     &        'Atlantic and Pacific basins from land mask'
         do j=1,jmax
            do i=1,imax
               if (k1(i,j).le.kmax) then
                  bmask(i,j) = 1
               else
                  bmask(i,j) = 0
               endif
            enddo
         enddo
crma - light edits of code for ocean basin start/end i-points

c find ocean positions semi-automatically, must start with a
c longitude i which is in the right ocean for all j, tricky in north


c     ****************************************************************
c     stop 'testing new code'
         ias(:)=0
         iaf(:)=0
         ips(:)=0
         ipf(:)=0
         ias(jmax) = nint(imax*24.0/36.0)
         ips(jmax) = nint(imax*10.0/36.0)
         jsf = 1
crma for 64x32l grid (15/9/05):
crma      if(ldeg) then
         if(igrid.ne.0) then
            ias(jmax) = 61
            ips(jmax) = 36
            jsf = 10
         endif
         if (debug_init) print*,'ips ipf ias iaf '
         do j=1,jmax
            ips(j) = ips(jmax)
            ipf(j) = ips(j)
            ias(j) = ias(jmax)
            iaf(j) = ias(j)
c     ************
c     This bit to get the Southern tip of Greenland into the Atlantic
            if (j.gt.nint((jmax*34.0/36.0)).and.
     &           j.le.nint((jmax*35.0/36.0))) then
               ias(j) = nint(imax*20.0/36.0)
            endif
c     ************
            do i=1,imax
               if(k1(ips(j)-1,j).le.kmax)ips(j) = ips(j) - 1
               if(k1(ipf(j)+1,j).le.kmax)ipf(j) = ipf(j) + 1
               if(k1(ias(j)-1,j).le.kmax)ias(j) = ias(j) - 1
               if(k1(iaf(j)+1,j).le.kmax)iaf(j) = iaf(j) + 1
               ips(j) = 1 + mod(ips(j)-1+imax,imax)
               ipf(j) = 1 + mod(ipf(j)-1+imax,imax)
               ias(j) = 1 + mod(ias(j)-1+imax,imax)
               iaf(j) = 1 + mod(iaf(j)-1+imax,imax)
            enddo
            if(igrid.eq.0) then 
               if(ias(j).ge.iaf(j).and.j.le.jmax/2)jsf = j
               if(ips(j).ge.ipf(j).and.j.le.jmax/2)jsf = j
            endif
         enddo

         if(igrid.eq.0) then
c     ************
c     This bit to get the Arctic all Atlantic
            do j=1,jmax
               if (j.gt.nint((jmax*35.0/36.0))) then
                  ips(j) = 1
                  ipf(j) = 0
                  ias(j) = 1
                  iaf(j) = imax
               endif
c     ************
c     This bit to get the Southern tip of Greenland out of the Pacific
               if (j.gt.nint((jmax*34.0/36.0)).and.
     &              j.le.nint((jmax*35.0/36.0))) then
                  ips(j) = 1
                  ipf(j) = 0 
               endif
c     ************
            enddo
c     ****************************************************************
         endif

crma      if(ldeg) then
         if(igrid.ne.0) then
            ips(jmax) = 1
            ipf(jmax) = 0 
            ips(jmax-1) = 1
            ipf(jmax-1) = 0 
            ias(jmax) = 1
            iaf(jmax) = imax
         endif

      endif

      if (debug_init) print*,'jsf ',jsf
      if (debug_init) write(6,'(5i4)')
     & (j,ips(j),ipf(j),ias(j),iaf(j),j=jmax,1,-1)

      do j=1,jmax
         ips_out(j)=ips(j)
         ipf_out(j)=ipf(j)
         ias_out(j)=ias(j)
         iaf_out(j)=iaf(j)
      enddo
      jsf_out=jsf

c Freshwater hosing: Initialise the denominators which are used to
c convert the total area-integrated freshwater-hosing flux in units of
c Sv into freshwater fluxes in units of m/s in the grid cells in a
c latitudinal band in the high-latitude Atlantic

c Find the region where the freshwater hosing will be applied by
c selecting grid cells in the northern Atlantic which have at least 50%
c of their surface area in the latitudinal band between 50N and 70N
      tv1=sin(50.0*pi/180.0)
      tv2=sin(70.0*pi/180.0)
      do j=1,jmax
c southern boundary
         if ((tv1.ge.sv(j-1)).and.
     &        (tv1.le.sv(j))) then
c at least half of box area has to be in latitudinal band
            if (((sv(j)-tv1)/ds(j)).ge.0.5) then
               j_hosing(1) = j
            else
               j_hosing(1) = j+1
            endif
         endif
c northern boundary
         if ((tv2.ge.sv(j-1)).and.
     &        (tv2.le.sv(j))) then
c at least half of box area has to be in latitudinal band
            if (((tv2-sv(j-1))/ds(j)).ge.0.5) then
               j_hosing(2) = j
            else
               j_hosing(2) = j-1
            endif
         endif
      enddo

      do i=1,imax
         do j=1,jmax
            rhosing(i,j) = 0.
         enddo
      enddo

      area_hosing = 0.
      do j=j_hosing(1),j_hosing(2)
         do i=ias(j),iaf(j)
            if(k1(i,j).le.kmax) area_hosing =
     &           area_hosing + asurf(j)
         enddo
      enddo
      do j=j_hosing(1),j_hosing(2)
         do i=ias(j),iaf(j)
            if(k1(i,j).le.kmax) rhosing(i,j) =
     &           1e6/area_hosing
         enddo
      enddo

      if(igrid.eq.2) then
         print*, 'rhosing(i,j) for grid 2:'
         do i=1,imax
            do j=1,jmax
               print*, i,j,rhosing(i,j)
            enddo
         enddo
      endif

crma information on FW flux anomalies (rma, 20/7/06)

c FW flux anomalies to be used?
      if (debug_init) print*,'Apply FW flux anomalies ?'
      if (debug_init) print*,fwanomin

c Read in FW flux anomaly filename, from CMIP/PMIP OAGCM (HadCM3 by default)
      lencmip_model = lnsig1(cmip_model)
      if (debug_init) print*,'CMIP model ',cmip_model(1:lencmip_model)

c initialise FW flux anomalies
      do i=1,imax
         do j=1,jmax
            fw_anom(i,j) = 0.
         enddo
      enddo

      if ((fwanomin.eq.'y').or.(fwanomin.eq.'Y')) then

crma freshwater flux anomalies
      call check_unit(13,__LINE__,__FILE__)
      open(23,file=indir_name(1:lenin)//cmip_model//'.fwanom'
     1     ,status='old',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c get anomalies in units m/y
      do j=1,jmax
         do i=1,imax
            read(23,100,iostat=ios)fw_anom_in(i,j)
            call check_iostat(ios,__LINE__,__FILE__)
         enddo
      enddo
  100 format(e14.7)
      close(23,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      do i=1,imax
         do j=1,jmax
c convert units to mm/s
            fw_anom_in(i,j) = m2mm*fw_anom_in(i,j)/syr
c get linear rate of change of fw flux anomalies in mm/s/s
            fw_anom_rate(i,j) = fw_anom_in(i,j)/(130.*syr)
         enddo
      enddo

      else

      do i=1,imax
         do j=1,jmax
            fw_anom_rate(i,j) = 0.
         enddo
      enddo

      endif

crma end of freshwater flux anomalies

c initialize psi

      do j=0,jmax
         do i=0,imax
           psi(i,j)=0.0
         enddo
         do i=0,imax+1
            ub(1,i,j) = 0
            ub(2,i,j) = 0
         enddo
      enddo

c seabed depth h

      do j=jmax+1,0,-1
         do i=0,imax+1
            if(k1(i,j).le.kmax)then
               do k=k1(i,j),kmax
                  h(3,i,j) = h(3,i,j) + dz(k)
               enddo
               rh(3,i,j) = 1.0/h(3,i,j)
            endif
         enddo
      enddo

      do j=0,jmax+1
         do i=0,imax
            h(1,i,j) = min(h(3,i,j),h(3,i+1,j))
            if(max(k1(i,j),k1(i+1,j)).le.kmax)rh(1,i,j) = 1.0/h(1,i,j)
         enddo
      enddo   

      do j=0,jmax
         do i=0,imax+1
            h(2,i,j) = min(h(3,i,j),h(3,i,j+1))
            if(max(k1(i,j),k1(i,j+1)).le.kmax)rh(2,i,j) = 1.0/h(2,i,j)
         enddo
      enddo   

      do 120 j=1,jmax
         do 120 i=1,imax
            ku(1,i,j) = max(k1(i,j),k1(i+1,j))
            ku(2,i,j) = max(k1(i,j),k1(i,j+1))
  120 continue
      tv2 = 0

c set up drag and diffusion values
c drag takes the value adrag in the interior, rising twice by factor
c drgf per gridpoint close to equator and in regions of
c shallow water (k1>kmxdrg) ie land in the case kmxdrg=kmax
c jeb = 1/2 width of equatorial region of maximum drag

      adrag = 1.0/(adrag*86400*fsc)
c cross equator need * 4 if drag is constant ie if drgf=1
      drgf = 3.0
      kmxdrg = kmax/2
      jeb = 1
      call drgset(adrag,drgf,kmxdrg,jeb)
      diff(1) = diff(1)/(rsc*usc)
      diff(2) = diff(2)*rsc/(usc*dsc*dsc)

c arrays for efficiency
      do j=1,jmax
         do i=1,imax
            rtv(i,j) = 1.0/(s(j)*s(j) + drag(1,i,j)*drag(1,i,j))
            rtv3(i,j) = 1.0/(sv(j)*sv(j) + drag(2,i,j)*drag(2,i,j))
         enddo
      enddo

      if (debug_init) print*,'dphi ds diff(1) diff(2)'
      if (debug_init) print*,dphi,ds(1),diff(1),diff(2)

c initialize some arrays to zero

      do i=0,imax
         do j=0,jmax
            do k=1,kmax
               do l=1,3
                  u(l,i,j,k) = 0
                  u1(l,i,j,k) = 0
               enddo
            enddo
         enddo
      enddo

      if (dosc) then
c v2 seasonal. Annual averages

      do j=1,jmax
         do i=1,imax
            do k=1,kmax
               do l=1,lmax
                  tsavg(l,i,j,k) = 0.
               enddo
               do l=1,3
                  uavg(l,i,j,k) = 0.
               enddo
               rhoavg(i,j,k) = 0.
            enddo
            do l=1,4
               fx0avg(l,i,j) = 0.
               fwavg(l,i,j)  = 0.
            enddo
            fx0avg(5,i,j)   = 0.
         enddo
      enddo
      endif

c initial conditions   

      do i=0,imax+1
         do j=0,jmax+1
            do k=0,kmax+1
c initial uniform temperature T0 large favours thermally direct solutions
               if(j.le.jmax/2)then
                  ts(1,i,j,k) = temp0
     1                          *0.5*(1 + sign(1,k-k1(i,j)))
               else
                  ts(1,i,j,k) = temp1
     1                          *0.5*(1 + sign(1,k-k1(i,j)))
               endif
c initial salinity
               ts(2,i,j,k) =  0.0
               ts1(1,i,j,k) = ts(1,i,j,k)
               ts1(2,i,j,k) = ts(2,i,j,k)
            enddo
            do k=1,kmax
               call eos(ec,ts(1,i,j,k),ts(2,i,j,k),zro(k),
     1            ieos,rho(i,j,k))
            enddo
         enddo
      enddo

c forcing fields and some more initialisation

cAR - amplitude of idealised wind forcing profile 
crma      ta0 = 0.5/(rh0sc*dsc*usc*fsc)

      do 20 i=1,imax
         do 30 j=1,jmax
c th at u and v points
            tv = asin(s(j)) 
            tv1 = asin(sv(j))
c convective frequency array
            cost(i,j) = 0
            icosd(i,j) = 0

            rho(i,j,0) = 0

crma extra code to set up idealised winds (should be in embm?):

c wind stress, tau(1,i,j) is the u component at a u point
c              tau(2,i,j) is the v component at a v point
c dztau(l,i,j) are the deriv's of the u,v components at u points
c dztav(l,i,j) are the deriv's of the u,v components at v points
cAR idealistic winds (yka)

crma            tau(1,i,j) = - ta0*cos(2*pi*tv/th1)
crma            dztau(1,i,j) = tau(1,i,j)/dzz
crma            tau(2,i,j) = 0
crma            dztau(2,i,j) = 0
crma            dztav(1,i,j) = - ta0*cos(2*pi*tv1/th1)
crma     1                     /dzz
crma            dztav(2,i,j) = 0

   30    continue
   20 continue
c
c array to determine limit of easy part of double p integral in J term
c use integer wetpoint indicator
c (1+sign(1,kmax-k1(i,j)))/2
c mk is largest of surrounding wet k1 values if i,j is wet, else 0
c
      do 130 j=1,jmax
         do 130 i=1,imax
            mk(i,j) = max(k1(i,j)*(1+sign(1,kmax-k1(i,j)))/2,
     1                k1(i+1,j)*(1+sign(1,kmax-k1(i+1,j)))/2,
     1                k1(i-1,j)*(1+sign(1,kmax-k1(i-1,j)))/2,
     1                k1(i,j+1)*(1+sign(1,kmax-k1(i,j+1)))/2,
     1                k1(i,j-1)*(1+sign(1,kmax-k1(i,j-1)))/2)
            mk(i,j) = mk(i,j) * (1+sign(1,kmax-k1(i,j)))/2
  130 continue

c initialize bp, only strictly needed at k=k1

      do i=1,imax+1
         do j=1,jmax
            do k=1,kmax
               bp(i,j,k) = 0.0
            enddo
         enddo
      enddo

c periodic b.c. required for Antarctic island integral

      do j=1,jmax
         mk(imax+1,j) = mk(1,j)
      enddo

c
c array to avoid J term in flat regions
c For non-trivial coasts essential to avoid adding J term at non-Psi points.
c Hence first condition ensures (i,j) is a wet Psi point, 2nd that bottom
c is not flat.
c
      do 140 j=1,jmax
         do 140 i=1,imax
            if( (max(k1(i,j),k1(i+1,j),k1(i,j+1),k1(i+1,j+1)).le.kmax)
     1          .and. (k1(i,j).ne.k1(i,j+1).or.k1(i,j).ne.k1(i+1,j)
     1                       .or.k1(i,j).ne.k1(i+1,j+1)))then
               getj(i,j) = .true.   
            else
               getj(i,j) = .false.  
            endif
  140 continue

c read island geometry file or write out for manual editing
c setting wet to zero and 1 on 1st landmass, 2 on 2nd landmass (1st island)
c etc nb narrow channels may have no wet psi points and hence not show up on 
c psi grid

c AY (01/12/03) : line modified to point to input directory
c     open(23,file=world//'.psiles',status='old')
      if (debug_init) print*,'island geometry being read in'
      call check_unit(23,__LINE__,__FILE__)
      open(23,file=indir_name(1:lenin)//world(1:lenworld)//'.psiles'
     1     ,status='old',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c SAM - 15/4/8 - isles is now a variable, determine here how
c many islands (i.e. landmasses-1) there are (maximum number in .psiles
c file minus 1)
c AR (14/09/25) : seed with isles = 0 rather than isles = 2
      isles = 0
      do j=jmax,0,-1
         read(23,*,iostat=ios)(gbold(i + j*imax),i=1,imax)
         call check_iostat(ios,__LINE__,__FILE__)
         do i=1,imax
            if(gbold(i+j*imax).gt.real(isles))isles=int(gbold(i+j*imax))
         enddo
c rotate grid to check b.c.s
      enddo
      close(23,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      isles = isles-1
      if (debug_init) print *,
     & 'Number of landmasses present in .psiles file:'
     1      ,isles+1,'(i.e.',isles,'islands)'
c AR (14/09/25) : don't exit with zero islands
ccc      if (isles.lt.1) then
ccc         call die('Not enough islands present in .psiles file!',
ccc     :        __LINE__,__FILE__)
ccc      endif
      if (isles.gt.maxisles) then
         call die('Too many islands (increase GOLDSTEINMAXISLES)!',
     :        __LINE__,__FILE__)
      endif
c 
c read island path integral data, read isles+1 paths only if want last path 
c for testing
c
c AY (01/12/03) : line modified to point to input directory
c     open(24,file=world//'.paths')
c AR (14/09/25) : added exception for zero islands
      if (isles.gt.0) then
      if (debug_init) print*,'island path integrals being read in'
      call check_unit(24,__LINE__,__FILE__)
      open(24,file=indir_name(1:lenin)//world(1:lenworld)//'.paths',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(24,*,iostat=ios)(npi(i),i=1,isles)
      call check_iostat(ios,__LINE__,__FILE__)
      do i=1,isles
         read(24,*,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         if(npi(i).gt.mpi) then
            call die('path integral around island too long',
     :           __LINE__,__FILE__)
         end if
         do j=1,npi(i)
            read(24,*,iostat=ios)lpisl(j,i), ipisl(j,i), jpisl(j,i)
            call check_iostat(ios,__LINE__,__FILE__)
c rotate grid to check b.c.s
            if(abs(lpisl(j,i)).ne.1.and.abs(lpisl(j,i)).ne.2) then
               call die('',__LINE__,__FILE__)
            end if
            if(ipisl(j,i).gt.imax.or.ipisl(j,i).lt.0) then
               call die('bad path',__LINE__,__FILE__)
            end if
            if(jpisl(j,i).gt.jmax.or.jpisl(j,i).lt.0) then
               call die('bad path',__LINE__,__FILE__)
            end if
            if(k1(ipisl(j,i),jpisl(j,i)).gt.kmax) then
               write(msgStr,*) 'dry path',j,i,ipisl(j,i),jpisl(j,i),
     &          k1(ipisl(j,i),jpisl(j,i)),kmax
              call die(msgStr,__LINE__,__FILE__)
            endif
         enddo
      enddo
      endif
      close(24,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      if (debug_init) print*,'ocean variables'
      if (debug_init) print*,
     & 'horizontal diffusivity',diff(1)*rsc*usc,' m**2/s'
      if (debug_init) print*,
     & 'vertical diffusivity',diff(2)*usc*dsc*dsc/rsc,' m**2/s'
      if (debug_init) print*,'basic drag coefficient',adrag*fsc,' /s'
      if (debug_init) print*,
     & 'wind stress scale',fsc*usc*dsc,' m**2/s**2'
      if (debug_init) print*,'or',fsc*usc*dsc*rh0sc,' N/m**2'
      if (debug_init) print*,'density variation scale',rhosc,' kg/m**3'
      if (debug_init) print*,
     & 'vertical velocity scale',usc*dsc/rsc,' m/s'
      if (debug_init) print*,'time scale',tsc/86400/yearlen,' yrs'
      if (debug_init) print*,'overturning scale',dsc*usc*rsc*1e-6,' Sv'
      if (debug_init) print*,
     & 'vertical heat flux scale',dsc*usc*rh0sc*cpsc/rsc,' W/m**2'
      if (debug_init) print*,
     & 'integrated energy scale',rh0sc*fsc*usc*rsc**3*dsc,' J'
      if (debug_init) print*,'integrated northward heat flux scale (W)'
      if (debug_init) write(6,'(e15.5)') usc*rh0sc*cpsc*rsc*dsc

c AY (09/12/03) : previously in gseta.F ...
c ----------------------------------------------------------------------

c AY (09/12/03) : This climatological albedo calculation has been
c                 reintroduced to the ocean model because albedo
c                 appears to be a desired output from the ocean
c                 model.  It may turn out that albedo is supplied
c                 instead by the surflux.F routines.
c AY (05/02/04) : Albedo calculation also restored to goldstein.F

c climatological albedo (similar to Weaver et al. 2001)

      do j=1,jmax
         tv = asin(s(j))
         tv2 = 0.2 + 0.36*0.5*(1.0 - cos(2.0*tv))
         do i=1,imax
            albcl(i,j) = tv2
         enddo
      enddo

c ======================================================================
c Setting up additional parameters for GENIE's new surflux routine
c ======================================================================

c AY (23/06/04) : the following portion of code has been added to
c	          define the values of parameters required in the
c	          ocean and sea-ice only surflux routine.  They
c	          shouldn't interfere with the existing code, so
c	          GENIE c-GOLDSTEIN should still work as usual.

    
c     ALBOCN now read in from the goin file 
      if (debug_init) print*,'ocean albedo, albocn =',albocn
c ocean albedo (set to my old Fasham, 1993, value)
c     albocn = 0.05     

c AY (02/02/05) : read in "gustiness" factor
      if (debug_init) print*,'gustiness factor, gust =',gust

c DJL(05/05/05) : read in flux factor
      if (debug_init) print*,'flux factor =',ene_tune

c RM (01/08/06) : read in choice of convection scheme
      if (debug_init) print*,'choice of convection scheme =',iconv

c drag coefficent
      cd = 0.0013
c air density
      rhoair = 1.25
c specific heat capacity of air
      cpa = 1004.
c consts for saturation specific humidity (Bolton 1980)
      const1 = 3.80*1e-3
      const2 = 21.87
      const3 = 265.5
      const4 = 17.67
      const5 = 243.5
c water density
      rho0 = 1e3
c ratio of air to ocean density
      rhoao = rhoair/rho0
c Stefan-Boltzmann constant (J/K**4/m**2/s)
      sigma = 5.67e-8
c shortwave radiation emission constant for ocean
      emo = 0.94 * sigma
c upper limit for ice temperature (set to 0C)
      tfreez = 0.
c latent heat of vapourization (J/kg)
      hlv = 2.501e6
c latent heat of fusion of ice (J/kg)
      hlf = 3.34e5
c for conservation of heat, require
      hls = hlv + hlf
c constant ice conductivity (W/m/K)
      consic = 2.166
c Kelvin temperature constant
      zeroc = 273.15
c in parameterization of heat flux at base of sea ice:
c empirical constant
      ch_ice = 0.0058
c skin friction velocity (m/s)
      u_tau_ice = 0.02
c specific heat of sea water under ice at constant pressure (J/kg/K)
      cpo_ice = 4044.
c representative ice density (kg/m**3)
      rhoice = 913.
c useful constant proportional to inverse timscale for surface freezing
c     rsictscsf = ch_ice*u_tau_ice*rho0*cpo_ice
      rsictscsf = dsc*dz(kmax)*rho0*cpo_ice/(17.5*86400.0)
c minimum average sea-ice thickness over a grid cell
      hmin = 0.01
      rhmin = 1.0/hmin
c density ratios
      rhooi = rho0/rhoice 
c melting factor
      rrholf = 1.0/(rhoice*hlf)
c FW flux conversion parameters
crma      m2mm = 1000.
crma      mm2m = 1.0/(m2mm)

      if (debug_init) print*
      if (debug_init) print*,
     & 'GENIE surflux parameters (for ocean and sea-ice fluxes)'
      if (debug_init) print*,'ocean albedo, albocn =',albocn
      if (debug_init) print*,'gustiness factor, gust =',gust
      if (debug_init) print*,'drag coefficient, cd =',cd
      if (debug_init) print*,'air density, rhoair =',rhoair
      if (debug_init) print*,'specific heat capacity of air, cpa =',cpa
      if (debug_init) print*,
     & 'constants for saturation specific humidity ...'
      if (debug_init) print*,'     const1 =',const1
      if (debug_init) print*,'     const2 =',const2
      if (debug_init) print*,'     const3 =',const3
      if (debug_init) print*,'     const4 =',const4
      if (debug_init) print*,'     const5 =',const5
      if (debug_init) print*,'water density, rho0 =',rho0
      if (debug_init) print*,'ratio of air/ocean density, rhoao =',rhoao
      if (debug_init) print*,'Stefan-Boltzmann constant, sigma =',sigma
      if (debug_init) print*,
     & 'ocean shortwave radiation emission constant, emo =',emo
      if (debug_init) print*,
     & 'upper limit for ice temperature, tfreez =',tfreez
      if (debug_init) print*,'latent heat of vapourization, hlv =',hlv
      if (debug_init) print*,'latent heat of fusion, hlf =',hlf
      if (debug_init) print*,'latent heat of sublimation, hls =',hls
      if (debug_init) print*,
     & 'constant ice conductivity, consic =',consic
      if (debug_init) print*,
     & 'Kelvin temperature constant, zeroc =',zeroc
      if (debug_init) print*,
     & 'base of sea-ice empirical constant, ch_ice =',ch_ice
      if (debug_init) print*,
     & 'skin friction velocity, u_tau_ice =',u_tau_ice
      if (debug_init) print*,
     & 'specific heat of seawater under ice, cpo_ice =',cpo_ice
      if (debug_init) print*,
     & 'representative ice density, rhoice =',rhoice
      if (debug_init) print*,
     & 'useful inverse timescale for surface freezing, ',
     +     'rsictscsf =',rsictscsf
      if (debug_init) print*,
     & 'minimum average sea-ice thickness, hmin =',hmin
      if (debug_init) print*,'density ratios, rhooi =',rhooi
      if (debug_init) print*,'melting factor, rrholf =',rrholf
      if (debug_init) print*,'m to mm conversion factor, m2mm =',m2mm
      if (debug_init) print*,'mm to m conversion factor, mm2m =',mm2m
      if (debug_init) print*

c MIXED LAYER DEPTH (mld) scheme (added to trunk 01/07/08), KICO
c mld scheme coefficients for mixing due to buoyancy and wind energy
c These are tunable parameters and should eventually be moved to genie-main
c mldpebuoycoeff = 0.15 and mldketaucoeff = 100.0 SHOULD be good starting
c points, according to theory. mldpebuoycoeff is an efficiency so should not
c exceed 1. mldwindkedec is the depth scale of exponential decay of wind
c energy efficiency, dec/dsc, where dec is the decay scale in metres. These
c paramters are now defined in xml in genie-main (e.g. definition.xml).
      mldwindkedec = mldwindkedec/dsc

c KICO mld scheme - calculate wind decay efficiency
      do k=kmax,1,-1
        mlddec(k)=exp(zro(k)/mldwindkedec)
        if(k.lt.kmax)then
          mlddecd(k)=mlddec(k)/mlddec(k+1)
        else
          mlddecd(kmax)=mlddec(kmax)
        endif
      enddo

c AY (02/02/05) : read in observational data filenames
      lentdata = lnsig1(tdatafile)
      if (debug_init) print*,'Temperature observations filename, ',
     :     tdatafile(1:lentdata)
      lensdata = lnsig1(sdatafile)
      if (debug_init) print*,'Salinity observations filename, ',
     :     sdatafile(1:lensdata)
      if (debug_init) print*
      if (tsinterp) then
         if (debug_init) print*,'Interpoate observational dataset'
         lentvar = lnsig1(tdata_varname)
         if (debug_init) print*,
     & 'Temperature observations variable name, ',
     :        tdata_varname(1:lentvar)
         if (debug_init) print*,
     & 'Temperature observations scaling factor, '
     $        ,tdata_scaling
         if (debug_init) print*,
     & 'Temperature observations offset, ' ,tdata_offset
         if (debug_init) print*,
     & 'Temperature observations missing value, '
     $        ,tdata_missing
         lensvar = lnsig1(sdata_varname)
         if (debug_init) print*,'Salinity observations variable name, ',
     :        sdata_varname(1:lensvar)
         if (debug_init) print*,'Salinity observations scaling factor, '
     $        ,sdata_scaling
         if (debug_init) print*,
     & 'Salinity observations offset, ' ,sdata_offset
         if (debug_init) print*,'Salinity observations missing value, '
     $        ,sdata_missing
      endif

c AY (01/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------

c AY (01/12/03) : this is commented out for now
c v2 seasonal. Calculate radiative forcing
c     call radfor

      if (debug_init) print*,'file extension for output (a3) ?'
      if (debug_init) print*,lout

c AY (12/12/03) : these files are now opened properly further down
c AY (01/12/03)
c     open(4,file=outdir_name(1:lenout)//lout//'.'//'t')
c     write(4,'(11(a11,3x))')
c    $              '%time      '
c    $             ,' Pac_T_d   ',' Atl_T_d   '
c    $             ,' Ind_T_d   ',' Sou_T_d   '
c    $             ,' Pac_T_u   ',' Atl_T_u   '
c    $             ,' Ind_T_u   ',' Sou_T_u   '
c    $             ,' drho/dz   ',' speed     '
c     close(4)

c AY (01/12/03)
c     open(14,file=outdir_name(1:lenout)//lout//'.'//'s')
c     write(14,'(11(a11,3x))')
c    $              '%time      '
c    $             ,' Pac_S_d   ',' Atl_S_d   '
c    $             ,' Ind_S_d   ',' Sou_S_d   '
c    $             ,' Pac_S_u   ',' Atl_S_u   '
c    $             ,' Ind_S_u   ',' Sou_S_u   '
c    $             ,' drho/dz   ',' speed     '
c     close(14)


c+DJL 31/8/2004
      if (debug_init) print*,'NETCDF restart input ?'
      if (debug_init) print*,netin
      if ((netin.eq.'n').or.(netin.eq.'N')) then
        lnetin=.false.
      else
        lnetin=.true.
      endif
      
      if (debug_init) print*,'NETCDF restart output ?'
      if (debug_init) print*,netout
      if ((netout.eq.'n').or.(netout.eq.'N')) then
        lnetout=.false.
      else
        lnetout=.true.
      endif

      if (debug_init) print*,'ASCII restart output ?'
      if (debug_init) print*,ascout
      if ((ascout.eq.'n').or.(ascout.eq.'N')) then
        lascout=.false.
      else
        lascout=.true.
      endif

      if (debug_init) print*,'filename for NETCDF restart input ?'
      if (debug_init) print*,filenetin 

      if (debug_init) print*,
     & 'directory name for NETCDF restart output ?'
      if (debug_init) print*,dirnetout 
c-DJL 31/8/2004


c Is this a new or continuing run?
      if(ans.eq.'n'.or.ans.eq.'N')then
c        This is a new run, initial conditions already set up
c+DJL 31/8/2004
c        But set up initial default time and date....
c        If we're not re-starting, then assume that
c          we are at the beginning of a year....:    
         iyear_rest=2000
         imonth_rest=1
         ioffset_rest=0
c AY (15/09/04) : why is this 2.0?  Is this some default time-step?
c         day_rest=2.0
        day_rest=yearlen / real(nyear)
         if (debug_init) print*,'day_rest = ',day_rest
c-DJL 31/8/2004
      else
c        This is a continuing run, read in end state filename
         if (debug_init) print*,'input file extension for input (a6)'
         if (debug_init) print*,lin

c AY (01/12/03)
         if (debug_init) print*,'Reading GOLDSTEIN restart file'
c+DJL 31/8/2004
        if (lnetin) then
         call inm_netcdf(lrestart_genie)
        else
         call check_unit(1,__LINE__,__FILE__)
         open(1,file=rstdir_name(1:lenrst)//lin,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         call inm(1)
         close(1,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c+DJL 14/9/2004
c        If we're re-starting from an ascii restart file, then assume that
c          the restart file was written at the end of a year....:
         iyear_rest=2000
         imonth_rest=1
         ioffset_rest=0
c AY (15/09/04) : why is this 2.0?  Is this some default time-step?
c         day_rest=2.0
        day_rest=yearlen / real(nyear)
         if (debug_init) print*,'day_rest = ',day_rest
c-DJL 14/9/2004
        endif
c-DJL 31/8/2004

         do k=1,kmax
            do j=1,jmax
               do i=1,imax
c AR (14/11/02) :: added option for resetting temperature value
c                  (code having been copied from initialization avove)
                  if (rst_reset_T) then
                     if(j.le.jmax/2)then
                        ts(1,i,j,k) = temp0
     1                       *0.5*(1 + sign(1,k-k1(i,j)))
                     else
                        ts(1,i,j,k) = temp1
     1                       *0.5*(1 + sign(1,k-k1(i,j)))
                     endif
                  end if
                  do l=1,lmax
                     ts1(l,i,j,k) = ts(l,i,j,k)
                  enddo
                  call eos(ec,ts(1,i,j,k),ts(2,i,j,k),zro(k),
     1               ieos,rho(i,j,k))
               enddo
            enddo
         enddo

      endif

c periodic b.c. (required for implicit code)

      do k=1,kmax
         do j=1,jmax
            rho(0,j,k) = rho(imax,j,k)
            rho(imax+1,j,k) = rho(1,j,k)
            do l=1,lmax
               ts(l,0,j,k) = ts(l,imax,j,k)
               ts(l,imax+1,j,k) = ts(l,1,j,k)
c for cimp.ne.1 need
               ts1(l,0,j,k) = ts(l,imax,j,k)
               ts1(l,imax+1,j,k) = ts(l,1,j,k)
            enddo
         enddo
      enddo

c oscillating forcing 

      flat = .true.
      do i=1,imax
         do j=1,jmax
            if(k1(i,j).gt.1.and.k1(i,j).le.kmax)flat = .false.
         enddo
      enddo
      if(flat)then
         if (debug_init) print*,'flat bottom'
      else
         if (debug_init) print*,'topography present'
      endif

      call invert

      do isol=1,isles

c set source term to 1 on the ith island (i+1th landmass) only

         do j=0,jmax
            do i=1,imax
               k=i + j*imax
               if(int(gbold(k)).eq.isol + 1)then
                  gb(k) = 1.0
               else
                  gb(k) = 0.0
               endif
            enddo
         enddo
         call ubarsolv(ubisl(1,0,0,isol),psisl(0,0,isol))

c find island path integral due to unit source on boundary

         do isl=1,isles
            call island(ubisl(1,0,0,isol),erisl(isl,isol),isl,0)
         enddo
      enddo

      if (debug_init) print*,
     & 'island path integrals due to unit sources',
     &       ((erisl(isl,isol),isl=1,isles),isol=1,isles)
c
crma added 20/9/05
c partially invert inland integral error matrix for psi bc calc
c
      call matinv_gold(isles,erisl)

c AY (24/01/04) : c-goldstein calls wind.f here
c	          replicate this only by fetching tau data for sake of
c                   duplicating c-goldstein output with genie.exe

      iw  = 1
      iav = 1

c v2 seasonal
c     istep0 = nint(t0/dt(kmax))
c SG > istep0 added for hdefoutput. Currently set to zero
c so RESTARTS WITH ENTS WILL NOT WORK!!!
      go_istep0 = 0
c SG <
c test
c     print*,'istep0 ',istep0

c ======================================================================
c Setting up grid structure (D. Lunt code)
c ======================================================================

c AY (01/12/03) : This code is copied from an earlier incarnation of
c                 c-GOLDSTEIN and IGCM coupling.  It calculates grid
c                 structure for use in conversion between atmosphere
c                 and ocean grids.  Previously it was inserted just
c                 after gseta.F was executed, but I've moved it here
c                 instead.
c
c AY (04/03/04) : Original code contains bounds error in lat3 and
c                 boxedge3_lat arrays.  An if statement has been used
c                 here to stop this from happening.

 111  format(i3,6f8.2)
 112  format(i3,2f8.2)

      if (debug_init) print*
      if (debug_init) print*,
     & 'GOLDSTEIN/GENIE grid interpolation variables :'
c
c     *************************
      if (debug_init) print*,
     & '* Longitude : olon1, olon2, olon3, obox1, obox2, obox3 *'
      if ((igrid.eq.0).or.(igrid.eq.1)) then
         phi0=-260.0*deg_to_rad
         do i=1,imax
            olon1(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
            olon2(i)=real(360.0*i/real(imax)+phi0/deg_to_rad)
            olon3(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
         end do
         do i=1,imax+1
            oboxedge1_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0
     $           /deg_to_rad)
            oboxedge2_lon(i)=real(360.0*(i-0.5)/real(imax)+phi0
     $           /deg_to_rad)
            oboxedge3_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0
     $           /deg_to_rad)
         end do
      else if (igrid.eq.2) then
cdjl set up IGCM grid
         phi0=igcm_lons_edge(1)*deg_to_rad
         do i=1,imax
            olon1(i)=igcm_lons(i)
            olon2(i)=igcm_lons(i)
            olon3(i)=igcm_lons(i)
         enddo
         do i=1,imax+1
            oboxedge1_lon(i)=igcm_lons_edge(i)
            oboxedge2_lon(i)=igcm_lons_edge(i)
            oboxedge3_lon(i)=igcm_lons_edge(i)
         enddo
      endif
c     *************************
c
c AY (09/03/04) : write this information out
      do i=1,imax+1
         if(i.lt.imax+1) then
c AY (17/03/04) : extra ocean.cmn arguments for netCDF writin
         nclon1(i) = olon1(i)
         if ((igrid.eq.0).or.(igrid.eq.1)) then
            nclon2(i)=real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
         else
            nclon2(i) = olon2(i)
         endif
         nclon3(i) = olon3(i)
            if (debug_init) write(*,111) i,olon1(i),olon2(i),olon3(i),
     +           oboxedge1_lon(i),oboxedge2_lon(i),oboxedge3_lon(i)
         else
            if (debug_init) write(*,111) i,-999.99,-999.99,-999.99,
     +           oboxedge1_lon(i),oboxedge2_lon(i),oboxedge3_lon(i)
         endif
      enddo
c
      if (debug_init) print*,
     & '* Latitude : olat1, olat2, olat3, obox1, obox2, obox3 *'
      nclat3(1)=real(asin(sv(0))*180.0/pi)
      do j=1,jmax
         olat1(j)=real(asin(s(j))*180.0/pi)
         olat2(j)=real(asin(s(j))*180.0/pi)
c        AY (04/03/04) : following if statement stops bounds error
         olat3(j)=real(asin(sv(j))*180.0/pi)
c AY (17/03/04) : extra ocean.cmn arguments for netCDF writing
         nclat1(j) = olat1(j)
         nclat2(j) = olat2(j)
         if (j.lt.jmax) nclat3(j+1) =real(asin(sv(j))*180.0/pi)
      end do
      do j=1,jmax+1
         oboxedge1_lat(j)=real(asin(sv(j-1))*180.0/pi)
         oboxedge2_lat(j)=real(asin(sv(j-1))*180.0/pi)
c        AY (04/03/04) : following if statement stops bounds error
         if (j.le.jmax) oboxedge3_lat(j)=real(asin(s(j))*180.0/pi)
      end do
      oboxedge3_lat(jmax+1)=real(asin(sv(jmax))*180.0/pi)
c
c AY (09/03/04) : write this information out
      do j=1,jmax+1
         if(j.lt.jmax+1) then
            if (debug_init) write(*,111) j,olat1(j),olat2(j),olat3(j),
     +           oboxedge1_lat(j),oboxedge2_lat(j),oboxedge3_lat(j)
         else
            if (debug_init) write(*,111) j,-999.99,-999.99,-999.99,
     +           oboxedge1_lat(j),oboxedge2_lat(j),oboxedge3_lat(j)
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
            endif
         enddo
      end do  
c
      if (debug_init) print*,'* Depth : depth, depth1 *'
      do l=1,kmax
         depth(l)=real(abs(dsc*zro(kmax+1-l)))
c AY (17/03/04) : extra ocean.cmn arguments for netCDF writing
         ncdepth(l) = depth(l)
      end do      
      do l=0,kmax
         depth1(l+1)=real(abs(dsc*zw(kmax-l)))
c AY (17/03/04) : extra ocean.cmn arguments for netCDF writing
         ncdepth1(l+1) = depth1(l+1)
      end do      
c
c AY (09/03/04) : write this information out
      do l=0,kmax
         if(l.gt.0) then
            if (debug_init) write(*,112) l,depth(l),depth1(l+1)
         else
            if (debug_init) write(*,112) l,-999.99,depth1(l+1)
         endif
      enddo
      if (debug_init) print*

c ======================================================================
c Open output files
c ======================================================================

c AY (08/12/03) : this section creates output files that have data
c                 put into them throughout model runs (time series)

c Average values of ocean T
      call check_unit(4,__LINE__,__FILE__)
      open(4,file=outdir_name(1:lenout)//lout//'.'//'t',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c    +     status='old')
      if (debug_init) write(4,*) 
     & '% GOLDSTEIN ocean model, ocean temperature'
c     write(4,'(11(a11,3x))')
      if (debug_init) write(4,'(11a14)',iostat=ios)
     $              '% time       '
     $             ,' Pac_T_d   ',' Atl_T_d   '
     $             ,' Ind_T_d   ',' Sou_T_d   '
     $             ,' Pac_T_u   ',' Atl_T_u   '
     $             ,' Ind_T_u   ',' Sou_T_u   '
     $             ,' drho/dz   ',' speed     '
      call check_iostat(ios,__LINE__,__FILE__)
      close(4,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c Average values of ocean S
      call check_unit(14,__LINE__,__FILE__)
      open(14,file=outdir_name(1:lenout)//lout//'.'//'s',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c    +     status='old')
      if (debug_init) write(14,*)
     & '% GOLDSTEIN ocean model, ocean salinity'
c     write(14,'(11(a11,3x))')
      if (debug_init) write(14,'(11a14)',iostat=ios)
     $              '% time       '
     $             ,' Pac_S_d   ',' Atl_S_d   '
     $             ,' Ind_S_d   ',' Sou_S_d   '
     $             ,' Pac_S_u   ',' Atl_S_u   '
     $             ,' Ind_S_u   ',' Sou_S_u   '
     $             ,' drho/dz   ',' speed     '
      call check_iostat(ios,__LINE__,__FILE__)
      close(14,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c Values of ocean maximum/minimum circulation
      call check_unit(40,__LINE__,__FILE__)
      open(40,file=outdir_name(1:lenout)//lout//'.'//'opsit',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c    +     status='new')
      if (debug_init) write(40,*)
     &  '% GOLDSTEIN ocean model, ocean overturning'
      if (debug_init) write(40,'(5a14,a10)',iostat=ios)
     &  '% time       ','Pacific min',
     +     'Pacific max','Atlantic min','Atlantic max',
     +     ' Location'
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_init) write(40,'(5a14,2a5)',iostat=ios) '%            ',
     +     'Sv','Sv','Sv','Sv','j','k'
      call check_iostat(ios,__LINE__,__FILE__)
      close(40,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c Values of ocean basin FW and heat fluxes
      call check_unit(41,__LINE__,__FILE__)
      open(41,file=outdir_name(1:lenout)//lout//'.'//'flux',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c    +     status='new')
      if (debug_init) write(41,'(3a28)',iostat=ios)
     &  '% GOLDSTEIN ocean model, FW ',
     +     'and heat fluxes, first data ',
     +     'line is region area (m2)    '
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_init) write(41,'(11a14)',iostat=ios) '% time        ',
     +     ' ',' ',' Total FW ',' ',' ',
     +     ' ',' ','Total heat',' ',' '
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_init) write(41,'(11a14)',iostat=ios) '%             ',
     +     'Global','Atlantic','Pacific','Indian','Southern',
     +     'Global','Atlantic','Pacific','Indian','Southern'
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_init) write(41,'(11a14)',iostat=ios) '%             ',
     +     'Sv','Sv','Sv','Sv','Sv',
     +     'PW','PW','PW','PW','PW'
      call check_iostat(ios,__LINE__,__FILE__)
c    +     'W/m2','W/m2','W/m2','W/m2','W/m2'
      close(41,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c file for writing extra freshwater forcing
      call check_unit(44,__LINE__,__FILE__)
      open(44,file=outdir_name(1:lenout)//lout//'.'//'hose',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_init) write(44,*)
     &  '% GOLDSTEIN ocean model, extra FW forcing'
      close(44,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      if (dosc) then
c Seasonal averages
c
c AY (30/04/04) : don't need to call this here anymore
c
c     open(50,file=outdir_name(1:lenout)//lout//'.osc')
c    +     status='new')
c     close(50)
      endif

c Output arguments
c ----------------------------------------------------------------------

c AY (07/01/04) : These arguments are required in other modules
c AY (09/01/04) : Note that the velocity components are zero at this
c                 point unless the run is restarted from a previous
c                 run - ideally, the components should be calculated
c                 once surface wind stresses are known
      do j=1,jmax
         do i=1,imax
c           Sea surface temperature [-> surface fluxes]
            tstar_ocn(i,j) = real(ts(1,i,j,kmax))
c           Sea surface salinity [-> surface fluxes]
            sstar_ocn(i,j) = real(ts(2,i,j,kmax))
c           Surface velocity (u component) [-> sea-ice]
            ustar_ocn(i,j) = real(u(1,i,j,kmax))
c           Surface velocity (v component) [-> sea-ice]
            vstar_ocn(i,j) = real(u(2,i,j,kmax))
c           Ocean albedo
            albedo_ocn(i,j) = real(albocn)
         enddo
      enddo
      
c Set values of dummy variables destined for BIOGEM
      go_saln0 = real(saln0)
      go_rhoair = real(rhoair)
      go_cd = real(cd)
      go_dphi = real(dphi)
      do j=1,jmax
         go_ips(j) = ips(j)
         go_ipf(j) = ipf(j)
         go_ias(j) = ias(j)
         go_iaf(j) = iaf(j)
         go_ds(j) = real(ds(j))
      enddo     
      go_jsf = jsf
      go_usc = real(usc)
      go_dsc = real(dsc)
      go_fsc = real(fsc)
      go_rh0sc = real(rh0sc)
      go_rhosc = real(rhosc)
      go_cpsc = real(cpsc)
      go_scf = real(scf)
      do j=1,jmax
         do i=1,imax
            go_k1(i,j) = k1(i,j)
         enddo
      enddo     
      do k=1,kmax
         go_dz(k)  = real(dz(k))
         go_dza(k) = real(dza(k))
      enddo     
      do j=0,jmax
         go_c(j)  = real(c(j))
         go_cv(j) = real(cv(j))
         go_s(j)  = real(s(j))
         go_sv(j) = real(sv(j))
      enddo     
      do k=1,kmax
         do j=1,jmax
            do i=1,imax
               do l=1,lmax
                  go_ts(l,i,j,k)  = real(ts(l,i,j,k))
                  go_ts1(l,i,j,k) = real(ts1(l,i,j,k))
               enddo
            enddo
         enddo
      enddo  

c KICO Choose diapycnal mixing scheme. iediff=0 (needed for tests to pass),
c is uniform diffusivity.
      if(iediff.gt.0)then
        call ediff
      endif

c KICO Define ssmax for isopycnal mixing as function of depth
      if(((ssmaxsurf-ssmaxdeep).lt.1e-7).AND.
     1      ((ssmaxsurf-ssmaxdeep).gt.-1e-7))then
        do k=1,kmax-1
          ssmax(k)=ssmaxdeep
        enddo
      else
        ssmaxmid=0.5*(log(ssmaxsurf)+log(ssmaxdeep))
        ssmaxdiff=0.5*(log(ssmaxsurf)-log(ssmaxdeep))
        ssmaxtanhefold=200/dsc
        ssmaxtanh0dep=-300/dsc
        do k=1,kmax-1
          zssmax=(zw(k)-ssmaxtanh0dep)/ssmaxtanhefold
          ssmax(k)=exp(ssmaxmid+ssmaxdiff*tanh(zssmax))
        enddo
      endif

c SG > Set values for ENTS
      go_rsc = real(rsc)
      go_nyear = nyear
      go_syr = real(syr)
      go_lin = lin
c SG <

      print*,' <<< Initialisation complete'
      print*,'======================================================='


* ======================================================================
* end of initialise_goldstein.F
* ======================================================================

      return
      end

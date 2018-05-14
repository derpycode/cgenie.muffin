* ======================================================================
* initialise_seaice.F
* ======================================================================
*
* AY (03/12/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
* 
*                 this code takes pieces from mains.F, gseta.F
*	          and gseto.F
*
* AY (20/01/04) : EMBM and surflux code excised
*
* AY (23/09/04) : upgraded to handle sea-ice albedo
*
* RM (20/5/05) : edited for variable sin(lat) resolution (from NRE, 6/12/04)
*                ldeg=true/false switches degree/sin lat grid
*                igrid=0,1,2... switches between grids

      subroutine initialise_seaice(
     :     ilon1,ilat1,ilon2,ilat2,ilon3,ilat3,
     :     iboxedge1_lon,iboxedge1_lat,
     :     iboxedge2_lon,iboxedge2_lat,
     :     iboxedge3_lon,iboxedge3_lat,
     :     ilandmask1,ilandmask2,ilandmask3,
     :     totsteps,
     :     hght_sic,frac_sic,temp_sic,albd_sic,
     :     test_energy_seaice)

      use genie_util, ONLY: check_unit,check_iostat

#include "seaice.cmn"

      include 'netcdf_grid.cmn'

c ======================================================================
c Declarations
c ======================================================================

c AY (20/01/04)
c Declare variables passed into this subroutine
      real
     :     ilon1(maxi),ilat1(maxj),
     :     ilon2(maxi),ilat2(maxj),
     :     ilon3(maxi),ilat3(maxj),
     :     iboxedge1_lon(maxi+1),iboxedge1_lat(maxj+1),
     :     iboxedge2_lon(maxi+1),iboxedge2_lat(maxj+1),
     :     iboxedge3_lon(maxi+1),iboxedge3_lat(maxj+1)

      integer ilandmask1(maxi,maxj),ilandmask2(maxi,maxj),
     :     ilandmask3(maxi,maxj)

      integer totsteps

      real
     :     hght_sic(maxi,maxj),
     :     frac_sic(maxi,maxj),
     :     temp_sic(maxi,maxj),
     :     albd_sic(maxi,maxj)

c AY (20/01/04)
c Local variables

      real tv
      real u_tau_ice,ch_ice,cpo_ice
c AY (03/05/04) : moved to seaice.cmn
c     real asurf

crma extra explicit declarations (rma, 22/12/03)
      real th0, th1, s0, s1, phix

crma extras for general grid (rma, 2/6/05)
      real theta,thv,dth,dscon
      real deg_to_rad

      integer i, j, l, ios

crma extra explicit declarations (rma, 22/12/03)
      integer lnsig1

crma grid type (rma, 2/6/05)
      integer igrid

crma      logical ldeg
c choose between grids (ldeg=.true. gives const dlat; ldeg=.false. gives const dsinlat)
c      parameter(ldeg=.true. )
c      parameter(ldeg=.false. )

      character(len=8) world
      integer          lenworld
c AY (02/02/05) : now read in from GOIN information
c     parameter(world='worbe2')

c AY (09/01/04) : local variables reinstated
      character ans, lin*13

c AY (04/10/04) : for netCDF restarts (following Dan)
      character netin*1,netout*1,ascout*1

      real test_energy_seaice

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

      NAMELIST /ini_sic_nml/indir_name,outdir_name,rstdir_name
      NAMELIST /ini_sic_nml/igrid,world
      NAMELIST /ini_sic_nml/npstp,iwstp,itstp,ianav,conserv_per
      NAMELIST /ini_sic_nml/ans,yearlen,nyear,diffsic,lout
      NAMELIST /ini_sic_nml/netin,netout,ascout,filenetin
      NAMELIST /ini_sic_nml/dirnetout,lin
      NAMELIST /ini_sic_nml/dosc,impsic,debug_init,debug_end,debug_loop
      NAMELIST /ini_sic_nml/par_sica_thresh,par_sich_thresh
      NAMELIST /ini_sic_nml/maxalbice,scale_oheat

c ======================================================================
c Setting up sea-ice
c ======================================================================

      print*,'======================================================='
      print*,' >>> Initialising sea-ice module ...'

c AY (22/10/04) : just for reference ...
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

ccc      if (debug_init) print*,'These are the igcm latitude edges:'
ccc      if (debug_init) print*,igcm_lats_edge
ccc      if (debug_init) print*,'These are the igcm longitude edges:'
ccc      if (debug_init) print*,igcm_lons_edge
ccc
crma      stop

c AY (01/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------
      
c read DATA (i.e. namelist) file
      call check_unit(56,__LINE__,__FILE__)
      open(unit=56,file='data_goldSIC',status='old',iostat=ios)
      if (ios /= 0) then
         print*,'ERROR: could not open SIC namelist file'
         stop
      end if

c read in namelist
      read(UNIT=56,NML=ini_sic_nml,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read SIC namelist'
         stop
      else
         close(56,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
      end if

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
         rstdir_name(lenrst:lenrst) = '/'
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
c AY (08/12/03) : have total steps passed into initialise_ocean.F
      nsteps = totsteps
      if (debug_init) print*,'npstp iwstp itstp ianav'
      if (debug_init) print*,npstp,iwstp,itstp,ianav
c     period of energy+water diagnostics:
      if (debug_init) print*,'period of water and energy checks:'
      if (debug_init) print*,conserv_per
c     New or continuing run
      if (debug_init) print*,'new or continuing run ?'
      if (debug_init) print*,ans

c AY (05/05/04) : number of days per GOLDSTEIN sea-ice year (usually 365.25)
c     yearlen = 365.25
      if (debug_init) print*,'number of days per GOLDSTEIN sea-ice year'
      if (debug_init) print*,yearlen

c AR (18/01/08) : read in choice of whether seasonality enabled
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

c EMBM reference salinity
c     saln0 = 34.9

c parameters for setting up grid
c th is latitude, coords are sin(th), longitude phi, and z

      th0 = - pi/2    
      th1 = pi/2 
      s0 = sin(th0)    
      s1 = sin(th1)     
      phix = 2*pi
      deg_to_rad = pi/180.

c grid dimensions must be no greater than array dimensions in var.cmn
c kmax needed to interpret topography file for masking

      imax = maxi
      jmax = maxj 
      kmax = maxk

      dphi = phix/imax
      if (igrid.lt.2) then
         phi0 = -260.0*deg_to_rad
      else
         phi0 = igcm_lons_edge(1)*deg_to_rad
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
      elseif(igrid.eq.2)then
crma set up IGCM grid (21/9/05)
crma note igcm orders latitude north-to-south!
         do j=1,jmax
            thv = deg_to_rad*igcm_lats_edge(jmax+1-j)
            dth = deg_to_rad*(igcm_lats_edge(jmax+2-j)
     1                      - igcm_lats_edge(jmax+1-j))
            theta = thv - 0.5*dth
            sv(j) = sin(thv)
            s(j) = sin(theta)
            cv(j) = cos(thv)
         enddo
      endif
      if (debug_init) print*,'SIC latitudes: velocity; tracers'
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

c v2 seasonality
      if (debug_init) print*,'timesteps per year'
      if (debug_init) print*,nyear
      tv = 86400.0*yearlen/(nyear*tsc)

c AY (13/01/04) : oops!  left this out
      dtsic = tv  
      if (debug_init) print*,'sea-ice timestep in days',tv*tsc/86400

      rdtdim = 1.0/(tsc*dtsic)
      if (debug_init) print*,'rdtdim = ',rdtdim

c efficiency array

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

c AY (04/12/03) : reading in ocean bathymetry/runoff patterns - not
c                 sure yet if this is entirely necessary.
c seabed depth h needed BEFORE forcing if coastlines are non-trivial
c note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
c k1(1,j) - k1(imax+1,j) = 0

c AY (06/01/04) : reinstate bathymetry/runoff read-in
c AY (01/12/03) : line modified to point to input directory
c     open(13,file=world//'.k1')
      if (debug_init) print*,'Bathymetry being read in'
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
         if(j.ne.0.and.j.ne.jmax+1.and.debug_init)
     1   write(6,'(i4,32i3)')j,(k1(i,j),i=1,32)
      enddo

c read ips etc if possible

      close(13,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      if (debug_init) print*,'Bathymetry successfully read in'

c AY (01/12/03) : previously in gseta.F ...
c ----------------------------------------------------------------------

c AY (11/12/03) : a lot of the following needs removed to surflux
c constants used in SSW parameterization

cc EMBM stuff follows...

      do j=1,jmax
      asurf(j) = rsc*rsc*ds(j)*dphi
      if (debug_init) 
     & print*,'j = ',j,'SIC grid cell area is',asurf(j),'m2'
      enddo

c read some scalings

c AY (11/12/03) : year length inconsistent here - corrected to 365.25
      ryear = 1.0/(yearlen*86400)

c more constants

c     rhoair = 1.25
      rho0 = 1e3
c latent heat of fusion of ice (J/kg)
      hlf = 3.34e5

c AY (06/01/04) : sea-ice parameter definitions
c                 reinstated from sea-ice section of gseta.f

c read sea-ice parameters

      if (debug_init) print*,'diffsic'
      if (debug_init) print*,diffsic
      if (debug_init) print*,'maxalbice'
      if (debug_init) print*,maxalbice
      if (debug_init) print*,'scale_oheat'
      if (debug_init) print*,scale_oheat

c non-dimensionalise eddy diffusivity
      diffsic = diffsic/(rsc*usc)
      
c empirical constant
      ch_ice = 0.0058
c skin friction velocity (m/s)
      u_tau_ice = 0.02
c specific heat of sea water under ice at constant pressure (J/kg/K)
      cpo_ice = 4044
c representative ice density (kg/m**3)
      rhoice = 913.
c representative under-seaice water density (kg/m**3)
c     rho0sea = 1035.
      hmin = 0.01
      rhmin = 1.0/hmin
c density ratios
      rhooi = rho0/rhoice 
      rhoio = rhoice/rho0 
c FW flux conversion parameters
      m2mm = 1000.
      mm2m = 1.0/(m2mm)

      if (debug_init) print*,'sea-ice variables'
      if (debug_init) print*,'water density, rho0 =',rho0
      if (debug_init) print*,'latent heat of fusion, hlf =',hlf
      if (debug_init) print*,
     :     '(dimensional) sea-ice diffusion, diffsic =',
     :     diffsic*(rsc*usc)
      if (debug_init) print*,
     :     'base of sea-ice empirical constant, ch_ice =',ch_ice
      if (debug_init) print*,
     :     'skin friction velocity, u_tau_ice =',u_tau_ice
      if (debug_init) print*,
     :     'specific heat of seawater under ice, cpo_ice =',cpo_ice
      if (debug_init) print*,
     :     'representative ice density, rhoice =',rhoice
      if (debug_init) print*,
     :     'minimum average sea-ice thickness, hmin =',hmin
      if (debug_init) print*,'density ratios, rhooi =',rhooi
      if (debug_init) print*,'m to mm conversion factor, m2mm =',m2mm
      if (debug_init) print*,'mm to m conversion factor, mm2m =',mm2m
      if (debug_init) print*

      do j=1,jmax
         do i=1,imax

c initialize  sea ice

c thickness, fractional area and temperature

            varice(1,i,j) = 0.
            varicedy(1,i,j) = 0.
            varicedy(2,i,j) = 0.
            variceth(1,i,j) = 0.
            variceth(2,i,j) = 0.
            varice1(1,i,j) = varice(1,i,j)
            varice(2,i,j) = 0.
            varice1(2,i,j) = varice(2,i,j)
            tice(i,j) = 0.
            albice(i,j) = 0.

            if (dosc) then
c v2 seasonal. Annual averages

            do l=1,2
c AY (20/01/04) : EMBM properties excised
c              tqavg(l,i,j) = 0.
               haavg(l,i,j) = 0.
            enddo
            ticeavg(i,j) = 0.
            albiceavg(i,j) = 0.
            dthaavg(1,i,j) = 0.
            dthaavg(2,i,j) = 0.
            fxdelavg(i,j) = 0.
            fwdelavg(i,j) = 0.
         endif
         enddo
      enddo

c AY (11/12/03) : calculated in surflux - excised from here
c diagnostic calculation of global heat source
c
c     ghs = 0.

c AY (04/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------

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
      if (debug_init) print*,'directory name for netCDF restart output ?'
      if (debug_init) print*,dirnetout 
c ----------------------------------------

c Is this a new or continuing run?
      if(ans.eq.'n'.or.ans.eq.'N')then
         if (debug_init) print*,
     :     'this is a new run, initial conditions already set up'
c        But set up initial default time and date....
         iyear_rest=2000
         imonth_rest=1
         ioffset_rest=0
c AY (15/09/04) : why is this 2.0?  Is this some default time-step?
c        day_rest=2.0
         day_rest=yearlen / nyear
         if (debug_init) print*,'day_rest = ',real(day_rest)
      else
c        This is a continuing run, read in end state filename
         if (debug_init) print*,'input file extension for input (a6)'
         if (debug_init) print*,lin

c AY (04/12/03)
         if (debug_init) print*,'Reading sea-ice restart file'
         if(lnetin) then
            call inm_netcdf_sic
         else
            open(1,file=rstdir_name(1:lenrst)//lin)
            call inm_seaice(1)
            close(1)
c        But set up initial default time and date....
            iyear_rest=2000
            imonth_rest=1
            ioffset_rest=0
c AY (15/09/04) : why is this 2.0?  Is this some default time-step?
c           day_rest=2.0
            day_rest=yearlen / real(nyear)
            if (debug_init) print*,'day_rest = ',day_rest
         endif
         
c AY (20/01/04)
c Sea-ice
         do j=1,jmax
            do i=1,imax
               varice1(1,i,j) = varice(1,i,j)
               varice1(2,i,j) = varice(2,i,j)
            enddo
         enddo
      endif

c ======================================================================
c Open output files
c ======================================================================

c AY (08/12/03) : this section creates output files that have data
c                 put into them throughout model runs (time series)

c 110  format(6e14.6,2i5,1e14.6,2i5)

c Average, etc. values of sea-ice properties
      call check_unit(43,__LINE__,__FILE__)
      open(43,file=outdir_name(1:lenout)//lout//'.'//'sic',
     +     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      write(43,*) '% GOLDSTEIN sea-ice, sea-ice properties'
      write(43,'(6a15,a10,a15,a10,a15,a10)',iostat=ios)'% time        ',
     +     'N hem ice vol','S hem ice vol','N hem ice area',
     +     'S hem ice area','Max ice thick',' Location','Min ice temp',
     +     ' Location',     'Max ice albedo',' Location'
      call check_iostat(ios,__LINE__,__FILE__)
      write(43,'(6a15,2a5,a15,2a5,a15,2a5)',iostat=ios)'%             ',
     +     'm^3','m^3','m^2','m^2','m','i','j',
     +     'degrees C','i','j','dimensionless','i','j'
      call check_iostat(ios,__LINE__,__FILE__)
      close(43,iostat=ios)
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
c                 Sea-ice grid overlaps that of GOLDSTEIN, so am using
c                 code from initialise_ocean.F here.
c
c AY (04/03/04) : Original code contains bounds error in lat3 and
c                 boxedge3_lat arrays.  An if statement has been used
c                 here to stop this from happening.

 314  format(i3,6f8.2)

      if (debug_init) print*
      if (debug_init) print*,
     & 'Sea-ice/GENIE grid interpolation variables :'
c
      if (debug_init) print*,
     & '* Longitude : ilon1, ilon2, ilon3, ibox1, ibox2, ibox3 *'
      do i=1,imax
         ilon1(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
         ilon2(i)=real(360.0*i/real(imax)+phi0/deg_to_rad)
         ilon3(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
c AY (22/03/04) : extra embm.cmn arguments for netCDF writing
         nclon1(i) = ilon1(i)
         nclon2(i) = real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
         nclon3(i) = ilon3(i)
      end do
      do i=1,imax+1
         iboxedge1_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
         iboxedge2_lon(i)=real(360.0*(i-0.5)/real(imax)+phi0/deg_to_rad)
         iboxedge3_lon(i)=real(360.0*(i-1.0)/real(imax)+phi0/deg_to_rad)
      end do
c
c AY (09/03/04) : write this information out
      if (debug_init) then
      do i=1,imax+1
         if(i.lt.imax+1) then
            write(*,314) i,ilon1(i),ilon2(i),ilon3(i),
     +           iboxedge1_lon(i),iboxedge2_lon(i),iboxedge3_lon(i)
         else
            write(*,314) i,-999.99,-999.99,-999.99,
     +           iboxedge1_lon(i),iboxedge2_lon(i),iboxedge3_lon(i)
         endif
      enddo
      endif
c
      if (debug_init) print*,
     &    '* Latitude : ilat1, ilat2, ilat3, ibox1, ibox2, ibox3 *'
      nclat3(1)=real(asin(sv(0))*180.0/pi)
      do j=1,jmax
         ilat1(j)=real(asin(s(j))*180.0/pi)
         ilat2(j)=real(asin(s(j))*180.0/pi)
         ilat3(j)=real(asin(sv(j))*180.0/pi)
c AY (22/03/04) : extra embm.cmn arguments for netCDF writing
         nclat1(j) = ilat1(j)
         nclat2(j) = ilat2(j)
         if (j.lt.jmax) nclat3(j+1)=real(asin(sv(j))*180.0/pi)
      end do
      do j=1,jmax+1
         iboxedge1_lat(j)=real(asin(sv(j-1))*180.0/pi)
         iboxedge2_lat(j)=real(asin(sv(j-1))*180.0/pi)
c        AY (04/03/04) : following if statement stops bounds error
         if (j.le.jmax) iboxedge3_lat(j)=real(asin(s(j))*180.0/pi)
      end do
      iboxedge3_lat(jmax+1)=real(asin(sv(jmax))*180.0/pi)
c
c AY (09/03/04) : write this information out
      if (debug_init) then
      do j=1,jmax+1
         if(j.lt.jmax+1) then
            write(*,314) j,ilat1(j),ilat2(j),ilat3(j),
     +           iboxedge1_lat(j),iboxedge2_lat(j),iboxedge3_lat(j)
         else
            write(*,314) j,-999.99,-999.99,-999.99,
     +           iboxedge1_lat(j),iboxedge2_lat(j),iboxedge3_lat(j)
         endif
      enddo
      endif
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

c+DJL initialise the seaice energy=0.0
      test_energy_seaice=0.0

c Output arguments
c ----------------------------------------------------------------------

c AY (17/12/03) : should perhaps pass out initial values for the
c	          fields that are passed as arguments between model
c	          modules

c AY (07/01/04) : These arguments are required in other modules

         do j=1,jmax
           do i=1,imax
c              Sea-ice height              [-> surface fluxes]
               hght_sic(i,j) = real(varice(1,i,j))
c              Sea-ice fractional area     [-> surface fluxes]
               frac_sic(i,j) = real(varice(2,i,j))
c              Sea-ice surface temperature [-> surface fluxes]
               temp_sic(i,j) = real(tice(i,j))
c              Sea-ice albedo              [-> surface fluxes]
               albd_sic(i,j) = real(albice(i,j))
            enddo
         enddo

      print*,' <<< Initialisation complete'
      print*,'======================================================='

* ======================================================================
* end of initialise_seaice.F
* ======================================================================

      return
      end

      module icemod

      use geniemod
      implicit none
!
!     version identifier (date)
!
      character(len=80) :: version = '13.10.2005 by Larry'
!
!
!     Parameter
!
      integer NPRO,NLAT,NLON,NLPP,NHOR,NROOT
      parameter(NPRO = NPRO_ATM)        ! Number of processes (resmod)
      parameter(NLAT = NLAT_ATM)        ! Number of latitudes (resmod)
      parameter(NLON = NLAT + NLAT)     ! Number of longitudes
      parameter(NLPP = NLAT / NPRO)     ! Latitudes per process
      parameter(NHOR = NLON * NLPP)     ! Horizontal part
      parameter(NROOT = 0)              ! Master node
!
      real TFREEZE,TMELT,CRHOS,CRHOI,CRHOF,CRHOSN,CPS,CPI,CPSN,CKAPI,&
           CKAPSN,CLFI,CLFSN
      parameter(TFREEZE=271.25)         ! freezing temp. for sea ice at S=34.7
      parameter(TMELT=273.16)           ! melting temp. for snow (0 deg C)
                                        ! ALL DENSITIES IN (kg/m**3)
      parameter(CRHOS = 1030.)          ! DENSITY OF SEA WATER AT S=34.7
      parameter(CRHOI = 920.)           ! DENSITY OF ICE
      parameter(CRHOF = 1003.8)         ! DENSITY OF 'FRESH' WATER AT S=5
      parameter(CRHOSN = 330.)          ! DENSITY OF SNOW
      parameter(CPS = 4180.)            ! SPECIFIC HEAT OF SEA WATER (J/kg*K)
      parameter(CPI = 2070.)            ! SPECIFIC HEAT OF ICE (J/(kg*K))
      parameter(CPSN = 2090.)           ! SPECIFIC HEAT OF SNOW (J/(kg*K))
      parameter(CKAPI = 2.03)           ! HEAT CONDUCTIVITY IN ICE (W/(m*K))
      parameter(CKAPSN = 0.31)          ! HEAT CONDUCTIVITY IN SNOW (W/(m*K))
      parameter(CLFI  = 3.28E5)         ! HEAT OF FUSION OF ICE (J/kg)
      parameter(CLFSN = 3.337E5)        ! HEAT OF FUSION OF SNOW (J/kg)
!
!     namelist parameters
!
      integer :: nice       = 1    ! compute ice yes/no (1/0)
      integer :: newsurf    = 0    ! 1: read surface data after restart
      integer :: nsnow      = 1    ! allow snow on ice yes/no (1/0)
      integer :: ntskin     = 1    ! compute skin temperature (0=clim.)
      integer :: ntspd      = 32   ! number of time steps per day
      integer :: noutput    = 1    ! master switch for output
      integer :: nout       = 32   ! output each nout timesteps
      integer :: nfluko     = 0    ! switch for flux correction
                                   ! (0 = none, 1 = heat-budget, 2 = newtonian)
      integer :: ncpl_ice_ocean = 1! coupling intervall ice - ocean
! PBH
      integer :: nflukoavg  = 115200	!timesteps for averaging flux correction

      integer :: nperpetual_ice = 0! perpetual climate conditions (day)
      integer :: nprint = 0        ! debug print out
      integer :: nprhor = 0        ! gp for debug printout
      integer :: nentropy = 0      ! switch for entropy diagnostics

!
      real :: taunc         =  0.  ! time scale for newtonian cooling
      real :: xmind         = 0.1  ! minimal ice thickness (m)
      real :: xmaxd         = 9.0  ! maximal ice thickness (m; neg. = no limit)
!
!     global integer
!
      integer :: nicec2d    = 0    ! 1: compute thickness from cover
      integer :: nstep      = 0    ! time step
      integer :: nrestart   = 0    ! restart switch
!
!     global real
!
      real :: xdt           = 0.   ! timestep (sec.)
      real :: cicemin       = 0.5  ! minimum compactness to be ice
      real :: solar_day  = 86400.0 ! length of day [sec]
!
!     global arrays
!
      real :: xshfl(NHOR)     = 0.   ! surface sensible heat flx
      real :: xshdt(NHOR)     = 0.   ! derivative of shfl w.r.t. temp
      real :: xlhfl(NHOR)     = 0.   ! surface latent heat flx
      real :: xlhdt(NHOR)     = 0.   ! derivative of slfl w.r.t. temp
      real :: xswfl(NHOR)     = 0.   ! surface short wave radiation
      real :: xlwfl(NHOR)     = 0.   ! surface long wave radiation
!
      real :: xts(NHOR)       = 0.   ! surface temperature (K)
      real :: xsst(NHOR)      = 0.   ! sea surface temperature (K)
      real :: xmld(NHOR)      = 0.   ! mixed layer depth (m) (from ocean)
      real :: xls(NHOR)       = 0.   ! land sea mask (1/0)
      real :: xiced(NHOR)     = 0.   ! ice thickness (m)
      real :: xicec(NHOR)     = 0.   ! ice cover (1/0)
      real :: xsnow(NHOR)     = 0.   ! snow depth (h2o equiv. m)
      real :: xsmelt(NHOR)    = 0.   ! snow melt  (m/s water equiv.)
      real :: xsmflx(NHOR)    = 0.   ! flux for snow melt  (w/m2)
      real :: ximelt(NHOR)    = 0.   ! flux for ice melt/freeze  (w/m2)
      real :: xsndch(NHOR)    = 0.   ! snow depth change (m/s water equiv.)
      real :: xqmelt(NHOR)    = 0.   ! res. heat flux for ice melt (W/m^2)
      real :: xstoi(NHOR)     = 0.   ! snow converted to ice (m h2o)
!
      real :: xheat(NHOR)     = 0.  ! heat flux from atmosphere (W/m^2)
      real :: xcflux(NHOR)    = 0.  ! conductive heatflux through ice (W/m^2)
      real :: xcfluxf(NHOR)   = 0.  ! delta c-heatflux wrt tfreeze (W/m^2)
      real :: xcfluxr(NHOR)   = 0.  ! res. c-heatflux due to xmaxd (W/m^2)
      real :: xprs(NHOR)      = 0.  ! snow precipitation from atmosphere (m/s)
      real :: xpme(NHOR)      = 0.  ! fresh water flux (P-E only; m/s)
      real :: xroff(NHOR)     = 0.  ! runoff (m/s)
      real :: xtaux(NHOR)     = 0.  ! zonal wind stress (pa)
      real :: xtauy(NHOR)     = 0.  ! meridional wind stress (pa)
      real :: xust3(NHOR)     = 0.  ! ustar**3 (m**3/s**3)
      real :: xoflux(NHOR)    = 0.  ! (residual) heat flux from ocean (w/m2)
      real :: xtsflux(NHOR)   = 0.  ! flux to warm/cool ice/snow (w/2)
      real :: xhnew(NHOR)     = 0.  ! cond. heatflux after skintemp (w/m2)
      real :: xscflx(NHOR)    = 0.  ! flux from snow -> ice conversion (w/m2)
      real :: xgw(NHOR)       = 0.  ! gaussian weights
!
!     Climatological fields
!
      real :: xclsst(NHOR,0:13) =-999.! climatological sst
      real :: xclicec(NHOR,0:13)=-999.! climatological ice cover
      real :: xcliced(NHOR,0:13)=-999.! climatological ice thickness
      real :: xflxice(NHOR,0:13)= 0.! flux correction (W/m^2)
      real :: xclsst2(NHOR)   = 0.  ! climatological sst
      real :: xclicec2(NHOR)  = 0.  ! climatological ice cover
      real :: xcliced2(NHOR)  = 0.  ! climatological ice thickness
      real :: xflxice2(NHOR)  = 0.  ! flux correction (W/m^2)
!
!     fluxes for coupling to the ocean (accumulated)
!
      integer :: naccuo       = 0   ! counter for accumulation
      real :: cheat(NHOR)     = 0.  ! heat flux to the ocean (w/m2)
      real :: cpme(NHOR)      = 0.  ! fresh water flux (p-e; m/s)
      real :: croff(NHOR)     = 0.  ! runoff ( m/s)
      real :: ctaux(NHOR)     = 0.  ! zonal wind stress (pa)
      real :: ctauy(NHOR)     = 0.  ! meridional wind stress (pa)
      real :: cust3(NHOR)     = 0.  ! ustar**3 (m**3/s**)
      real :: csnow(NHOR)     = 0.  ! snow depth (m h2o eqv.)
!
!     accumulated diagnostics
!
      integer :: naccuout     = 0   ! counter for accumulation
      real :: xflxicea(NHOR)  = 0.  ! flux correction (w/m2)
      real :: xheata(NHOR)    = 0.  ! flux from the atmosphere (w/m2)
      real :: xofluxa(NHOR)   = 0.  ! flux from the ocean (w/m2)
      real :: xqmelta(NHOR)   = 0.  ! res flux to melt ice (w/m2)
      real :: xcfluxa(NHOR)   = 0.  ! flux to the ocean (w/m2)
      real :: xsmelta(NHOR)   = 0.  ! flux for snow melt (w/m2)
      real :: ximelta(NHOR)   = 0.  ! flux used for ice melt (w/m2)
      real :: xtsfluxa(NHOR)  = 0.  ! flux to warm/cool ice/snow (w/2)
      real :: xhnewa(NHOR)    = 0.  ! cond. heatflux after skintemp (w/m2)
      real :: xcpmea(NHOR)    = 0.  ! fresh water (p-e; m/s) for lsg
      real :: xcroffa(NHOR)   = 0.  ! runoff  (m/s) for lsg
      real :: xstoia(NHOR)    = 0.  ! snow converted to ice (m h2o)
      real :: xscflxa(NHOR)   = 0.  ! flux from snow -> ice conversion (w/m2)
! PBH
      real :: xflxice2a(NHOR,0:13) = 0.0 !accumulated flux correction for output (when nice=0)
!
!     entropy diagnostics
!
      real,allocatable :: xentro(:,:)

      real :: deglat(NLPP) = 0.0    ! latitude in degrees
!
!     Parallel Stuff
!
      integer :: mpinfo  = 0
      integer :: mypid   = 0
      integer :: myworld = 0
      integer :: nproc   = NPRO
!
      end module icemod


!     ===========================
!     SUBROUTINE READ_ICE_SURFACE
!     ===========================

      subroutine read_ice_surface
      use icemod
      implicit none

      real zmax
      integer jm

      call mpsurfgp('xls',xls,NHOR,1)

      call mpsurfgp('xclsst' ,xclsst ,NHOR,14)
      call mpsurfgp('xclicec',xclicec,NHOR,14)
      call mpsurfgp('xcliced',xcliced,NHOR,14)

!     make sure, that land sea mask values are 0 or 1

      where (xls(:) > 0.5)
         xls(:) = 1.0
      elsewhere
         xls(:) = 0.0
      endwhere

      call mpmaxval(xclicec,NHOR,14,zmax)
      if (zmax > 5.0) then
         xclicec(:,:) = xclicec(:,:) * 0.01
         if (mypid == NROOT) &
         write (*,*) 'ice cover {xclicec} converted from % to fraction'
      endif

      if (zmax < 0.0) then ! xclicec was not read
         xclicec(:,:) = 0.0
         where (xclsst(:,:) <= TFREEZE) xclicec(:,:) = 1.0
         if (mypid == NROOT) &
         write (*,*) 'ice cover {xclicec} constructed from SST'
      endif

      if (xcliced(1,1) < 0.0) then ! xcliced was not read
         nicec2d = 1
         call make_ice_thickness
         if (mypid == NROOT) &
         write (*,*) 'ice thickness {xcliced} computed from ice cover'
      endif

!     correct climatological ice with land-sea mask

      do jm = 0 , 13
         where (xls(:) >= 1.0)
            xclicec(:,jm) = 0.0
            xcliced(:,jm) = 0.0
         endwhere
      enddo
      return
      end subroutine read_ice_surface


!     =================
!     SUBROUTINE ICEINI
!     =================

      subroutine iceini(kstep,krestart,koutput,kdpy,pts,psst,pmld  &
     &                 ,picec,piced,psnow,ktspd,psolday,pdeglat)
      use icemod
      implicit none

      integer kstep,krestart,koutput,ktspd,kdpy
      real psolday
      integer jlat
!
      real :: pts(NHOR)
      real :: psst(NHOR)
      real :: pmld(NHOR)
      real :: picec(NHOR)
      real :: piced(NHOR)
      real :: psnow(NHOR)
      real :: pdeglat(NLPP)
      real (kind=8) :: zsi(NLAT)
      real (kind=8) :: zgw(NLAT)
      real :: zgw2(NLON,NLAT)

! PBH added nflukoavg
      namelist/icepar/nout,nfluko,nperpetual_ice,ntspd,nprint,nprhor    &
     &               ,nentropy,nice,nsnow,ntskin,ncpl_ice_ocean,taunc   &
     &               ,xmind,xmaxd,newsurf,nflukoavg
!
!     copy input parameter to icemod
!
      nstep     = kstep
      nrestart  = krestart
      noutput   = koutput
      ntspd     = ktspd
      solar_day = psolday
      deglat(:) = pdeglat(:)

!     compute grids properties
!
      if(mypid == NROOT) then
       call inigau_plasim(NLAT,zsi,zgw)
       do jlat=1,NLAT
        zgw2(:,jlat)=zgw(jlat)
       enddo
      endif
      call mpscgp(zgw2,xgw,1)
!
!     get process id
!
      call mpi_info(nproc,mypid)
!
!     print version number and read namelist
!
      if (mypid == NROOT) then
         open(20,file=trim(runtime_root)//'/genie-plasim/config/ice_namelist',form='formatted')
         read(20,icepar)
         write (*,'(/," ***************************************")')
         write (*,'(" * ICEMOD ",a28," *")') trim(version)
         write (*,'(" ***************************************")')
         write (*,'(" * Namelist ICEPAR from <ice_namelist> *")')
         write (*,'(" ***************************************")')
         write(*,icepar)
         close(20)
      endif

      call mpbci(nout)
      call mpbci(nfluko)
      call mpbci(nice)
      call mpbci(newsurf)
      call mpbci(nsnow)
      call mpbci(ntskin)
      call mpbci(ntspd)
      call mpbci(nperpetual_ice)
      call mpbci(ncpl_ice_ocean)
      call mpbci(nprint)
      call mpbci(nprhor)
      call mpbci(nentropy)
      call mpbcr(taunc)
      call mpbcr(xmind)
      call mpbcr(xmaxd)
!
!     set time step
!
      xdt   = solar_day / real(ntspd)
      taunc = solar_day * taunc

      if (nrestart == 0) then ! read start file
       
         call read_ice_surface
!
!        initialize
!
         call iceget
         xiced(:)=xcliced2(:)

         where (xiced(:) > xmind .and. xls(:) < 1.0)
            xicec(:)=1.0
         elsewhere
            xicec(:)=0.0
         endwhere

      else ! (nrestart /= 0)
!
!        restart from restart file
!
         if (mypid == NROOT) then
            call get_restart_integer('nstep',nstep)
            call get_restart_integer('naccuice',naccuout)
            call get_restart_integer('naccuo',naccuo)
         endif
  
         call mpbci(nstep)
         call mpbci(naccuout)
         call mpbci(naccuo)
  
         call mpgetgp('xls'     ,xls     ,NHOR, 1)
         call mpgetgp('xts'     ,xts     ,NHOR, 1)
         call mpgetgp('xicec'   ,xicec   ,NHOR, 1)
         call mpgetgp('xiced'   ,xiced   ,NHOR, 1)
         call mpgetgp('xsnow'   ,xsnow   ,NHOR, 1)
         call mpgetgp('cheat'   ,cheat   ,NHOR, 1)
         call mpgetgp('cpme'    ,cpme    ,NHOR, 1)
         call mpgetgp('croff'   ,croff   ,NHOR, 1)
         call mpgetgp('ctaux'   ,ctaux   ,NHOR, 1)
         call mpgetgp('ctauy'   ,ctauy   ,NHOR, 1)
         call mpgetgp('cust3'   ,cust3   ,NHOR, 1)
         call mpgetgp('csnow'   ,csnow   ,NHOR, 1)
         call mpgetgp('xflxicea',xflxicea,NHOR, 1)
         call mpgetgp('xheata'  ,xheata  ,NHOR, 1)
         call mpgetgp('xofluxa' ,xofluxa ,NHOR, 1)
         call mpgetgp('xqmelta' ,xqmelta ,NHOR, 1)
         call mpgetgp('xcfluxa' ,xcfluxa ,NHOR, 1)
         call mpgetgp('xsmelta' ,xsmelta ,NHOR, 1)
         call mpgetgp('ximelta' ,ximelta ,NHOR, 1)
         call mpgetgp('xtsfluxa',xtsfluxa,NHOR, 1)
         call mpgetgp('xhnewa'  ,xhnewa  ,NHOR, 1)
         call mpgetgp('xscflxa' ,xscflxa ,NHOR, 1)
         call mpgetgp('xcpmea'  ,xcpmea  ,NHOR, 1)
         call mpgetgp('xcroffa' ,xcroffa, NHOR, 1)
         call mpgetgp('xstoia'  ,xstoia  ,NHOR, 1)

         if (newsurf == 1) then 
            call read_ice_surface
         else
            call mpgetgp('xclicec' ,xclicec ,NHOR,14)
            call mpgetgp('xcliced' ,xcliced ,NHOR,14)
            call mpgetgp('xclsst'  ,xclsst  ,NHOR,14)
         endif ! (newsurf == 1)
      endif ! (nrestart == 0)
!
!     read flux correction
!
      if (nfluko == 1) then
         call mpsurfgp('xflxice',xflxice,NHOR,14)
      endif
!
!     open output file
!
      if (mypid == NROOT) then
         open  (71,file=trim(outdir_name)//'/ice_output',form='unformatted')
      endif ! (mypid == NROOT)
!
!     initialize ocean
!
      call oceanini(nstep,nrestart,noutput,kdpy,xsst,xmld,xoflux   &
     &             ,ntspd,solar_day)
!
!     initialize skintemperature
!
      if (nrestart == 0) xts(:)=xsst(:)
!
!     copy output from icemod
!
      pts(:)=xts(:)
      psst(:)=xsst(:)
      pmld(:)=xmld(:)
      picec(:)=xicec(:)
      piced(:)=xiced(:)
      psnow(:)=xsnow(:)
!
      if(nentropy > 0) then
       allocate(xentro(NHOR,1))
       xentro(:,:)=0.
      endif
!
      return
      end subroutine iceini

!     =====================================================================
!     SUBROUTINE icestep
!     =====================================================================

      subroutine icestep(pheat,pshfl,pshdt,plhfl,plhdt,plwfl,pswfl      &
     &                  ,ppme,proff,pprs,ptaux,ptauy,pust3              &
     &                  ,pts,picec,piced,psnow,psmelt,psndch,psst,pmld  &
     &                  ,ngenie)
      use icemod
      implicit none

      integer ngenie

      real zrhoilfdt
      integer imin,ihou,iday,imon,iyea
!
      real :: pheat(NHOR),pshfl(NHOR),pshdt(NHOR),plhfl(NHOR),plhdt(NHOR)
      real :: plwfl(NHOR),pswfl(NHOR),ppme(NHOR),proff(NHOR),pprs(NHOR)
      real :: ptaux(NHOR),ptauy(NHOR),pust3(NHOR),pts(NHOR),picec(NHOR)
      real :: piced(NHOR),psnow(NHOR),psmelt(NHOR),psndch(NHOR),psst(NHOR)
      real :: pmld(NHOR)
!
      real :: zsnowold(NHOR),zicedold(NHOR),zcflux(NHOR)
      real :: zsum(2)
!
!     debug arrays
!
      real,allocatable :: zprf1(:),zprf2(:),zprf3(:),zprf4(:)
!PBH
      integer idum
!
!     set some helpful bits
!
      zrhoilfdt=CRHOI*CLFI/xdt
!
!     reset arrays
!
      ximelt(:)=0.
      xsmelt(:)=0.
      xsmflx(:)=0.
      xtsflux(:)=0.
      xhnew(:)=0.
      xscflx(:)=0.
      xcfluxr(:)=0.
!
!     copy input to icemod
!
      xheat(:)=pheat(:)
      xshfl(:)=pshfl(:)
      xshdt(:)=pshdt(:)
      xlhfl(:)=plhfl(:)
      xlhdt(:)=plhdt(:)
      xlwfl(:)=plwfl(:)
      xswfl(:)=pswfl(:)
      xpme(:)=ppme(:)
      xroff(:)=proff(:)
      xprs(:)=pprs(:)
      xtaux(:)=ptaux(:)
      xtauy(:)=ptauy(:)
      xust3(:)=pust3(:)
!
!     get climatology
!
      call iceget
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       allocate(zprf4(NLON*NLAT))
       call mpgagp(zprf1,xheat,1)
       call mpgagp(zprf2,xclicec2,1)
       call mpgagp(zprf3,xcliced2,1)
       call mpgagp(zprf4,xsst,1)
       if(mypid==NROOT) then
        print*,'In icestep:'
        print*,'sst (old time step): ',zprf4(nprhor)
        print*,'heatflx from atm: ',zprf1(nprhor)
        print*,'clim. ice c and d: ',zprf2(nprhor),zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
       deallocate(zprf4)
      endif
!
      where(xls(:) < 0.5)
       zsnowold(:)=xsnow(:)
       zicedold(:)=xiced(:)
      endwhere
!
!     add new snow
!
      if(nsnow==1) then
       where(xicec(:) >= cicemin) 
        xsnow(:)=xsnow(:)+xprs(:)*xdt
        xprs(:)=0.
       endwhere
      endif
!
!     compute surface temperature
!
      if(ntskin==1) then
       call skintemp
      else
       where(xiced(:)+1.E3/CRHOSN*xsnow(:) >= 0.1)
        xts(:)=xclsst2(:)
       elsewhere
        xts(:)=xsst(:)
       endwhere
      endif
!
!     make conductive heatflux
!
      call mkcflux
!
!     make new snow (alter conductive heatflux if neccessary)
!
      if(nsnow==1) then
       call subsnow
      else
       xsnow(:)=0.
      endif
!
!     make new ice
!
      call mkice
!
      if(nice == 0) then
!
!     climatological ice and 
!     flux correction diagnostics
!
       where(xiced(:) > 0. .or. xcliced2(:) > 0.)
        xflxice2(:)=(xiced(:)-xcliced2(:))*zrhoilfdt-xcflux(:)
        xcflux(:)=0.                    
        xiced(:)=xcliced2(:)
       elsewhere
        xflxice2(:)=0.
       endwhere
!
! PBH
       call ntomin(nstep,imin,ihou,iday,imon,iyea)
       xflxice2a(:,imon)=xflxice2a(:,imon)+xflxice2(:)
! output flux correction every nflukoavg timesteps
       if(mod(nstep,nflukoavg)==0) call outflux_ice
! end PBH

      else
!
!     add flux corrction (if switched on)
!
       if(nfluko == 0) then
        xflxice2(:)=0.
       elseif(nfluko == 1) then
        call getflx
        call addfci
       elseif(nfluko == 2) then
        call mkflukoi
        call addfci
       endif
!
      endif

!PBH HACK TO FIX ANTICE
      do idum=NHOR/2+1,NHOR 
       zcflux(idum)=(xcliced2(idum)-xiced(idum))*zrhoilfdt
       xiced(idum)=xcliced2(idum)
       xcfluxr(idum)=xcfluxr(idum)+zcflux(idum)
      enddo
!IF NO MAX ICE THICKNESS THEN NEED TO CORRECT HEAT CONDUCTION HERE.
!OTHERWISE IT IS DONE BELOW 
      if(xmaxd<0.) then
       zsum(1)=SUM(xcfluxr(:)*xgw(:),MASK=(xls < 1.)) 
       zsum(2)=SUM(xgw(:),MASK=(xls(:) < 1.)) 
       call mpsumbcr(zsum,2)
       if(zsum(1) /= 0.) then 
        where(xls(:) < 1.)
         xcflux(:)=xcflux(:)+zsum(1)/zsum(2)
        end where
       endif
      endif
! END PBH HACK

!
!     correct sea ice to a maximum of 1.1*xmaxd 
!     get the needed hflx from the global ocean/ice 
!
      if(xmaxd >= 0.) then 
       zcflux(:)=0.
       where(xiced(:) > 1.1*xmaxd)
        zcflux(:)=(1.1*xmaxd-xiced(:))*zrhoilfdt
        xiced(:)=1.1*xmaxd
        xcfluxr(:)=xcfluxr(:)+zcflux(:)
       end where
       zsum(1)=SUM(xcfluxr(:)*xgw(:),MASK=(xls < 1.)) 
       zsum(2)=SUM(xgw(:),MASK=(xls(:) < 1.)) 
       call mpsumbcr(zsum,2)
       if(zsum(1) /= 0.) then 
        where(xls(:) < 1.)
         xcflux(:)=xcflux(:)+zsum(1)/zsum(2)
        end where
       endif
      endif
!
!     depug print out if needed
!
      if (nprint==2) then 
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       call mpgagp(zprf1,zcflux,1)
       call mpgagp(zprf2,xiced,1)
       if(mypid==NROOT) then 
        print*,'limit ice thickness (if > xmaxd):'
        print*,'iced: ',zprf2(nprhor)
        print*,'heat for correction: ',zprf1(nprhor)
        print*,'global average added to cflux: ',zsum(1)/zsum(2)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
      endif
!
!     set sea ice compactness according to thickness
!
       where(xiced(:) > xmind)
        xicec(:)=1.
       elsewhere
        xicec(:)=0.
       end where

!GENIE COUPLING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if(ngenie.ge.1) then
        xiced(:)=genie_hght_sic(:)
        xicec(:)=genie_frac_sic(:)
        xts(:)=xicec(:)*genie_icet(:)+(1.0-xicec(:))*genie_sst(:)
       endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!     correct snow with new ice mask (ice was melted below snow)
!     and melt snow (modify heat flux)
!
      where(xicec(:) < cicemin .and. xsnow(:) > 0.)
       xsmelt(:)=xsmelt(:)+xsnow(:)/xdt
       xsmflx(:)=xsmflx(:)+xsnow(:)*1000.*CLFSN/xdt
       xcflux(:)=xcflux(:)-xsnow(:)*1000.*CLFSN/xdt
       xsnow(:)=0.
      end where
!
!     correct wind stress and ustar**3 according to ice
!
!!      where(xicec >= cicemin)
!!       xtaux(:)=0.
!!       xtauy(:)=0.
!!       xust3(:)=0.
!!      end where
!
!     possible input for other ocean models? (correct for ice/snow)
!
!     correct fresh water flux into ocean due to ice/snow changes
!
!     where(xls(:) < 0.5) 
!      xpme(:)=xpme(:)                                                  &
!    &        -(xsnow(:)-zsnowold(:))/xdt                               &
!    &        -(xiced(:)-zicedold(:))/xdt*CRHOI/1.E3
!     endwhere
!
!     output
!
!     accumulate
!
      xflxicea(:)=xflxicea(:)+xflxice2(:)
      xheata(:)=xheata(:)+xheat(:)
      xofluxa(:)=xofluxa(:)+xoflux(:)
      xqmelta(:)=xqmelta(:)+xqmelt(:)
      ximelta(:)=ximelta(:)+ximelt(:)
      xsmelta(:)=xsmelta(:)+xsmflx(:)
      xcfluxa(:)=xcfluxa(:)+xcflux(:)
      xtsfluxa(:)=xtsfluxa(:)+xtsflux(:)
      xhnewa(:)=xhnewa(:)+xhnew(:)
      xscflxa(:)=xscflxa(:)+xscflx(:)
      xcpmea(:)=xcpmea(:)+xpme(:)
      xcroffa(:)=xcroffa(:)+xroff(:)
      xstoia(:)=xstoia(:)+xstoi(:)
      naccuout=naccuout+1
!
!     write out
!
      if(nout > 0) then
      if(mod(nstep,nout) == 0) then
       xflxicea(:)=xflxicea(:)/REAL(naccuout)
       xheata(:)=xheata(:)/REAL(naccuout)
       xofluxa(:)=xofluxa(:)/REAL(naccuout)
       xqmelta(:)=xqmelta(:)/REAL(naccuout)
       xcfluxa(:)=xcfluxa(:)/REAL(naccuout)
       ximelta(:)=ximelta(:)/REAL(naccuout)
       xsmelta(:)=xsmelta(:)/REAL(naccuout)
       xtsfluxa(:)=xtsfluxa(:)/REAL(naccuout)
       xhnewa(:)=xhnewa(:)/REAL(naccuout)
       xscflxa(:)=xscflxa(:)/REAL(naccuout)
       xcpmea(:)=xcpmea(:)/REAL(naccuout)
       xcroffa(:)=xcroffa(:)/REAL(naccuout)
       xstoia(:)=xstoia(:)/REAL(naccuout)
       if(noutput > 0) call iceout
       xflxicea(:)=0.
       xheata(:)=0.
       xofluxa(:)=0.
       xqmelta(:)=0.
       xcfluxa(:)=0.
       ximelta(:)=0.
       xsmelta(:)=0.
       xtsfluxa(:)=0.
       xhnewa(:)=0.
       xscflxa(:)=0.
       xcpmea(:)=0.
       xcroffa(:)=0.
       xstoia(:)=0.
       naccuout=0
      endif
      endif
!
!     ocean coupling
!
!     accumulate fluxes
!
      where(xls(:) < 0.5)
       cheat(:)=cheat(:)+xcflux(:)
       cpme(:)=cpme(:)+xpme(:)
       croff(:)=croff(:)+xroff(:)
       ctaux(:)=ctaux(:)+xtaux(:)
       ctauy(:)=ctauy(:)+xtauy(:)
       cust3(:)=cust3(:)+xust3(:)
       csnow(:)=csnow(:)+xsnow(:)
      end where
      naccuo=naccuo+1
      if(mod(nstep,ncpl_ice_ocean) == 0) then
       where(xls(:) < 0.5)
        cheat(:)=cheat(:)/real(naccuo)
        cpme(:)=cpme(:)/real(naccuo)
        croff(:)=croff(:)/real(naccuo)
        ctaux(:)=ctaux(:)/real(naccuo)
        ctauy(:)=ctauy(:)/real(naccuo)
        cust3(:)=cust3(:)/real(naccuo)
        csnow(:)=csnow(:)/real(naccuo)
       endwhere

       call oceanstep(xicec,xiced,cheat,cpme,croff,ctaux,ctauy,cust3    &
     &               ,csnow,xsst,xmld,xoflux,xcliced2,ngenie)
!
       where(xls(:) < 0.5)
        cheat(:)=0.
        cpme(:)=0.
        croff(:)=0.
        ctaux(:)=0.
        ctauy(:)=0.
        cust3(:)=0.
        csnow(:)=0.
       end where
       naccuo=0
!
      endif
!
!     snow diagnostics
!
      where(xls(:) < 0.5) xsndch(:)=(xsnow(:)-zsnowold(:))/xdt
!
!     copy output from icemod
!
      pts(:)=xts(:)
      picec(:)=xicec(:)
      piced(:)=xiced(:)
      psnow(:)=xsnow(:)
      psmelt(:)=xsmelt(:)
      psndch(:)=xsndch(:)
      psst(:)=xsst(:)
      pmld(:)=xmld(:)
!
!     advance time step
!
      nstep=nstep+1
!
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       allocate(zprf4(NLON*NLAT))
       call mpgagp(zprf1,xoflux,1)
       call mpgagp(zprf2,xicec,1)
       call mpgagp(zprf3,xiced,1)
       call mpgagp(zprf4,xsst,1)
       if(mypid==NROOT) then
        print*,'final ice c and d to atm and oce: ',zprf2(nprhor),zprf3(nprhor)
        print*,'heatflx and sst from ocean: ',zprf1(nprhor),zprf4(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
       deallocate(zprf4)
      endif
!
      return
      end subroutine icestep

!     ===================================
!     SUBROUTINE ICE_REST
!     ===================================

      subroutine ice_rest
      use icemod
      implicit none

      if (mypid == NROOT) then
         call put_restart_integer('naccuice',naccuout)
         call put_restart_integer('naccuo',naccuo)
      endif

      call mpputgp('xls'     ,xls     ,NHOR, 1)
      call mpputgp('xts'     ,xts     ,NHOR, 1)
      call mpputgp('xicec'   ,xicec   ,NHOR, 1)
      call mpputgp('xiced'   ,xiced   ,NHOR, 1)
      call mpputgp('xsnow'   ,xsnow   ,NHOR, 1)
      call mpputgp('xclicec' ,xclicec ,NHOR,14)
      call mpputgp('xcliced' ,xcliced ,NHOR,14)
      call mpputgp('xclsst'  ,xclsst  ,NHOR,14)
      call mpputgp('cheat'   ,cheat   ,NHOR, 1)
      call mpputgp('cpme'    ,cpme    ,NHOR, 1)
      call mpputgp('croff'   ,croff   ,NHOR, 1)
      call mpputgp('ctaux'   ,ctaux   ,NHOR, 1)
      call mpputgp('ctauy'   ,ctauy   ,NHOR, 1)
      call mpputgp('cust3'   ,cust3   ,NHOR, 1)
      call mpputgp('csnow'   ,csnow   ,NHOR, 1)
      call mpputgp('xflxicea',xflxicea,NHOR, 1)
      call mpputgp('xheata'  ,xheata  ,NHOR, 1)
      call mpputgp('xofluxa' ,xofluxa ,NHOR, 1)
      call mpputgp('xqmelta' ,xqmelta ,NHOR, 1)
      call mpputgp('xcfluxa' ,xcfluxa ,NHOR, 1)
      call mpputgp('xsmelta' ,xsmelta ,NHOR, 1)
      call mpputgp('ximelta' ,ximelta ,NHOR, 1)
      call mpputgp('xtsfluxa',xtsfluxa,NHOR, 1)
      call mpputgp('xhnewa'  ,xhnewa  ,NHOR, 1)
      call mpputgp('xscflxa' ,xscflxa ,NHOR, 1)
      call mpputgp('xcpmea'  ,xcpmea  ,NHOR, 1)
      call mpputgp('xcroffa' ,xcroffa ,NHOR, 1)
      call mpputgp('xstoia'  ,xstoia  ,NHOR, 1)
!
      call ocean_rest
!
      return
      end subroutine ice_rest

!     ===================================
!     SUBROUTINE ICESTOP
!     ===================================

      subroutine icestop
      use icemod
      implicit none
!
!     close output file
!
      if (mypid == NROOT) then
       close(71)
      endif
!
!     write restart file
!
      if (mypid == NROOT) then
         call put_restart_integer('naccuice',naccuout)
         call put_restart_integer('naccuo',naccuo)
      endif

      call mpputgp('xls'     ,xls     ,NHOR, 1)
      call mpputgp('xts'     ,xts     ,NHOR, 1)
      call mpputgp('xicec'   ,xicec   ,NHOR, 1)
      call mpputgp('xiced'   ,xiced   ,NHOR, 1)
      call mpputgp('xsnow'   ,xsnow   ,NHOR, 1)
      call mpputgp('xclicec' ,xclicec ,NHOR,14)
      call mpputgp('xcliced' ,xcliced ,NHOR,14)
      call mpputgp('xclsst'  ,xclsst  ,NHOR,14)
      call mpputgp('cheat'   ,cheat   ,NHOR, 1)
      call mpputgp('cpme'    ,cpme    ,NHOR, 1)
      call mpputgp('croff'   ,croff   ,NHOR, 1)
      call mpputgp('ctaux'   ,ctaux   ,NHOR, 1)
      call mpputgp('ctauy'   ,ctauy   ,NHOR, 1)
      call mpputgp('cust3'   ,cust3   ,NHOR, 1)
      call mpputgp('csnow'   ,csnow   ,NHOR, 1)
      call mpputgp('xflxicea',xflxicea,NHOR, 1)
      call mpputgp('xheata'  ,xheata  ,NHOR, 1)
      call mpputgp('xofluxa' ,xofluxa ,NHOR, 1)
      call mpputgp('xqmelta' ,xqmelta ,NHOR, 1)
      call mpputgp('xcfluxa' ,xcfluxa ,NHOR, 1)
      call mpputgp('xsmelta' ,xsmelta ,NHOR, 1)
      call mpputgp('ximelta' ,ximelta ,NHOR, 1)
      call mpputgp('xtsfluxa',xtsfluxa,NHOR, 1)
      call mpputgp('xhnewa'  ,xhnewa  ,NHOR, 1)
      call mpputgp('xscflxa' ,xscflxa ,NHOR, 1)
      call mpputgp('xcpmea'  ,xcpmea  ,NHOR, 1)
      call mpputgp('xcroffa' ,xcroffa ,NHOR, 1)
      call mpputgp('xstoia'  ,xstoia  ,NHOR, 1)
!
!     finalize ocean
!
      call oceanstop
!
      if(nentropy > 0) then
       deallocate(xentro)
      endif
!
      return
      end subroutine icestop

!     =====================================================================
!     SUBROUTINE mkice
!     =====================================================================

      subroutine mkice
      use icemod
      implicit none

      real zrhoilfdt
!
!     debug arrays
!
      real, allocatable :: zprf1(:),zprf2(:),zprf3(:)
!
      zrhoilfdt=CRHOI*CLFI/xdt
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       call mpgagp(zprf1,xcflux,1)
       call mpgagp(zprf2,xoflux,1)
       if(mypid==NROOT) then
        print*,'In mkice:'
        print*,'conductive hf and oce fl: ',zprf1(nprhor),zprf2(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
      endif
!
!     add flux from ocean
!
      where(xls(:) < 0.5) 
       xcflux(:)=xcflux(:)+xoflux(:)
      endwhere
!
!     make new ice
!
      where(xsst(:) <= TFREEZE .or. xiced(:) > 0.)               
!
!      add flux due to snow conversion
!
       ximelt(:)=xcflux(:)+xscflx(:)-xcfluxf(:)
       xiced(:)=xiced(:)-ximelt(:)/zrhoilfdt
!
!     reset heat flux 
!     (if ice > 0 and SST > TFREEZE (eg different climatologies)
!     use part of the conductive heat flux to cool the ocean)
!
       xcflux(:)=xcfluxf(:)
!
      end where
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,xcflux,1)
       call mpgagp(zprf2,ximelt,1)
       call mpgagp(zprf3,xiced,1)
       if(mypid==NROOT) then
        print*,'new ice and flux for ice: ',zprf3(nprhor),zprf2(nprhor)
        print*,'new (residual) conductive hf: ',zprf1(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
!     correct negative ice thickness (warm ocean)
!
      where(xiced(:) <= 0.)
       xcflux(:)=xcflux(:)-xiced(:)*zrhoilfdt
       ximelt(:)=ximelt(:)+xiced(:)*zrhoilfdt
       xiced(:)=0.
      end where
!
!     set infinitisimal sea ice to zero
!
      where(ABS(xiced(:)) < 1.E-9) xiced(:)=0.
!
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,xcflux,1)
       call mpgagp(zprf2,ximelt,1)
       call mpgagp(zprf3,xiced,1)
       if(mypid==NROOT) then
        print*,'final ice: ',zprf3(nprhor)
        print*,'final flux for ice: ',zprf2(nprhor)
        print*,'final conductive hf: ',zprf1(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
      return
      end subroutine mkice

!     =====================================================================
!     SUBROUTINE subsnow
!     =====================================================================

      subroutine subsnow
      use icemod
      implicit none

      real zrhosls,zrhoilfdt
      integer jhor
!
      real :: zhice(NHOR) = 0. ! new ice thickness
      real :: zdice(NHOR) = 0. ! ice thickness change due to snow conversion
      real :: zdsnow(NHOR)= 0. ! snow depth change due to snow conversion
!
!     dbug arrays
!
      real,allocatable :: zprf1(:),zprf2(:),zprf3(:),zprf4(:),zprf5(:)
!
      zrhosls=CRHOSN*CLFSN
      zrhoilfdt=CRHOI*CLFI/xdt
!
!     snow melt
!
      do jhor=1,NHOR
       if(xicec(jhor) >= cicemin) then
        if(xqmelt(jhor) > (1.E3/CRHOSN)*xsnow(jhor)*zrhosls/xdt) then
         xsmelt(jhor)=xsmelt(jhor)+xsnow(jhor)/xdt
         xsmflx(jhor)=xsmflx(jhor)+xsnow(jhor)*1000.*CLFSN/xdt
         xqmelt(jhor)=xqmelt(jhor)-xsnow(jhor)*1000.*CLFSN/xdt
         xsnow(jhor)=0.
        elseif(xqmelt(jhor) > 0. .and. xsnow(jhor) > 0.) then
         xsmelt(jhor)=xsmelt(jhor)+CRHOSN/1.E3*xqmelt(jhor)/zrhosls
         xsnow(jhor)=xsnow(jhor)-CRHOSN/1.E3*xqmelt(jhor)/zrhosls*xdt
         xsmflx(jhor)=xsmflx(jhor)+xqmelt(jhor)
         xqmelt(jhor)=0.
        endif
!
!     add residual flux to conductive heat flux and reset qmelt
!
        xcflux(jhor)=xcflux(jhor)+xqmelt(jhor)
!
       else
!
!       snow falls into water
!
        xsmelt(jhor)=xsmelt(jhor)+xsnow(jhor)/xdt
        xcflux(jhor)=xcflux(jhor)-xsnow(jhor)*1000.*CLFSN/xdt
        xsmflx(jhor)=xsmflx(jhor)+xsnow(jhor)*1000.*CLFSN/xdt
        xsnow(jhor)=0.
!
       endif
      enddo
!
!     dbug print out
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       allocate(zprf4(NLON*NLAT))
       call mpgagp(zprf1,xsmelt,1)
       call mpgagp(zprf2,xsnow,1)
       call mpgagp(zprf3,xsmflx,1)
       call mpgagp(zprf4,xcflux,1)
       if(mypid==NROOT) then
        print*,'in subsnow:'
        print*,'snow melt, and flx used for melting: ',zprf1(nprhor)    &
     &        ,zprf3(nprhor)
        print*,'new snow: ',zprf2(nprhor)
        print*,'new conductive heat flux: ',zprf4(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
       deallocate(zprf4)
      endif
!
!     convert snow to sea ice if snow/ice interface is below sea level:
!     (note: xsnow = h2o equiv.) the new ice is build in mkice via the
!     flux xscflx (note: this conversion conserves mass
!     but not energy. therefor, an additional flux is added to xscflx
!     in order to build additional ice:
!     xscflx(:)=zdice(:)*CRHOI*(CLFI-CLFSN)/xdt
!
      xstoi(:)=0.0

      where(xls(:) < 0.5 .and. 1.E3*xsnow(:) > (CRHOS-CRHOI)*xiced(:))
       zhice(:)=(1.E3*xsnow(:)+CRHOI*xiced(:))/CRHOS
       zdsnow(:)=(CRHOS-CRHOI)*zhice(:)/1.E3-xsnow(:)
       zdice(:)=zhice(:)-xiced(:)
       xsnow(:)=(CRHOS-CRHOI)*zhice(:)/1.E3
       xscflx(:)=zdice(:)*CRHOI*(CLFI-CLFSN)/xdt
!
!     diagnose ice thickness change from snow conversion
!     (to correct P-E in LSG-coupling)
!
       xstoi(:)=(zhice(:)-xiced(:))*CRHOI/1000./xdt
      endwhere
! 
!     no ice growth if ciced > xmaxd
!
      if (xmaxd >= 0.) then
       where(xiced(:) > xmaxd)
        xscflx(:)=0.
        xstoi(:)=0.
       endwhere
      endif
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       allocate(zprf4(NLON*NLAT))
       allocate(zprf5(NLON*NLAT))
       call mpgagp(zprf1,zdice,1)
       call mpgagp(zprf2,zdsnow,1)
       call mpgagp(zprf3,xiced,1)
       call mpgagp(zprf4,xsnow,1)
       call mpgagp(zprf5,xscflx,1)
       if(mypid==NROOT) then
        print*,'snow change by snow -> ice conv.: ',zprf2(nprhor)
        print*,'ice change by snow -> ice conv.: ',zprf1(nprhor)
        print*,'new snow: ',zprf4(nprhor),' new ice ',zprf3(nprhor)
        print*,'residual flux (to build add. ice): ',zprf5(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
       deallocate(zprf4)
       deallocate(zprf5)
      endif
!
      return
      end subroutine subsnow

!     =====================================================================
!     SUBROUTINE MKCFLUX
!     =====================================================================

      subroutine mkcflux
      use icemod
      implicit none
      real zrhosls
!
      real :: zckap(NHOR)
      real :: zhsnow(NHOR)
!
!     debug arrays
!
      real, allocatable :: zprf1(:),zprf2(:),zprf3(:)
!
      zrhosls=CRHOSN*CLFSN
!
!     compute conductive heat flux
!
      where(xls(:) < 0.5) 
       xcflux(:)=xheat(:)
       xcfluxf(:)=0.
      end where
      where(xicec(:) >= cicemin)
       zhsnow(:)=1.E3/CRHOSN*xsnow(:)
       zckap(:)=(CKAPSN*zhsnow(:)+CKAPI*xiced(:))/(zhsnow(:)+xiced(:))
      endwhere
!
!     limit ice thickness 
!
      if(xmaxd >= 0.) then
       where(xiced(:) > xmaxd)
        zckap(:)=0.
       endwhere
      endif
!
      where(xicec(:) >= cicemin) 
       xcflux(:)=zckap(:)*(xts(:)-xsst(:))/(zhsnow(:)+xiced(:))
       xcfluxf(:)=zckap(:)*(TFREEZE-xsst(:))/(zhsnow(:)+xiced(:))
      end where
!
      if(nfluko > 0. .or. nice == 0) xcfluxf(:)=0.
!
!     melt snow falling into water (cool water, energy balance)
!      
      where(xicec(:) < cicemin .and. xprs(:) > 0.)
       xcflux(:)=xcflux(:)-xprs(:)*1000.*CLFSN
       xsmelt(:)=xprs(:)+xsmelt(:)
       xsmflx(:)=xsmflx(:)+zrhosls*(1.E3/CRHOSN)*xprs(:)
       xprs(:)=0.
      endwhere
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,zhsnow,1)
       call mpgagp(zprf2,zckap,1)
       call mpgagp(zprf3,xcflux,1)
       if(mypid==NROOT) then
        print*,'in mkcflux: '
        print*,'snow depth (in m snow): ',zprf1(nprhor)
        print*,'kappa and conductive hf: ',zprf2(nprhor),zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
      return
      end subroutine mkcflux

!     =====================================================================
!     SUBROUTINE OUTFLUX
!     =====================================================================
!PBH
      subroutine outflux_ice
! outputs ice flux correction
      use icemod
      implicit none
      integer nmin,nhour,nday,nmonth,nyear
      integer i

      integer :: ih(8)

      call ntomin(nstep,nmin,nhour,nday,nmonth,nyear)

      ih(1) = 709
      ih(2) = 0
      ih(3) = nyear*10000 +nmonth*100 + nday
      ih(4) = nhour*100 + nmin
      ih(5) = NLON
      ih(6) = NLAT
      ih(7) = 0
      ih(8) = 0

! calculate average
      xflxice2a(:,:)=xflxice2a(:,:)*12.0/real(nflukoavg)
! copy january (to 13) and december (to 0)
      xflxice2a(:,0)=xflxice2a(:,12)
      xflxice2a(:,13)=xflxice2a(:,1)

!temporary path and filename (generalise this)
      IF(NLAT.EQ.32) THEN
        open(10,file=trim(runtime_root)//'/genie-plasim/data/input/T21/N032_surf_0709.sra',form='formatted')
      ELSE IF(NLAT.EQ.64) THEN
        open(10,file=trim(runtime_root)//'/genie-plasim/data/input/T42/N064_surf_0709.sra',form='formatted')
      ELSE
        print*,"look at outflux_ice"
        STOP
      ENDIF


      do i=0,13
       write(10,*) ih
       write(10,*) xflxice2a(:,i)
      enddo
      close(10)

!reset accumulated flux
      xflxice2a(:,:)=0.0

      return
      end


!     =====================================================================
!     SUBROUTINE iceout
!     =====================================================================

      subroutine iceout
      use icemod
      implicit none
      integer nmin,nhour,nday,nmonth,nyear
!
      integer :: ih(8)
!
      real :: zsnow(NHOR) = 0.
!
      where (xls(:) < 0.5) zsnow(:) = 1.e3/CRHOSN *xsnow(:)
!
      call ntomin(nstep,nmin,nhour,nday,nmonth,nyear)
!
      ih(2) = 0
      ih(3) = nyear*10000 +nmonth*100 +nday
      ih(4) = nhour*100 +nmin
      ih(5) = NLON
      ih(6) = NLAT
      ih(7) = 0
      ih(8) = 0
!
      ih(1) = 701
      call mpwritegph(71,xheata,NHOR,1,ih)
      ih(1) = 702
      call mpwritegph(71,xhnewa,NHOR,1,ih)
      ih(1) = 703
      call mpwritegph(71,xtsfluxa,NHOR,1,ih)
      ih(1) = 704
      call mpwritegph(71,xsmelta,NHOR,1,ih)
      ih(1) = 705
      call mpwritegph(71,ximelta,NHOR,1,ih)
      ih(1) = 706
      call mpwritegph(71,xofluxa,NHOR,1,ih)
      ih(1) = 707
      call mpwritegph(71,xcfluxa,NHOR,1,ih)
      ih(1) = 708
      call mpwritegph(71,xqmelta,NHOR,1,ih)
      ih(1) = 709
      call mpwritegph(71,xflxicea,NHOR,1,ih)
      ih(1) = 710
      call mpwritegph(71,xicec,NHOR,1,ih)
      ih(1) = 711
      call mpwritegph(71,xiced,NHOR,1,ih)
      ih(1) = 712
      call mpwritegph(71,xscflxa,NHOR,1,ih)
      ih(1) = 739
      call mpwritegph(71,xts,NHOR,1,ih)
      ih(1) = 741
      call mpwritegph(71,zsnow,NHOR,1,ih)
      ih(1) = 769
      call mpwritegph(71,xsst,NHOR,1,ih)
      ih(1) = 772
      call mpwritegph(71,xls,NHOR,1,ih)
      ih(1) = 790
      call mpwritegph(71,xclicec2,NHOR,1,ih)
      ih(1) = 791
      call mpwritegph(71,xcliced2,NHOR,1,ih)
      ih(1) = 794
      call mpwritegph(71,xcpmea,NHOR,1,ih)
      ih(1) = 795
      call mpwritegph(71,xcroffa,NHOR,1,ih)
      ih(1) = 796
      call mpwritegph(71,xstoia,NHOR,1,ih)
      if(nentropy > 0) then
       ih(1) = 797
       call mpwritegph(71,xentro,NHOR,1,ih)
      endif
!
      return
      end subroutine iceout

!====================================================================
!     SUBROUTINE MKFLUKOI
!====================================================================

      subroutine mkflukoi
      use icemod
      real zrhoilf
!
      zrhoilf=CRHOI*CLFI
!
      xflxice2(:)=0.
      if(taunc > 0.) then
       where(xls(:) < 0.5)
        xflxice2(:)=(xiced(:)-xcliced2(:))*zrhoilf/taunc
       end where
      else
       where(xls(:) < 0.5)
        xflxice2(:)=(xiced(:)-xcliced2(:))*zrhoilf/xdt                  &
     &             -xcflux(:)-xscflx(:)
       end where
      endif
!
      return
      end subroutine mkflukoi

!     =====================================================================
!     SUBROUTINE addfci
!     =====================================================================

      subroutine addfci
      use icemod
      implicit none
      real zrhoilfdt
      integer jhor
!
      real :: zmelt(NHOR) = 0.
      real :: zflr(NHOR) = 0.
      real :: zsum(2) = 0.
!
!     debug arrays
!
      real, allocatable :: zprf1(:),zprf2(:),zprf3(:)
!
      zrhoilfdt=CRHOI*CLFI/xdt
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,xflxice2,1)
       call mpgagp(zprf2,xclsst2,1)
       call mpgagp(zprf3,xcliced2,1)
       if(mypid==NROOT) then
        print*,'In addfci:'
        print*,'fluko: ',zprf1(nprhor)
        print*,'clsst: ',zprf2(nprhor)
        print*,'cliced: ',zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
!     add flux correction (ensure energy conservation)
!
      zflr(:)=0.
      do jhor=1,NHOR
       if(xls(jhor) < 1.) then
!
!     if ice or (clim. ice and t <= tclim):
!     make new ice (from fluko) and substract fluko from conductive hfl
!
        if((xsst(jhor) <= TFREEZE) .or. (xiced(jhor) > 0.)              &
     &    .or. (xcliced2(jhor) > 0. .and. xsst(jhor) <= xclsst2(jhor))) &
     &  then
         zmelt(jhor)=xcflux(jhor)-xcfluxf(jhor)+xflxice2(jhor)
         xiced(jhor)=xiced(jhor)-zmelt(jhor)/zrhoilfdt
         xcflux(jhor)=xcfluxf(jhor)
        else
!
!     else: distribute not used flux correction to the global ocean  
!
         zflr(jhor)=xflxice2(jhor)
        endif
       endif
      enddo
!
      zsum(1)=SUM(zflr(:)*xgw(:),MASK=(xls(:) < 1.))
      zsum(2)=SUM(xgw(:),MASK=(xls(:) < 1.))
      call mpsumbcr(zsum,2)
      if(zsum(1) /= 0.) then
       where(xls(:) < 1.)
        xcflux(:)=xcflux+zsum(1)/zsum(2)
       end where
      endif
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,zmelt,1)
       call mpgagp(zprf2,xiced,1)
       call mpgagp(zprf3,xcflux,1)
       if(mypid==NROOT) then
        print*,'new ice and flux for ice: ',zprf2(nprhor),zprf1(nprhor)
        print*,'residual flux correction (global): ',zsum(1)/zsum(2)
        print*,'new flux into ocean: ',zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
!     correct negative ice thickness (warm ocean)
!
      where(xiced(:) <= 0.)
       xcflux(:)=xcflux(:)-xiced(:)*zrhoilfdt
       xiced(:)=0.
      end where
!
!     set infinitisimal sea ice to zero
!
      where(ABS(xiced(:)) < 1.E-9) xiced(:)=0.
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       call mpgagp(zprf1,xcflux,1)
       call mpgagp(zprf2,xiced,1)
       if(mypid==NROOT) then
        print*,'final ice: ',zprf2(nprhor)
        print*,'final conductive hf: ',zprf1(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
      endif
!
      return
      end subroutine addfci

!====================================================================
!     SUBROUTINE SKINTEMP
!====================================================================

      subroutine skintemp
      use icemod
      implicit none
      real stb,zrcpl,zcpdt,zkapz,zflx
      integer jhor
!
      parameter(stb=5.67E-8) ! stefan boltzmann constant
!
      real :: ztso(NHOR)         ! old ts for diagnostics
      real :: zhsnow(NHOR)       ! snow depth (m snow)
      real :: zckap_mean(NHOR)   ! kappa (ice+snow layer)
      real :: zoflux(NHOR)       ! residual oceanic heat flux (diagnostics)
!
!     debug arrays
!
      real, allocatable :: zprf1(:),zprf2(:),zprf3(:),zprf4(:),zprf5(:)
      real, allocatable :: zprf6(:),zprf7(:),zprf8(:),zprf9(:)
!
!     ONLY VALID FOR ICE COVER
!     SST GIVEN BY OCEAN MODEL
!
!     F.LUNKEIT   UNIHH     FEB-07
!
!     PURPOSE.
!     --------
!     CALCULATE ICE SKIN-TEMPERATURE AS A PROGNOSTIC VARIABLE
!
!     
      zrcpl=xmind*CRHOI*CPI
!
!     preset fluxes
!
      zoflux(:)=xoflux(:)
      xhnew(:)=xheat(:)
      xtsflux(:)=0.
      xqmelt(:)=0.
!
!     limit ice thickness: put res. ocean hfl to the global ocean
!
      if(xmaxd >= 0.) then
       where(xiced(:) >= 1.1*xmaxd .and. xoflux(:) <  0.)
        xcfluxr(:)=xheat(:)+xoflux(:)
        xoflux(:)=0.
       end where
      endif
!
!     save old ts of ice (max of tmelt)
!
      ztso(:)=AMIN1(TMELT,xts(:))
!
!     start calculations
!
      do jhor=1,NHOR
       zhsnow(jhor)=1.E3/CRHOSN*xsnow(jhor)
       if(xiced(jhor) >= xmind) then
        zcpdt=zrcpl/xdt
        zckap_mean(jhor)=(CKAPSN*zhsnow(jhor)+CKAPI*xiced(jhor))        &
     &                  /(zhsnow(jhor)+xiced(jhor))
!
!     new skin temperature (implicit w.r.t. heat conduction)
!
        zkapz=zckap_mean(jhor)/(xiced(jhor)+zhsnow(jhor))
!
!     set ice flux to 0 if ice > xmaxd
!
        if(xiced(jhor) > xmaxd .and. xmaxd >= 0.) zkapz=0.
!
        zflx=xheat(jhor)+zkapz*xsst(jhor)
        xts(jhor)=(zcpdt*xts(jhor)+zflx)/(zcpdt+zkapz)
!
!     residual flux going into ice/snow melt
!
        if(xts(jhor) > TMELT) then
         xqmelt(jhor)=xheat(jhor)+zkapz*(xsst(jhor)-TMELT)              &
     &               -zcpdt*(TMELT-ztso(jhor))
         xts(jhor)=TMELT
        endif
!
!     diagnose fluxes
!
        xhnew(jhor)=zkapz*(xts(jhor)-xsst(jhor))
        xtsflux(jhor)=zcpdt*(xts(jhor)-ztso(jhor))
!
       else
        xts(jhor)=xsst(jhor)
        if(xiced(jhor) > 0.) then
         xqmelt(jhor)=xheat(jhor)
        endif
       endif
      enddo
!
!     depug print out if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       allocate(zprf4(NLON*NLAT))
       allocate(zprf5(NLON*NLAT))
       allocate(zprf6(NLON*NLAT))
       allocate(zprf7(NLON*NLAT))
       allocate(zprf8(NLON*NLAT))
       allocate(zprf9(NLON*NLAT))
       call mpgagp(zprf1,zhsnow,1)
       call mpgagp(zprf2,zckap_mean,1)
       call mpgagp(zprf3,xheat,1)
       call mpgagp(zprf4,zoflux,1)
       call mpgagp(zprf5,ztso,1)
       call mpgagp(zprf6,xts,1)
       call mpgagp(zprf7,xhnew,1)
       call mpgagp(zprf8,xtsflux,1)
       call mpgagp(zprf9,xqmelt,1)
       if(mypid==NROOT) then
        print*,'in skintemp: '
        print*,'snow depth (in m snow): ',zprf1(nprhor)
        print*,'kappa: ',zprf2(nprhor)
        print*,'atm. heat flux: ',zprf3(nprhor)
        print*,'oce. (residual) heat flux: ',zprf4(nprhor)
        print*,'old and new Ts: ',zprf5(nprhor),zprf6(nprhor)
        print*,'conductive heat flux (w. new ts): ',zprf7(nprhor)
        print*,'heat flux used to warm/cool ice: ',zprf8(nprhor)
        print*,'residual flux to melt snow/ice: ',zprf9(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
       deallocate(zprf4)
       deallocate(zprf5)
       deallocate(zprf6)
       deallocate(zprf7)
       deallocate(zprf8)
       deallocate(zprf9)
      endif
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       xentro(:,1)=xtsflux(:)/ztso(:)
      endif
!
      return
      end subroutine skintemp


!     =================
!     SUBROUTINE ICEGET
!     =================

      subroutine iceget
      use icemod
      implicit none
      integer iyea,imon,iday,ihou,imin,jm1,jm2
      real zgw1,zgw2

      if (nperpetual_ice > 0) then
         imon = mod(nperpetual_ice-1,30) + 1
         iday = nperpetual_ice - imon * 30
         ihou = 0
         imin = 0
      else
         call ntomin(nstep+1,imin,ihou,iday,imon,iyea) ! ts = ts(t)+ dtsdt
      endif

      jm1 = imon
      if (iday > 15) then
        jm2 = jm1 + 1
      else
        jm2 = jm1 - 1
      endif

      zgw2 = abs(((iday-1) * 1440 + ihou * 60 + imin)-21600.)/ 43200.0
      if (zgw2 > 1.0) zgw2 = 1.0 ! Happens for 31th. day of real calendar
      zgw1 = 1.0 - zgw2
      xclsst2(:) = zgw1 * xclsst(:,jm1) + zgw2 * xclsst(:,jm2) ! SST

      if (nicec2d == 1) then ! interpolate cover then calculate thickness
         xclicec2(:) = zgw1 * xclicec(:,jm1) + zgw2 * xclicec(:,jm2)
         call icecovertothickness(xclicec2,xcliced2,zgw1,zgw2,jm1,jm2)
      else
         xcliced2(:) = zgw1 * xcliced(:,jm1) + zgw2 * xcliced(:,jm2)
      endif

!     Compute binary ice mask from ice thickness

      where (xcliced2(:) > xmind)
         xclicec2(:) = 1.0
      elsewhere
         xclicec2(:) = 0.0
      endwhere

      return
      end subroutine iceget


!     =================
!     SUBROUTINE GETFLX
!     =================

      subroutine getflx
      use icemod
      implicit none
      integer iyea,imon,iday,ihou,imin,jm1,jm2
      real zgw1,zgw2

      if (nperpetual_ice > 0) then
         imon = mod(nperpetual_ice-1,30) + 1
         iday = nperpetual_ice - imon * 30
         ihou = 0
         imin = 0
      else
         call ntomin(nstep+1,imin,ihou,iday,imon,iyea) ! ts = ts(t)+ dtsdt
      endif

      jm1 = imon
      if (iday > 15) then
        jm2 = jm1 + 1
      else
        jm2 = jm1 - 1
      endif

      zgw2 = abs(((iday-1) * 1440 + ihou * 60 + imin)-21600.)/ 43200.0
      if (zgw2 > 1.0) zgw2 = 1.0 ! Happens for 31th. day of real calendar
      zgw1 = 1.0 - zgw2

      xflxice2(:) = zgw1 * xflxice(:,jm1) + zgw2 * xflxice(:,jm2)

      return
      end subroutine getflx


!     ==============================
!     SUBROUTINE ICECOVERTOTHICKNESS
!     ==============================

      subroutine icecovertothickness(picec,piced,pw1,pw2,km1,km2)
      use icemod
      implicit none
      real zf,zc,zd,zmaxn,pw1,pw2
      integer km1,km2,jlat,jlon

!     convert ice compactness to thickness (see CCM3 report pp 127-129)
      real cminn,cmins,cmaxn,cmaxs,hminn,hmins,hmaxn,hmids,hdeln
      parameter(cminn = 0.1 ,cmins = 0.25)
      parameter(cmaxn = 0.9 ,cmaxs = 1.0 )
      parameter(hminn = 0.25,hmins = 0.25)
      parameter(hmaxn = 3.0 ,hmids = 0.50)
      parameter(hdeln = hmaxn - hminn)

      real :: picec(NLON,NLPP)
      real :: piced(NLON,NLPP)

!     annual cycle factor     Dec   Jan   Feb   Mar   Apr   May   Jun
      real :: zhfac(0:13)=(/0.912,0.942,1.000,1.058,1.124,1.161,1.175 &
                           ,1.058,0.931,0.883,0.880,0.876,0.912,0.942/)
!     annual cycle factor     Jul   Aug   Sep   Oct   Nov   Dec   Jan

      zf = pw1 * zhfac(km1) + pw2 * zhfac(km2) ! time interpolation

      do jlat = 1 , NLPP
         if (deglat(jlat) > 0.0) then ! northern hemisphere
            zmaxn = hminn + hdeln * (deglat(jlat) - 50.0) / 20.0 
            if (zmaxn > hmaxn) zmaxn = hmaxn ! north of 70 degrees North
            if (zmaxn < hminn) zmaxn = hminn ! south of 50 degrees North
            do jlon = 1 , NLON
               zc = picec(jlon,jlat)
               if (zc > cmaxn) then
                  zd = zmaxn
               elseif (zc > cminn) then
                  zd = hminn + (zmaxn-hminn)*(zc-cminn)/(cmaxn-cminn)
               elseif (zc > 0.0) then
                  zd = hminn
               else
                  zd = 0.0
               endif
! PBH now turn this to weighted average thickness
               zd=zd*zc

               piced(jlon,jlat) = zd * zf ! apply annual cycle factor
            enddo ! jlon
         else                    ! southern hemisphere
            do jlon = 1 , NLON
               zc = picec(jlon,jlat)
               if (zc > cmins) then
                  zd = hmids
               elseif (zc > 0.0) then
                  zd = hmins + zc
               else
                  zd = 0.0
               endif
! PBH now turn this to weighted average thickness
               zd=zd*zc

               piced(jlon,jlat) = zd
            enddo ! jlon
         endif ! (deglat(jlat) > 0.0)
      enddo ! jlat
      return
      end subroutine icecovertothickness


!     =============================
!     SUBROUTINE MAKE_ICE_THICKNESS
!     =============================

      subroutine make_ice_thickness
      use icemod
      implicit none
      integer jm,jlat

      real cminn,cmins,cmaxn,cmaxs,hminn,hmins,hmaxn,hmaxs
      parameter(cminn=0.1 ,cmins=0.25)
      parameter(cmaxn=0.9 ,cmaxs=1.0 )
      parameter(hminn=0.25,hmins=0.25)
      parameter(hmaxn=3.0 ,hmaxs=0.50)

      real :: zc(NLON,NLAT,0:13)
      real :: zd(NLON,NLAT,0:13)

      real :: zhfac(0:13)=(/0.912,0.942,1.,1.058,1.124,1.161,1.175,1.058,0.931 &
                        ,0.883,0.88,0.876,0.912,0.942/)

!     convert ice compactness to thickness (see CCM3 report pp 127-129)

      call mpgagp(zc,xclicec,14)
      zd(:,:,:) = 0.0
      
      if (mypid == NROOT) then
         do jm = 0 , 13
   
      !     northern hemisphere
      
            do jlat = 1 , NLAT/2
               where (zc(:,jlat,jm) >= cmaxn)
                  zd(:,jlat,jm)=hmaxn*zhfac(jm)
               elsewhere (zc(:,jlat,jm) >= cminn)
                  zd(:,jlat,jm)=(hminn+(hmaxn-hminn)*(zc(:,jlat,jm)-cminn) &
                               /(cmaxn-cminn))*zhfac(jm)
               elsewhere (zc(:,jlat,jm) > 0.0)
                  zd(:,jlat,jm)=hminn*zhfac(jm)
               endwhere
! PBH now turn this to weighted average thickness
               zd(:,jlat,jm)=zd(:,jlat,jm)*zc(:,jlat,jm)

            enddo
      
      !     southern hemisphere
      
            do jlat = NLAT/2+1 , NLAT
               where (zc(:,jlat,jm) >= cmins)
                  zd(:,jlat,jm)=hmaxs
               elsewhere (zc(:,jlat,jm) > 0.0)
                  zd(:,jlat,jm)=hmins+zc(:,jlat,jm)
               endwhere
! PBH now turn this to weighted average thickness
               zd(:,jlat,jm)=zd(:,jlat,jm)*zc(:,jlat,jm)

            enddo
   
         enddo ! jm
      endif ! (mypid == NROOT)

      call mpscgp(zd,xcliced,14)

      return
      end subroutine make_ice_thickness

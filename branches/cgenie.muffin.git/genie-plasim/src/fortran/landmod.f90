      module landmod
      use pumamod
      implicit none
!
!     version identifier (date)
!
      character(len=80) :: version = '22.02.2005 by Larry'
!
!     parameters
!
      integer NLSOIL
      real WSMAX_EARTH,WSMAX_MARS
      parameter(NLSOIL=5)
      parameter(WSMAX_EARTH = 0.5) ! Initial value vor Earth
      parameter(WSMAX_MARS  = 0.0) ! Initial value for Mars
!
!     namelist parameters
!
      integer :: nlandt   = 1     ! switch for land model (1/0 : prog./clim)
      integer :: nlandw   = 1     ! switch for soil model (1/0 : prog./clim)
      integer :: nbiome   = 0     ! switch for vegetation model (1/0 : prog./clim)
      integer :: nmoist   = 1     ! switch for ENTS moisture limitation term (can set to 0 to test effect of irrigation on NPP)
      integer :: newsurf  = 0     ! (dtcl,dwcl) 1: update from file, 2:reset 
      integer :: nwatcini = 0     ! (0/1) initialize water content of soil
      real    :: albland  = 0.2   ! albedo for land
      real    :: albsmin  = 0.4   ! min. albedo for snow
      real    :: albsmax  = 0.8   ! max. albedo for snow
      real    :: albsminf = 0.3   ! min. albedo for snow (with forest)
      real    :: albsmaxf = 0.4   ! max. albedo for snow (with forest)
      real    :: albgmin  = 0.6   ! min. albedo for glaciers
      real    :: albgmax  = 0.8   ! max. albedo for glaciers
      real    :: dz0land  = 2.0   ! roughness length land
      real    :: drhsland = 0.25  ! wetness factor land
      real    :: drhsfull = 0.4   ! threshold above which drhs=1 [frac. of wsmax]
      real    :: dzglac   = -1.   ! threshold of orography to be glacier (-1=none)
      real    :: dztop    = 0.20  ! thickness of the uppermost soil layer (m)
      real    :: dsmax    = 5.00  ! maximum snow depth (m-h20; -1 = no limit)

      real    :: wsmax    = WSMAX_EARTH ! max field capacity of soil water (m)
      real    :: dwatcini = 0           ! water content of soil (m)

!     SIMBA - switches

      integer :: ncveg    = 1     ! compute new dcveg (0=keep initial state)

!     SIMBA - fixed parameters

      real    :: rlue     =  8.0E-10
      real    :: co2conv  = 14.0
      real    :: tau_veg  = 10.0  ! [years] - in landini scaled to seconds
      real    :: tau_soil = 42.0  ! [years] - in landini scaled to seconds
      real    :: riniveg  =  1.0 ! PBH changed from zero - we need a seed for ents - very high seed favoured in case effects snow covered regions
      real    :: rinifor  =  0.5
      real    :: rinisoil =  1.0 ! PBH chnaged from zero to speed up eqm
      real    :: rnbiocats=  0.0
!
!     global scalars (snow similar to the sea ice module)
!
      real :: rhosnow  = 330.    ! snow density (kg/m**3)
      real :: soildiff = 7.      ! heat diffusivity of the soil (W/m/K)
      real :: sicediff = 2.03    ! heat diffusivity of ice      (W/m/K)
      real :: snowdiff = 0.31    ! heat diffusivity of snow     (W/m/K)
      real :: soilcap  = 2.4E6   ! heat capacity of the soil  (J/m**3/K)
      real :: sicecap  = 2.07E6  ! heat capacity of ice       (J/m**3/K)
      real :: snowcap  = 0.6897E6! heat capacity of snow      (J/m**3/K)

!     SIMBA - flexible parameters

      real    :: forgrow  = 1.0
      real    :: rlaigrow = 0.5
      real    :: gs       = 1.0
      real    :: z0_max   = 2.0

!
!     global arrays
!
!     a) surface definitions
!
      real :: doro(NHOR) = 0.0     ! orography (m2/s2)
      real :: dts(NHOR)  = 1.0e20  ! surface temperature (K)
      real :: dtsm(NHOR) = 1.0e20  ! surface temperature (K)
      real :: dqs(NHOR)  = 1.0e20  ! surface humidity    (kg/kg)
!
!     b) runoff
!
      real :: driver(NHOR) = 0.0   ! surface water (river runoff) (m)
      real :: duroff(NHOR) = 0.0   ! zonal runoff-velocity  (1/s)
      real :: dvroff(NHOR) = 0.0   ! meridional runoff-velocity  (1/s)
      real :: darea(NHOR)  = 1.0   ! area weights
!
!     c) soil
!
      real :: dsoilz(NLSOIL)=(/0.4,0.8,1.6,3.2,6.4/)  ! soil layer thickness (m)
      real :: dsoilt(NHOR,NLSOIL) = TMELT  ! soil temperatur (K)
      real :: dsnowt(NHOR)        = TMELT  ! snow temperatur (K)
      real :: dtclsoil(NHOR)      = TMELT  ! clim soil temp. (initilization) (K)
      real :: dsnowz(NHOR)=0.           ! snow depth (m water equivalent)
      real :: dwater(NHOR)              ! surface water for soil (m/s)
      real :: dhfls(NHOR)               !heat flux into soil
!
!     d) vegetation (simba)
!
!     * parameters for optimization

      real :: pgrow(NHOR)    =1.0       ! biomass growth parameter
      real :: plai(NHOR)     =0.5       ! aboveground growth parameter
      real :: pgs(NHOR)      =1.0       ! stomatal conductance
      real :: pz0_max(NHOR)  =2.0       ! maximum roughness

!
!     e) climatological surface
!
      real :: dtcl(NHOR,0:13)      ! climatological surface temperature
                                   ! (monthly means)
      real :: dwcl(NHOR,0:13)=-1.0 ! climatological soil wetness
                                   ! (monthly means)
      real :: dalbcl(NHOR,0:13)    ! climatological background albedo
                                   ! (monthly means)
      real :: dtclim(NHOR)         ! climatological surface temperature
      real :: dwclim(NHOR)         ! climatological soil wetness
      real :: dz0clim(NHOR)        ! climatological z0  (total)
      real :: dz0climo(NHOR)       ! climatological z0  (from topograhpy only)
      real :: dalbclim(NHOR)       ! climatological background albedo
!
      end module landmod

!     ==================
!     FUNCTION NCOUNTSEA
!     ==================

      integer function ncountsea(plsm)
      use pumamod
      implicit none
      integer ilpo,ispo,ilperc,isperc
      real zsum
      real :: plsm(NHOR)

      call mpsumval(plsm,NHOR,1,zsum)
      ilpo    = nint(zsum)
      ispo    = NUGP - ilpo
      ilperc  = nint((100.0 * zsum) / NUGP)
      isperc  = 100 - ilperc
  
      if (mypid == NROOT) then
         write(*,'(a,i6,a,i6,a,i3,a)') &
              ' Land:',ilpo,' from',NUGP,' = ',ilperc,'%'
         write(*,'(a,i6,a,i6,a,i3,a)') &
              ' Sea: ',ispo,' from',NUGP,' = ',isperc,'%'
      endif
      ncountsea = ispo

      return
      end

!     ==================
!     SUBROUTINE LANDINI
!     ==================

      subroutine landini
      use landmod
      implicit none
      integer jm,jhor
      real zalbmax,zalbmin,zdalb,zalbsnow
!
!     initialize land surface
!
      namelist/landpar/nlandt,nlandw,albland,dz0land,drhsland    &
     &                ,albsmin,albsmax,albsminf,albsmaxf                &
     &                ,albgmin                                          &
     &                ,dsmax,wsmax,drhsfull,dzglac,dztop,dsoilz         &
     &                ,rlue,co2conv,tau_veg,tau_soil,forgrow            &
     &                ,rlaigrow,gs,z0_max,riniveg,rinisoil,rnbiocats    &
     &                ,ncveg,newsurf,rinifor,nwatcini,dwatcini
!
      namelist/landpar_plasim_nml/                                &
               albgmax,nbiome,nmoist
      interface
         integer function ncountsea(plsm)
            use pumamod
            real plsm(NHOR)
         end function ncountsea
      end interface

      dtclsoil(:) = TMELT
      dsoilt(:,:) = TMELT
      dsnowt(:)   = TMELT

      if (mars == 1) then
         wsmax = WSMAX_MARS
!        nlandt = 0
         nlandw = 0
!        dtclsoil(:) = TMELT_CO2
!        dsoilt(:,:) = TMELT_CO2
!        dsnowt(:)   = TMELT_CO2
      endif

      if (mypid == NROOT) then
         open(12,file=trim(runtime_root)//'/genie-plasim/config/land_namelist',form='formatted')
         read(12,landpar)
         write (*,'(/," *****************************************")')
         write (*,'(" * LANDMOD ",a29," *")') trim(version)
         write (*,'(" *****************************************")')
         write (*,'(" * Namelist LANDPAR from <land_namelist> *")')
         write (*,'(" *****************************************")')
         write(*,landpar)
         close(12)
      endif
      open(unit=56,file='data_PLASIM')
      read(unit=56,NML=landpar_plasim_nml)
      write(*,landpar_plasim_nml)
      close(56)

      if (wsmax < 0.0) wsmax = 0.0 ! Catch user error

      call mpbci(nlandt)
      call mpbci(nlandw)
      call mpbci(nbiome)
      call mpbci(nmoist)
      call mpbci(newsurf)
      call mpbci(nwatcini)
      call mpbcr(albland)
      call mpbcr(albsmin)
      call mpbcr(albsmax)
      call mpbcr(albsminf)
      call mpbcr(albsmaxf)
      call mpbcr(albgmin)
      call mpbcr(albgmax)
      call mpbcr(dz0land)
      call mpbcr(drhsland)
      call mpbcr(drhsfull)
      call mpbcr(wsmax)
      call mpbcr(dwatcini)
      call mpbcr(dzglac)
      call mpbcr(dztop)
      call mpbcr(dsmax)
      call mpbci(ncveg)
      call mpbcr(rlue)
      call mpbcr(co2conv)
      call mpbcr(tau_veg)
      call mpbcr(tau_soil)
      call mpbcr(forgrow)
      call mpbcr(rlaigrow)
      call mpbcr(gs)
      call mpbcr(z0_max)
      call mpbcr(riniveg)
      call mpbcr(rinifor)
      call mpbcr(rinisoil)
      call mpbcr(rnbiocats)
      call mpbcrn(dsoilz,NLSOIL)

!     scale taus from years to seconds

      tau_veg  = tau_veg  * n_days_per_year * solar_day
      tau_soil = tau_soil * n_days_per_year * solar_day

      if (tau_veg < 1.0 .or. tau_soil < 1.0) then
         print *,' *** error: tau_veg = ',tau_veg,'  tau_soil = ',tau_soil
         stop 1
       endif

!     * set biota parameter independent of restart

      if (mypid == NROOT) then
         write(*,*) 'SET BIOME PARAMETER'
         write(*,*) '==================='
      endif
!
!     preset
!
      if (nrestart == 0) then
           dcveg(:)  = riniveg*dls
           dforest(:)= rinifor*dls
           dcsoil(:) = rinisoil*dls
      endif

      pgrow(:)   = forgrow
      pgs(:)     = gs
      plai(:)    = rlaigrow
      pz0_max(:) = z0_max
!
!     read biome arrays from surface file
!
      call mpsurfgp('pgrow'   ,pgrow   ,NHOR,1)
      call mpsurfgp('plai'    ,plai    ,NHOR,1)
      call mpsurfgp('pgs'     ,pgs     ,NHOR,1)
      call mpsurfgp('pz0_max' ,pz0_max ,NHOR,1)
      call mpsurfgp('dcveg'   ,dcveg   ,NHOR,1)
      call mpsurfgp('dcsoil'  ,dcsoil  ,NHOR,1)

      if (nrestart == 0) then
!
!*       preset some fields
!
         dz0clim(:)  = dz0land
         dwmax(:)    = wsmax
         dalbcl(:,:) = albland
!
!*       read surface parameters
!
         call mpsurfgp('doro'    ,doro    ,NHOR,1)
         call mpsurfgp('dls'     ,dls     ,NHOR,1)
         call mpsurfgp('dz0clim' ,dz0clim ,NHOR,1)
         call mpsurfgp('dz0climo',dz0climo,NHOR,1)
         call mpsurfgp('dglac'   ,dglac   ,NHOR,1)
         call mpsurfgp('dforest' ,dforest ,NHOR,1)
         call mpsurfgp('dwmax'   ,dwmax   ,NHOR,1)
         call mpsurfgp('dtclsoil',dtclsoil,NHOR,1)
         call mpsurfgp('dtcl',dtcl,NHOR,14)
         call mpsurfgp('dwcl',dwcl,NHOR,14)
         call mpsurfgp('dalbcl',dalbcl,NHOR,14)

!        make sure, that dwmax is positive

         where (dwmax(:) < 0.0) dwmax(:) = 0.0

         if (dwcl(1,1) < 0.0) then ! not in file
            do jm = 0 , 13
               dwcl(:,jm) = dwmax(:) * drhsfull * drhsland
            enddo
         endif

!        make sure, that land sea mask values are 0 or 1

         where (dls(:) > 0.5)
            dls(:) = 1.0
         elsewhere
            dls(:) = 0.0
         endwhere
         n_sea_points = ncountsea(dls)
!
!*       modify glacier mask according to dzglac
!

         if (dzglac > 0.0) then
            where (doro(:) > dzglac * ga) dglac(:)=1.0
         endif
!
!*       convert fractional glacier mask to binary mask
!
         where (dglac(:) > 0.5)
            dglac(:) = 1.0
         elsewhere
            dglac(:) = 0.0
         endwhere
!
!*    initialize soil
!
       call soilini
!
!*    initialize runoff (except for land only simulations)
!
       if (n_sea_points > 0) call roffini
!
!*    get new background albedo 
!
       call getalb
!
!*    set other surface variables
!
       do jhor=1,NHOR
        if(dls(jhor) > 0.0) then
         dtsm(jhor)=dts(jhor)
         dqs(jhor)=rdbrv*ra1*EXP(ra2*(dts(jhor)-TMELT)/(dts(jhor)-ra4)) &
     &            /psurf
         dqs(jhor)=dqs(jhor)/(1.-(1./rdbrv-1.)*dqs(jhor))
         dsnow(jhor)=dsnowz(jhor)
         if(dsnow(jhor) > 0.) then
          zalbmax=dforest(jhor)*albsmaxf+(1.-dforest(jhor))*albsmax
          zalbmin=dforest(jhor)*albsminf+(1.-dforest(jhor))*albsmin
          zdalb=(zalbmax-zalbmin)*(dts(jhor)-263.16)/(TMELT-263.16)
          zalbsnow=MAX(zalbmin,MIN(zalbmax,zalbmax-zdalb))
          dalb(jhor)=dalbclim(jhor)                                     &
     &        +(zalbsnow-dalbclim(jhor))*dsnow(jhor)/(dsnow(jhor)+0.01)
          drhs(jhor)=1.
         else
          dalb(jhor)=dalbclim(jhor)
          if (dwmax(jhor) > 0.0)                                        &
          drhs(jhor)=AMIN1(1.,dwatc(jhor)/(drhsfull*dwmax(jhor)))
         endif
         dz0(jhor)=dz0clim(jhor)
!
!        diagnostics: soil temps.
!
         dtsoil(jhor)=dsoilt(jhor,1)
         if(NLSOIL > 2) dtd2(jhor)=dsoilt(jhor,2)
         if(NLSOIL > 3) dtd3(jhor)=dsoilt(jhor,3)
         if(NLSOIL > 4) dtd4(jhor)=dsoilt(jhor,4)
         dtd5(jhor)=dsoilt(jhor,NLSOIL)

!
!*       modifications according to glacier mask
!

         if(dglac(jhor) > 0.5) then
          dsnowz(jhor)=AMAX1(dsmax,0.)
          dsnow(jhor)=dsnowz(jhor)
          zdalb=(albgmax-albgmin)*(dts(jhor)-263.16)/(TMELT-263.16)
          dalb(jhor)=MAX(albgmax,MIN(albgmax,albgmax-zdalb))
          drhs(jhor)=1.0
         end if

!
!*    set PUMA temperature and moisture:
!

         dt(jhor,NLEP)=dts(jhor)
         dq(jhor,NLEP)=dqs(jhor)

        endif
       enddo

      else ! restart > 0

       call mpgetgp('dtsl'    ,dts     ,NHOR,     1)
       call mpgetgp('dtsm'    ,dtsm    ,NHOR,     1)
       call mpgetgp('dqs'     ,dqs     ,NHOR,     1)
       call mpgetgp('driver'  ,driver  ,NHOR,     1)
       call mpgetgp('duroff'  ,duroff  ,NHOR,     1)
       call mpgetgp('dvroff'  ,dvroff  ,NHOR,     1)
       call mpgetgp('darea'   ,darea   ,NHOR,     1)
       call mpgetgp('dwmax'   ,dwmax   ,NHOR,     1)
       call mpgetgp('dtcl'    ,dtcl    ,NHOR,    14)
       call mpgetgp('dwcl'    ,dwcl    ,NHOR,    14)
       call mpgetgp('dsnowt'  ,dsnowt  ,NHOR,     1)
       call mpgetgp('dsnowz'  ,dsnowz  ,NHOR,     1)
       call mpgetgp('dsoilt'  ,dsoilt  ,NHOR,NLSOIL)
       call mpgetgp('dcveg'   ,dcveg   ,NHOR,     1)
       call mpgetgp('dcsoil'  ,dcsoil  ,NHOR,     1)
       call mpgetgp('dglac'   ,dglac   ,NHOR,     1)
       call mpgetgp('dz0clim' ,dz0clim ,NHOR,     1)
       call mpgetgp('dz0climo',dz0climo,NHOR,     1)
       call mpgetgp('dalbcl'  ,dalbcl  ,NHOR,    14)

       n_sea_points = ncountsea(dls)

      endif ! if (restart == 0)

      fixcveg(:)=dcveg(:)
      fixcsoil(:)=dcsoil(:)

      if (newsurf == 1) then
         call mpsurfgp('dtcl',dtcl,NHOR,14)
         call mpsurfgp('dwcl',dwcl,NHOR,14)
      endif

      if (newsurf == 2) then ! preset some fields
         dwmax(:)    = wsmax
         dz0clim(:)  = dz0land
         dalbcl(:,:) = albland
         dwcl(:,:)   = wsmax * drhsfull * drhsland
      endif

      return
      end subroutine landini


!     ===================
!     SUBROUTINE LANDSTEP
!     ===================

      subroutine landstep
      use landmod
      implicit none
      integer jhor
      real zalbmax,zalbmin,zdalb,zalbsnow

!
!     get climatological values if t and/or w are non interactive
!

      if(nlandt==0 .or. nlandw==0 ) call gettcll

!
!     soil
!

      call soilstep

!
!     runoff
!

      call roffstep
!
!     get new background albedo
!
      call getalb
!
!     set surface variables
!
      do jhor=1,NHOR
       if(dls(jhor) > 0.0) then
        dtsm(jhor)=dts(jhor)
        dqs(jhor)=rdbrv*ra1*EXP(ra2*(dts(jhor)-TMELT)/(dts(jhor)-ra4))  &
     &           /psurf
        dqs(jhor)=dqs(jhor)/(1.-(1./rdbrv-1.)*dqs(jhor))
        dsnow(jhor)=dsnowz(jhor)
        if(dsnow(jhor) > 0.) then
         zalbmax=dforest(jhor)*albsmaxf+(1.-dforest(jhor))*albsmax
         zalbmin=dforest(jhor)*albsminf+(1.-dforest(jhor))*albsmin
         zdalb=(zalbmax-zalbmin)*(dts(jhor)-263.16)/(TMELT-263.16)
         zalbsnow=MAX(zalbmin,MIN(zalbmax,zalbmax-zdalb))
         dalb(jhor)=dalbclim(jhor)                                     &
     &       +(zalbsnow-dalbclim(jhor))*dsnow(jhor)/(dsnow(jhor)+0.01)
         drhs(jhor)=1.
        else
         dalb(jhor)=dalbclim(jhor)
         if (dwmax(jhor) > 0.0)                                        &
         drhs(jhor)=AMIN1(1., dwatc(jhor)/(drhsfull *dwmax(jhor)))
        endif

!
!     diagnostics: soil temps.
!

        dtsoil(jhor)=dsoilt(jhor,1)
        if(NLSOIL > 2) dtd2(jhor)=dsoilt(jhor,2)
        if(NLSOIL > 3) dtd3(jhor)=dsoilt(jhor,3)
        if(NLSOIL > 4) dtd4(jhor)=dsoilt(jhor,4)
        dtd5(jhor)=dsoilt(jhor,NLSOIL)

!
!*    set PUMA temperature and moisture:
!

        dt(jhor,NLEP)=dts(jhor)
        dq(jhor,NLEP)=dqs(jhor)

       end if
      enddo

!
!*    vegetation
!

      call vegstep

!
!*     modifications according to glacier mask
!

      where(dglac(:) > 0.5 .and. dls(:) > 0.0)
       dalb(:)=MAX(albgmin,MIN(albgmax                                  &
     &  ,albgmax-(albgmax-albgmin)*(dts(:)-263.16)/(TMELT-263.16)))
       drhs(:)=1.0
      end where

      return
      end subroutine landstep

!     ===================
!     SUBROUTINE LAND_REST
!     ===================

      subroutine land_rest
      use landmod
      implicit none

      if (mypid == NROOT) then
         call put_restart_integer('nlsoil',NLSOIL)
      endif

      call mpputgp('dtsl'    ,dts     ,NHOR, 1)
      call mpputgp('dtsm'    ,dtsm    ,NHOR, 1)
      call mpputgp('dqs'     ,dqs     ,NHOR, 1)
      call mpputgp('driver'  ,driver  ,NHOR, 1)
      call mpputgp('duroff'  ,duroff  ,NHOR, 1)
      call mpputgp('dvroff'  ,dvroff  ,NHOR, 1)
      call mpputgp('darea'   ,darea   ,NHOR, 1)
      call mpputgp('dwmax'   ,dwmax   ,NHOR, 1)
      call mpputgp('dtcl'    ,dtcl    ,NHOR,14)
      call mpputgp('dwcl'    ,dwcl    ,NHOR,14)
      call mpputgp('dsnowt'  ,dsnowt  ,NHOR, 1)
      call mpputgp('dsnowz'  ,dsnowz  ,NHOR, 1)
      call mpputgp('dsoilt'  ,dsoilt  ,NHOR,NLSOIL)
      call mpputgp('dcveg'   ,dcveg   ,NHOR, 1)
      call mpputgp('dcsoil'  ,dcsoil  ,NHOR, 1)
      call mpputgp('dglac'   ,dglac   ,NHOR, 1)
      call mpputgp('dz0clim' ,dz0clim ,NHOR, 1)
      call mpputgp('dz0climo',dz0climo,NHOR, 1)
      call mpputgp('dalbcl'  ,dalbcl  ,NHOR,14)

      return
      end subroutine land_rest

!     ===================
!     SUBROUTINE LANDSTOP
!     ===================

      subroutine landstop
      use landmod
      implicit none

      if (mypid == NROOT) then
         call put_restart_integer('nlsoil',NLSOIL)
      endif

      call mpputgp('dtsl'    ,dts     ,NHOR, 1)
      call mpputgp('dtsm'    ,dtsm    ,NHOR, 1)
      call mpputgp('dqs'     ,dqs     ,NHOR, 1)
      call mpputgp('driver'  ,driver  ,NHOR, 1)
      call mpputgp('duroff'  ,duroff  ,NHOR, 1)
      call mpputgp('dvroff'  ,dvroff  ,NHOR, 1)
      call mpputgp('darea'   ,darea   ,NHOR, 1)
      call mpputgp('dwmax'   ,dwmax   ,NHOR, 1)
      call mpputgp('dtcl'    ,dtcl    ,NHOR,14)
      call mpputgp('dwcl'    ,dwcl    ,NHOR,14)
      call mpputgp('dsnowt'  ,dsnowt  ,NHOR, 1)
      call mpputgp('dsnowz'  ,dsnowz  ,NHOR, 1)
      call mpputgp('dsoilt'  ,dsoilt  ,NHOR,NLSOIL)
      call mpputgp('dcveg'   ,dcveg   ,NHOR, 1)
      call mpputgp('dcsoil'  ,dcsoil  ,NHOR, 1)
      call mpputgp('dglac'   ,dglac   ,NHOR, 1)
      call mpputgp('dz0clim' ,dz0clim ,NHOR, 1)
      call mpputgp('dz0climo',dz0climo,NHOR, 1)
      call mpputgp('dalbcl'  ,dalbcl  ,NHOR,14)

      return
      end subroutine landstop

!     ================
!     SUBROUTINE TANDS
!     ================

      subroutine tands
      use landmod
      implicit none
      integer jlev,jhor
      real zzmax,zzmin
!
      real zsnowmax,ztop
      parameter(zsnowmax=1.)
      parameter(ztop=0.1)
!
      real zsnowz(NHOR)       ! new snow depth
      real zhfls(NHOR)        ! heatflux inot soil
      real zhfla(NHOR)        ! heatflux from atmosphere
      real zhflm(NHOR)        ! heatflux used by snowmelt
      real zdsnowz(NHOR)      ! snow depth tendency
      real zcap(NHOR,NLSOIL)  ! heat capacity  of soil layers
      real zdiff(NHOR,NLSOIL) ! thermal conductivity of soil layers
      real zsoilz(NHOR,NLSOIL)! soil layer thicknesses
      real zcap1(NHOR)        ! heat capacity (upper soil layer)
      real zdiff1(NHOR)       ! thermal conductivity (upper soil layer)
      real zsoilz1(NHOR)      ! layer thickness (upper soil layer)
      real zctop(NHOR)        ! heat capacity (top soil/snow layer)
      real zsntop(NHOR)       ! snow depth (top soil/snow layer)
      real zztop(NHOR)        ! depth of the uppermost layer
!
!     debug
!
      integer kmaxl(1),kminl(1)
      real,allocatable :: zfpr1(:)
      real,allocatable :: zfpr2(:)
      real,allocatable :: zfpr3(:)
      real,allocatable :: zfpr4(:)
      real,allocatable :: zfpr5(:)
      real,allocatable :: zfpr6(:)
      real,allocatable :: zfpr7(:)
      real,allocatable :: zfpr8(:)
      real,allocatable :: zfpr9(:)
      real,allocatable :: zfprl(:,:)
!
!     preset
!
      dwater(:)=0.
      dsmelt(:)=0.
      dsndch(:)=dsnowz(:)
      zdsnowz(:)=0.
      zhfls(:)=0.
      zhflm(:)=0.
      zztop(:)=dztop
!
      do jlev=1,NLSOIL
       where(dls(:) > 0.0)
        zcap(:,jlev)=sicecap*dglac(:)+soilcap*(1.-dglac(:))
        zdiff(:,jlev)=sicediff*dglac(:)+soildiff*(1.-dglac(:))
        zsoilz(:,jlev)=dsoilz(jlev)
       endwhere
      enddo
!
      where(dsnowz(:) == 0.0) dsnowt(:)=TMELT
!
!     copy heatflux
!
      zhfla(:)=dshfl(:)+dlhfl(:)+dflux(:,NLEP)
!
!     precip destribution
!
      do jhor=1,NHOR

!
!     surface water
!

       if(dls(jhor) > 0.0) then
        if(dprs(jhor) > 0.) zdsnowz(jhor)=dprs(jhor)
        if(dsnowz(jhor) > 0.) zdsnowz(jhor)=zdsnowz(jhor)+devap(jhor)
        zsnowz(jhor)=AMAX1(0.,dsnowz(jhor)+zdsnowz(jhor)*deltsec)
        zdsnowz(jhor)=(zsnowz(jhor)-dsnowz(jhor))/deltsec
        dwater(jhor)=devap(jhor)+dprl(jhor)+dprc(jhor)-zdsnowz(jhor)
        dsnowz(jhor)=zsnowz(jhor)
       end if
      enddo
!
!     surface temperature and heat flux into the soil
!
      where(dls(:) > 0.0)
!
!     new properties of the top dztop meters (mixed soil/snow layer):
!     1: snow (m of ztop) 2: heat capacity
!
       zsntop(:)=AMIN1(dsnowz(:)*1000./rhosnow,zztop(:))
       zctop(:)=zcap(:,1)*snowcap*zztop(:)                              &
     &         /(zcap(:,1)*zsntop(:)+snowcap*(zztop(:)-zsntop(:)))
!
!     new properties of the uppermost soil layer (mixed soil/snow)
!     1: snow depth (m) 2: total thickness 3: conductivity
!
       zsnowz(:)=AMIN1(dsnowz(:)*1000./rhosnow,zsnowmax)-zsntop(:)
       zsoilz1(:)=zsoilz(:,1)+zsnowz(:)
       zdiff1(:)=zdiff(:,1)*snowdiff*zsoilz1(:)                         &
     &          /(snowdiff*zsoilz(:,1)+zdiff(:,1)*zsnowz(:))
       zcap1(:)=zcap(:,1)*snowcap*zsoilz1(:)                            &
     &         /(zcap(:,1)*zsnowz(:)+snowcap*zsoilz(:,1))
!
!     new surface temp. implicit w.r.t. conductive heat flux
!
       dts(:)=(zctop(:)*zztop(:)*dtsm(:)/deltsec+zhfla(:)               &
              +2.*zdiff1(:)*dsoilt(:,1)/zsoilz1(:))                     &
             /(zctop(:)*zztop(:)/deltsec+2.*zdiff1(:)/zsoilz1(:)) 
!
!     heat flux into the soil
!
       zhfls(:)=2.*zdiff1(:)/zsoilz1(:)*(dts(:)-dsoilt(:,1))
!
      endwhere
!
!     snow melt
!
      where(dls(:) > 0.0 .and. dsnowz(:) > 0. .and. dts(:) > TMELT )
       dts(:)=TMELT
       zhflm(:)=AMAX1(0.,zhfla(:)                                       &
     &                  -zctop(:)*zztop(:)*(dts(:)-dtsm(:))/deltsec     &
     &                  +2.*zdiff1(:)*(dsoilt(:,1)-dts(:))/zsoilz1(:))  
       zsnowz(:)=AMAX1(0.,dsnowz(:)-zhflm(:)*deltsec/((ALS-ALV)*1000.))
       zdsnowz(:)=(zsnowz(:)-dsnowz(:))/deltsec
       zhflm(:)=-zdsnowz(:)*1000.*(ALS-ALV)
!
!     new snow depth (h2o equiv. and snow melt diagnostic)
!
       dsnowz(:)=zsnowz(:)
       dsmelt(:)=-zdsnowz(:)
!
!     heat flux and water flux into the soil
!
       zhfls(:)=2.*zdiff1(:)/zsoilz1(:)*(dts(:)-dsoilt(:,1))
       dwater(:)=dwater(:)-zdsnowz(:)
!
!     new properties of the top dztop meters (mixed soil/snow layer):
!     1: snow (m of dztop) 2: heat capacity
!
       zsntop(:)=AMIN1(dsnowz(:)*1000./rhosnow,zztop(:))
       zctop(:)=zcap(:,1)*snowcap*zztop(:)                              &
     &         /(zcap(:,1)*zsntop(:)+snowcap*(zztop(:)-zsntop(:)))
!
!     new properties of the uppermost soil layer (mixed soil/snow)
!     1: snow depth (m) 2: total thickness 3: conductivity 4: heat capacity
!
       zsnowz(:)=AMIN1(dsnowz(:)*1000./rhosnow,zsnowmax)-zsntop(:)
       zsoilz1(:)=zsoilz(:,1)+zsnowz(:)
       zdiff1(:)=zdiff(:,1)*snowdiff*zsoilz1(:)                         &
     &          /(snowdiff*zsoilz(:,1)+zdiff(:,1)*zsnowz(:))
       zcap1(:)=zcap(:,1)*snowcap*zsoilz1(:)                            &
     &         /(zcap(:,1)*zsnowz(:)+snowcap*zsoilz(:,1))
!
      endwhere
!     write(*,*) 'landmod 881 zsnowz = ',sum(zsnowz(:))
!
!     correct surface temp and flux where snow is totally melted
!
      where(dls(:) > 0.0 .and. dsnowz(:) == 0. .and. zhflm(:) > 0.)
!
!     new surface (as above)
!
       dts(:)=(zctop(:)*zztop(:)*dtsm(:)/deltsec+zhfla(:)-zhflm(:)      &
     &        +2.*zdiff1(:)*dsoilt(:,1)/zsoilz1(:))                     &
     &       /(zctop(:)*zztop(:)/deltsec+2.*zdiff1(:)/zsoilz1(:)) 
!
!     heat flux into the soil
!
       zhfls(:)=2.*zdiff1(:)/zsoilz1(:)*(dts(:)-dsoilt(:,1))
!
      endwhere
!
!     set new soil properties for the uppermost blended soil/snow layer
!

      where(dls(:) > 0.0)
       zsoilz(:,1)=zsoilz1(:)
       zcap(:,1)=zcap1(:)
       zdiff(:,1)=zdiff1(:)
      endwhere

!
!     deep soil temperatures
!

      dhfls(:)=zhfls(:) !copied globally for outmod
      call mktsoil(zhfls,zsoilz,zcap,zdiff)

!
!     snow temperature (at the moment set to ts)
!

      where(dls(:) > 0.0 .and. dsnowz(:) > 0.) dsnowt(:)=dts(:)
!
!     limit snow depth to maximum (if switched on)
!     (heat s conserved by artifical cooling of ts (dsnowt)
!      water is conserved)
!
      if(dsmax > 0.) then
       where(dls(:) > 0.0 .and. dsnowz(:) > dsmax)
        zdsnowz(:)=(dsmax-dsnowz(:))/deltsec
!
!     new snow depth (h2o equiv. and snow melt diagnostic)
!
        dsnowz(:)=dsmax
!
!     water flux into the soil
!
        dwater(:)=dwater(:)-zdsnowz(:)
!
!     cool ts (dsnowt)
!
        dts(:)=dts(:)+zdsnowz(:)*1000.*(ALS-ALV)*deltsec/zctop(:)/zztop(:)
!
       endwhere
      endif
!
!     diagnostic: snow depth change
!
      where(dls(:) > 0.) dsndch(:)=(dsnowz(:)-dsndch(:))/deltsec
!
!     dbug output
!

      if(nprint == 1) then
       allocate(zfpr1(NLON*NLAT))
       allocate(zfpr2(NLON*NLAT))
       allocate(zfpr3(NLON*NLAT))
       allocate(zfpr4(NLON*NLAT))
       allocate(zfprl(NLON*NLAT,NLSOIL))
       call mpgagp(zfpr1,dprs,1)
       call mpgagp(zfpr2,dts,1)
       call mpgagp(zfpr3,dsnowz,1)
       call mpgagp(zfpr4,dls,1)
       call mpgagp(zfprl,dsoilt,NLSOIL)
       if(mypid == NROOT) then
        print*,'Land Surface Global Diagnostic:'
        zzmax=MAXVAL(zfpr2(:),MASK=(zfpr4(:) > 0.5))
        kmaxl=MAXLOC(zfpr2(:),MASK=(zfpr4(:) > 0.5))
        zzmin=MINVAL(zfpr2(:),MASK=(zfpr4(:) > 0.5))
        kminl=MINLOC(zfpr2(:),MASK=(zfpr4(:) > 0.5))
        print*,'MAX TS = ',zzmax,' NHOR= ',kmaxl(1)
        print*,'MIN TS = ',zzmin,' NHOR= ',kminl(1)
        zzmax=MAXVAL(zfpr3(:),MASK=(zfpr4(:) > 0.5))
        kmaxl=MAXLOC(zfpr3(:),MASK=(zfpr4(:) > 0.5))
        zzmin=MINVAL(zfpr3(:),MASK=(zfpr4(:) > 0.5))
        kminl=MINLOC(zfpr3(:),MASK=(zfpr4(:) > 0.5))
        print*,'MAX ZSNOW = ',zzmax,' NHOR= ',kmaxl(1)
        print*,'MIN ZSNOW = ',zzmin,' NHOR= ',kminl(1)
        do jlev=1,NLSOIL
         zzmax=MAXVAL(zfprl(:,jlev),MASK=(zfpr4(:) > 0.5))
         kmaxl=MAXLOC(zfprl(:,jlev),MASK=(zfpr4(:) > 0.5))
         zzmin=MINVAL(zfprl(:,jlev),MASK=(zfpr4(:) > 0.5))
         kminl=MINLOC(zfprl(:,jlev),MASK=(zfpr4(:) > 0.5))
         print*,'MAX TSOIL L= ',jlev,' = ',zzmax,' NHOR= ',kmaxl(1)
         print*,'MIN TSOIL L= ',jlev,' = ',zzmin,' NHOR= ',kminl(1)
        enddo
        zzmax=MAXVAL(zfpr1(:),MASK=(zfpr4(:) > 0.5))
        kmaxl=MAXLOC(zfpr1(:),MASK=(zfpr4(:) > 0.5))
        print*,'MAX PRS = ',zzmax,' NHOR= ',kmaxl(1)
       endif
       deallocate(zfpr1)
       deallocate(zfpr2)
       deallocate(zfpr3)
       deallocate(zfpr4)
       deallocate(zfprl)
      endif
      if(nprint == 2) then
       allocate(zfpr1(NLON*NLAT))
       allocate(zfpr2(NLON*NLAT))
       allocate(zfpr3(NLON*NLAT))
       allocate(zfpr4(NLON*NLAT))
       allocate(zfpr5(NLON*NLAT))
       allocate(zfpr6(NLON*NLAT))
       allocate(zfpr7(NLON*NLAT))
       allocate(zfpr8(NLON*NLAT))
       allocate(zfpr9(NLON*NLAT))
       allocate(zfprl(NLON*NLAT,NLSOIL))
       call mpgagp(zfpr1,dprs,1)
       call mpgagp(zfpr2,dts,1)
       call mpgagp(zfpr3,dsnowz,1)
       call mpgagp(zfpr4,zhfla,1)
       call mpgagp(zfpr5,zhflm,1)
       call mpgagp(zfpr6,zhfls,1)
       call mpgagp(zfpr8,dwater,1)
       call mpgagp(zfpr9,dsmelt,1)
       call mpgagp(zfprl,dsoilt,NLSOIL)
       if(mypid == NROOT) then
        print*,'Land Surface Local Diagnostic:'
        do jlev=1,NLSOIL
         print*,'TSOIL L= ',jlev,' = ',zfprl(NPRHOR,jlev)
        enddo
        print*,'TS             = ',zfpr2(NPRHOR)
        print*,'ZSNOW          = ',zfpr3(NPRHOR)
        print*,'HFLA           = ',zfpr4(NPRHOR)
        print*,'HFLX MELTING   = ',zfpr5(NPRHOR)
        print*,'HFLX TO SOIL   = ',zfpr6(NPRHOR)
        print*,'PRS (mm/d)     = ',zfpr1(NPRHOR)*1000.*deltsec*ntspd
        print*,'DWATER (mm/d)  = ',zfpr8(NPRHOR)*1000.*deltsec*ntspd
        print*,'SNOWMELT (mm/d)= ',zfpr9(NPRHOR)*1000.*deltsec*ntspd
       endif
       deallocate(zfpr1)
       deallocate(zfpr2)
       deallocate(zfpr3)
       deallocate(zfpr4)
       deallocate(zfpr5)
       deallocate(zfpr6)
       deallocate(zfpr7)
       deallocate(zfpr8)
       deallocate(zfpr9)
       deallocate(zfprl)
      endif
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       where(dls(:) > 0.)
        dentropy(:,18)=zctop(:)*zztop(:)*(dts(:)-dtsm(:))/(dtsm(:)*deltsec)
       endwhere
      endif
!
      return
      end subroutine tands

!     ==================
!     SUBROUTINE MKTSOIL
!     ==================

      subroutine mktsoil(pftop,psoilz,pcap,pdiff)
      use landmod
      implicit none
      integer jlev,jlem,jlep
!
!     input
!
      real pftop(NHOR)         ! surface heat flux
      real psoilz(NHOR,NLSOIL) ! layer thickness
      real pcap(NHOR,NLSOIL)   ! heat capacities
      real pdiff(NHOR,NLSOIL)  ! heat conductivities
!
!     local
!
      real ztn(NHOR,NLSOIL)    ! new temperatures
      real zebs(NHOR,NLSOIL)
      real zcap(NHOR,NLSOIL)
      real zdiff(NHOR,NLSOIL-1)
      real ztold(NHOR,NLSOIL)
!
      if(nentropy > 0) ztold(:,:)=dsoilt(:,:)
!
!     implicit scheme for soiltemp
!
!     a) deep layer elimination (zero flux at bottom)
!

      jlev=NLSOIL
      jlem=NLSOIL-1
      where(dls(:) > 0.0)
       zdiff(:,jlem)=2.*pdiff(:,jlev)*pdiff(:,jlem)                     &
     &              /(pdiff(:,jlev)*psoilz(:,jlem)                      &
     &               +pdiff(:,jlem)*psoilz(:,jlev))
       zcap(:,jlev)=pcap(:,jlev)*psoilz(:,jlev)/deltsec
       zebs(:,jlev)=1./(zcap(:,jlev)+zdiff(:,jlem))
       ztn(:,jlev)=zcap(:,jlev)*dsoilt(:,jlev)*zebs(:,jlev)
      endwhere

!
!     middle layer elimination
!

      do jlev=NLSOIL-1,2,-1
       jlep=jlev+1
       jlem=jlev-1
       where(dls(:) > 0.0)
        zdiff(:,jlem)=2.*pdiff(:,jlev)*pdiff(:,jlem)                    &
     &                /(pdiff(:,jlev)*psoilz(:,jlem)                    &
     &                 +pdiff(:,jlem)*psoilz(:,jlev))
        zcap(:,jlev)=pcap(:,jlev)*psoilz(:,jlev)/deltsec
        zebs(:,jlev)=1./(zcap(:,jlev)+zdiff(:,jlem)                     &
     &                  +zdiff(:,jlev)*(1.-zdiff(:,jlev)*zebs(:,jlep)))
        ztn(:,jlev)=zebs(:,jlev)*(zcap(:,jlev)*dsoilt(:,jlev)           &
     &                           +zdiff(:,jlev)*ztn(:,jlep))
       endwhere
      enddo

!
!     top layer elimination
!

       jlev=1
       jlep=2
       where(dls(:) > 0.0)
        zcap(:,jlev)=pcap(:,jlev)*psoilz(:,jlev)/deltsec
        zebs(:,jlev)=1./(zcap(:,jlev)+zdiff(:,jlev)                     &
     &                               *(1.-zdiff(:,jlev)*zebs(:,jlep)))
        ztn(:,jlev)=zebs(:,jlev)*(zcap(:,jlev)*dsoilt(:,jlev)           &
     &                           +zdiff(:,jlev)*ztn(:,jlep)+pftop(:))
       endwhere

!
!     back-substitution
!

      do jlev=2,NLSOIL
       jlem=jlev-1
       where(dls(:) > 0.0)
        ztn(:,jlev)=ztn(:,jlev)+zebs(:,jlev)*zdiff(:,jlem)*ztn(:,jlem)
       endwhere
      enddo

!
!     new temperatures
!

      do jlev=1,NLSOIL
       where(dls(:) > 0.0)
        dsoilt(:,jlev)=ztn(:,jlev)
       endwhere

!
!      do not allow warming of permanet glaciers
!

       where(dls(:) > 0.0 .and. dglac(:) > 0.5)
        dsoilt(:,jlev)=AMIN1(dsoilt(:,jlev),TMELT)
       endwhere
      enddo
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       dentropy(:,19)=0.
       do jlev=1,NLSOIL
        where(dls(:) > 0.)
         dentropy(:,19)=dentropy(:,19)                                  &
     &                 +zcap(:,jlev)*(dsoilt(:,jlev)-ztold(:,jlev))     &
     &                 /dsoilt(:,jlev)
        endwhere
       enddo
      endif
!
      return
      end subroutine mktsoil

!     ================
!     SUBROUTINE WANDR
!     ================

      subroutine wandr
      use landmod
      implicit none
!
!     calculate soil water and runoff
!
      drunoff(:)=0.
      where (dls(:) > 0.0)
       dwatc(:)=dwatc(:)+deltsec*dwater(:)
       drunoff(:)=AMAX1(0.,dwatc(:)-dwmax(:))/deltsec
       dwatc(:)=AMAX1(AMIN1(dwmax(:),dwatc(:)),0.)
      end where
!
      return
      end subroutine wandr

!     ==================
!     SUBROUTINE SOILINI
!     ==================

      subroutine soilini
      use landmod
      implicit none
      integer jlev
!
      if(nrestart == 0) then
!
!     initialize snow and temperatures
!
       where(dls(:) > 0.0)
        dsnowz(:)=0.
        dsnowt(:)=TMELT
        dts(:)=dtclsoil(:)
       endwhere
       do jlev=1,NLSOIL
        where(dls(:) > 0.0) dsoilt(:,jlev)=dts(:)
       enddo
!
!     initialize soil water and runoff
!
       if (nwatcini > 0) then
        where(dls(:) > 0.0)
         dwatc(:)=dwatcini
        endwhere
       else
        where(dls(:) > 0.0)
         dwatc(:)=drhsland*drhsfull*dwmax(:)
        endwhere
       endif
       drunoff(:)=0.
!
      endif
!
      return
      end subroutine soilini

!     ===================
!     SUBROUTINE SOILSTEP
!     ===================

      subroutine soilstep
      use landmod
      implicit none
!
!     calculate snow and temperatures
!
      if(nlandt==1) then
       call tands
      else
       where(dls(:) > 0.0)
        dwater(:)=devap(:)+dprl(:)+dprc(:)
        dts(:)=dtclim(:)
       endwhere
      end if
!
!     calculate soil water and runoff
!
      if(nlandw==1) then
       call wandr
      else
       drunoff(:)=0.
       where(dls(:) > 0.)
        drunoff(:)=AMAX1(0.,dwater(:))
        dwatc(:)=dwclim(:)
       endwhere
      endif
!
      return
      end subroutine soilstep

!     ==================
!     SUBROUTINE ROFFINI
!     ==================

      subroutine roffini
      use landmod
      implicit none
      integer ilat,jlat,jlon,jhor,jconv
!      integer k1(nlon,nlat) !for genie runoff
      real zdx,zdy,zdh,zfac
      real zcvel,zcexp
      parameter(zcvel=4.2)
      parameter(zcexp=0.18)


      real d91,d92,d93,d94 !for genie runoff

!
      real zuroff(NLON,NLAT)
      real zvroff(NLON,NLAT)
      real zoro(NLON,NLAT)
      real zoron(NLON,NLAT)
      real zlsm(NLON,NLAT)
      real zsi(NLON,NLAT)
      real zsir(NLON,NLPP)

!
      ilat = NLAT ! using ilat suppresses compiler warnings for T1
!
      if(NRESTART==0) then

       do jlat=1,NLPP
        do jlon=1,NLON
         jhor=(jlat-1)*NLON+jlon
         darea(jhor)=gwd(jlat)
         zsir(jlon,jlat)=sid(jlat)
        enddo
       enddo

       call mpgagp(zsi,zsir,1)
       call mpgagp(zoro,doro,1)
       call mpgagp(zlsm,dls,1)

       if(mypid==NROOT) then

        zoro(:,:)=MAX(zoro(:,:),0.)
        where(zlsm(:,:) < 1.) zoro(:,:)=zlsm(:,:)-1.
!
!     iterate to remove local minima
!

 1000   continue
        jconv=0
        do jlat=2,ilat-1
         do jlon=2,NLON-1
          if(zlsm(jlon,jlat) > 0.                                       &
     &      .and. zoro(jlon,jlat) <= zoro(jlon,jlat-1)                  &
     &      .and. zoro(jlon,jlat) <= zoro(jlon,jlat+1)                  &
     &      .and. zoro(jlon,jlat) <= zoro(jlon+1,jlat)                  &
     &      .and. zoro(jlon,jlat) <= zoro(jlon-1,jlat)) then
           zoron(jlon,jlat)=1.+MIN(zoro(jlon+1,jlat),zoro(jlon-1,jlat)  &
     &                            ,zoro(jlon,jlat+1),zoro(jlon,jlat-1))
           jconv=jconv+1
          else
           zoron(jlon,jlat)=zoro(jlon,jlat)
          endif
         enddo
         if(zlsm(1,jlat) > 0.                                           &
     &     .and. zoro(1,jlat) <= zoro(1,jlat-1)                         &
     &     .and. zoro(1,jlat) <= zoro(1,jlat+1)                         &
     &     .and. zoro(1,jlat) <= zoro(2,jlat)                           &
     &     .and. zoro(1,jlat) <= zoro(NLON,jlat)) then
          zoron(1,jlat)=1.+MIN(zoro(2,jlat),zoro(NLON,jlat)             &
     &                        ,zoro(1,jlat+1),zoro(1,jlat-1))
          jconv=jconv+1
         else
          zoron(1,jlat)=zoro(1,jlat)
         endif
         if(zlsm(NLON,jlat) > 0.                                        &
     &     .and. zoro(NLON,jlat) <= zoro(NLON,jlat-1)                   &
     &     .and. zoro(NLON,jlat) <= zoro(NLON,jlat+1)                   &
     &     .and. zoro(NLON,jlat) <= zoro(1,jlat)                        &
     &     .and. zoro(NLON,jlat) <= zoro(NLON-1,jlat)) then
          zoron(NLON,jlat)=1.+MIN(zoro(1,jlat),zoro(NLON-1,jlat)        &
     &                           ,zoro(NLON,jlat+1),zoro(NLON,jlat-1))
          jconv=jconv+1
         else
          zoron(NLON,jlat)=zoro(NLON,jlat)
         endif
        enddo
        do jlon=2,NLON-1
         if(zlsm(jlon,1) > 0.                                           &
     &    .and. zoro(jlon,1) <= zoro(jlon,2)                            &
     &    .and. zoro(jlon,1) <= zoro(jlon+1,1)                          &
     &    .and. zoro(jlon,1) <= zoro(jlon-1,1)) then
          zoron(jlon,1)=1.+MIN(zoro(jlon+1,1),zoro(jlon-1,1)            &
     &                        ,zoro(jlon,2))
          jconv=jconv+1
         else
          zoron(jlon,1)=zoro(jlon,1)
         endif
         if(zlsm(jlon,NLAT) > 0.                                        &
     &    .and. zoro(jlon,NLAT) <= zoro(jlon,NLAT-1)                    &
     &    .and. zoro(jlon,NLAT) <= zoro(jlon+1,NLAT)                    &
     &    .and. zoro(jlon,NLAT) <= zoro(jlon-1,NLAT)) then
          zoron(jlon,NLAT)=1.+MIN(zoro(jlon+1,NLAT),zoro(jlon-1,NLAT)   &
     &                           ,zoro(jlon,NLAT-1))
          jconv=jconv+1
         else
          zoron(jlon,NLAT)=zoro(jlon,NLAT)
         endif
        enddo
        if(zlsm(1,1) > 0.                                               &
     &    .and. zoro(1,1) <= zoro(1,2)                                  &
     &    .and. zoro(1,1) <= zoro(2,1)                                  &
     &    .and. zoro(1,1) <= zoro(NLON,1)) then
         zoron(1,1)=1.+MIN(zoro(2,1),zoro(NLON,1)                       &
     &                    ,zoro(1,2))
         jconv=jconv+1
        else
         zoron(1,1)=zoro(1,1)
        endif
        if(zlsm(NLON,NLAT) > 0.                                         &
     &    .and. zoro(NLON,NLAT) <= zoro(NLON,NLAT-1)                    &
     &    .and. zoro(NLON,NLAT) <= zoro(1,NLAT)                         &
     &    .and. zoro(NLON,NLAT) <= zoro(NLON-1,NLAT)) then
         zoron(NLON,NLAT)=1.+MIN(zoro(1,NLAT),zoro(NLON-1,NLAT)         &
     &                          ,zoro(NLON,NLAT-1))
         jconv=jconv+1
        else
         zoron(NLON,NLAT)=zoro(NLON,NLAT)
        endif
        if(zlsm(NLON,1) > 0.                                            &
     &    .and. zoro(NLON,1) <= zoro(NLON,2)                            &
     &    .and. zoro(NLON,1) <= zoro(1,1)                               &
     &    .and. zoro(NLON,1) <= zoro(NLON-1,1)) then
         zoron(NLON,1)=1.+MIN(zoro(1,1),zoro(NLON-1,1)                  &
     &                       ,zoro(NLON,2))
         jconv=jconv+1
        else
         zoron(NLON,1)=zoro(NLON,1)
        endif
        if(zlsm(1,NLAT) > 0.                                            &
     &    .and. zoro(1,NLAT) <= zoro(1,NLAT-1)                          &
     &    .and. zoro(1,NLAT) <= zoro(2,NLAT)                            &
     &    .and. zoro(1,NLAT) <= zoro(NLON,NLAT)) then
         zoron(1,NLAT)=1.+MIN(zoro(NLON,NLAT),zoro(2,NLAT)              &
     &                       ,zoro(1,NLAT-1))
         jconv=jconv+1
        else
         zoron(1,NLAT)=zoro(1,NLAT)
        endif
        zoro(:,:)=zoron(:,:)
        if(jconv > 0 ) goto 1000

        do jlat=1,NLAT
         do jlon=1,NLON-1
          zdx=TWOPI*cos(ASIN(zsi(jlon,jlat)))*plarad/real(NLON)
          zdh=(zoro(jlon+1,jlat)-zoro(jlon,jlat))/zdx
          zfac=1.
          if(zdh > 0.) zfac=-1.
          zuroff(jlon,jlat)=zfac*zcvel/zdx*ABS(zdh)**zcexp
         enddo
         zdx=TWOPI*cos(ASIN(zsi(NLON,jlat)))*plarad/real(NLON)
         zdh=(zoro(1,jlat)-zoro(NLON,jlat))/zdx
         zfac=1.
         if(zdh > 0.) zfac=-1.
         zuroff(NLON,jlat)=zfac*zcvel/zdx*ABS(zdh)**zcexp
        enddo

        do jlat=1,NLAT-1
         do jlon=1,NLON
          zdy=(ASIN(zsi(jlon,jlat+1))-ASIN(zsi(jlon,jlat)))*plarad
          zdh=(zoro(jlon,jlat+1)-zoro(jlon,jlat))/zdy
          zfac=1.
          if(zdh < 0.) zfac=-1.
          zvroff(jlon,jlat)=zfac*zcvel/zdy*ABS(zdh)**zcexp
         enddo
        enddo
        zvroff(1:NLON,NLAT)=0.

!calclate runoff map for genie embm (can only be zonal or meridional)
!        do jlat=1,nlat
!          do jlon=1,nlon
!           if(jlon.lt.nlon) then
!             d91=zoro(jlon,jlat)-zoro(jlon+1,jlat)
!           else
!             d91=zoro(jlon,jlat)-zoro(1,jlat)
!           endif
!           if(jlat.lt.nlat) then
!             d92=zoro(jlon,jlat)-zoro(jlon,jlat+1)
!           else
!             d92=-1.0e6
!           endif
!           if(jlon.gt.1) then
!             d93=zoro(jlon,jlat)-zoro(jlon-1,jlat)
!           else
!             d93=zoro(jlon,jlat)-zoro(nlon,jlat)
!           endif
!           if(jlat.gt.1) then
!             d94=zoro(jlon,jlat)-zoro(jlon,jlat-1)
!           else
!             d94=-1.0e6
!           endif
!NB ordering of following lines important for determing runoff into
!ocean when more than one adjacent ocean cells
!           if(d94.ge.max(d91,d92,d93)) k1(jlon,jlat)=94
!           if(d92.ge.max(d91,d93,d94)) k1(jlon,jlat)=92
!           if(d91.ge.max(d92,d93,d94)) k1(jlon,jlat)=91
!           if(d93.ge.max(d91,d92,d94)) k1(jlon,jlat)=93
!          enddo
!        enddo
! land sea mask
!        k1=k1*zlsm
!
!        open(unit=456,file='plasim.k1')
! 456    format(64i3)
!        do jlat=1,nlat
!         write(456,456) (k1(jlon,jlat),jlon=1,nlon)
!        enddo
!        close(456)
!        stop
        
!!FL        
!        open(62,file='roffuv.srv',form='unformatted')
!        write(62) 131,0,0,0,NLON,NLAT,0,0
!        write(62) zuroff
!        write(62) 132,0,0,0,NLON,NLAT,0,0
!        write(62) zvroff
!        write(62) 129,0,0,0,NLON,NLAT,0,0
!        write(62) zoro
!        write(62) 172,0,0,0,NLON,NLAT,0,0
!        write(62) zlsm
!        close(62)
!!
       endif

       call mpscgp(zuroff,duroff,1)
       call mpscgp(zvroff,dvroff,1)

       driver(:)=0.
       drunoff(:)=0.

      endif

      return
      end subroutine roffini

!     ===================
!     SUBROUTINE ROFFSTEP
!     ===================

      subroutine roffstep
      use landmod
      implicit none

!
!     make advection
!

      call mkradv(driver,drunoff,duroff,dvroff,darea,dls)

!
!     new water
!

      where(dls(:) > 0.) driver(:)=driver(:)+drunoff(:)*deltsec

      return
      end subroutine roffstep

!     =================
!     SUBROUTINE MKRADV
!     =================

      subroutine mkradv(zriver,zroff,zuroff,zvroff,zarea,zls)
      use landmod
      implicit none
      integer jlon

      real zarea(NLON,NLPP)
      real zuroff(NLON,NLPP)
      real zvroff(NLON,NLPP)
      real zriver(NLON,NLPP)
      real zrivin(NLON,NLPP)
      real zroff(NLON,NLPP)
      real zls(NLON,NLPP)

      real zrip(NLON,NLPP)
      real zrop(NLON,NLPP)

      real zrigl(NLON,NLAT),zriglp(NLON,NLAT)
      real zrogl(NLON,NLAT),zroglp(NLON,NLAT)
!

      zrivin(:,:)=zriver(:,:)*zarea(:,:)*zls(:,:)

!
!     make advection in zonal direction (no partitioning in mpp)
!

      do jlon=1,NLON-1
       where(zuroff(jlon,:) > 0.)
        zroff(jlon,:)=zroff(jlon,:)                                     &
     &               -zuroff(jlon,:)*zrivin(jlon,:)/zarea(jlon,:)
        zroff(jlon+1,:)=zroff(jlon+1,:)                                 &
     &                 +zuroff(jlon,:)*zrivin(jlon,:)/zarea(jlon+1,:)
       elsewhere
        zroff(jlon,:)=zroff(jlon,:)                                     &
     &               -zuroff(jlon,:)*zrivin(jlon+1,:)/zarea(jlon,:)
        zroff(jlon+1,:)=zroff(jlon+1,:)                                 &
     &                 +zuroff(jlon,:)*zrivin(jlon+1,:)/zarea(jlon+1,:)
       endwhere
      enddo
      where(zuroff(NLON,:) > 0.)
       zroff(NLON,:)=zroff(NLON,:)                                      &
     &              -zuroff(NLON,:)*zrivin(NLON,:)/zarea(NLON,:)
       zroff(1,:)=zroff(1,:)                                            &
     &           +zuroff(NLON,:)*zrivin(NLON,:)/zarea(1,:)
      elsewhere
       zroff(NLON,:)=zroff(NLON,:)                                      &
     &              -zuroff(NLON,:)*zrivin(1,:)/zarea(NLON,:)
       zroff(1,:)=zroff(1,:)                                            &
     &           +zuroff(NLON,:)*zrivin(1,:)/zarea(1,:)
      endwhere

!
!     make advection in meridional direction (partitioning in mpp)
!     simple schema (exchange all gps)
!

      call mpgagp(zrigl,zrivin,1)
      if(mypid==NROOT) then
       zriglp(:,1:NLAT-1)=zrigl(:,2:NLAT)
       zriglp(:,NLAT)=0.
      endif
      call mpscgp(zriglp,zrip,1)
      where(zvroff(:,:) < 0.)
       zroff(:,:)=zroff(:,:)+zvroff(:,:)*zrivin(:,:)/zarea(:,:)
       zrop(:,:)=-zvroff(:,:)*zrivin(:,:)
      elsewhere
       zroff(:,:)=zroff(:,:)+zvroff(:,:)*zrip(:,:)/zarea(:,:)
       zrop(:,:)=-zvroff(:,:)*zrip(:,:)
      endwhere
      call mpgagp(zroglp,zrop,1)
      if(mypid==NROOT) then
       zrogl(:,2:NLAT)=zroglp(:,1:NLAT-1)
       zrogl(:,1)=0.
      endif
      call mpscgp(zrogl,zrop,1)

      zroff(:,:)=zroff(:,:)+zrop(:,:)/zarea(:,:)

      return
      end subroutine mkradv

!     =================
!     SUBROUTINE GETTCL
!     =================

      subroutine gettcll
      use landmod
      implicit none
      integer jstep,jm1,jm2
      integer nmin,nhour,nday,nmonth,nyear
      real zgw1,zgw2
!
!     get surface temperature annual cycle
!
      jstep=nstep+1    !advance time step (ts = ts(t)+ dtsdt)
      call ntomin(jstep,nmin,nhour,nday,nmonth,nyear)
!
      if(nperpetual > 0) then
       nmonth=(nperpetual-1)/30.+1
       nday=nperpetual-nmonth*30
      endif
!
      jm1=nmonth
      if(nday.gt.15) then
       jm2=jm1+1
       if(jm1.eq.12) jm2=1
      else
       jm2=jm1-1
       if(jm1.eq.1) jm2=12
      endif
!
!*    interpolate
!
      zgw2=abs(nday-15)/30.
      zgw1=1.-zgw2
      dtclim(:)=zgw1*dtcl(:,jm1)+zgw2*dtcl(:,jm2)
      dwclim(:)=zgw1*dwcl(:,jm1)+zgw2*dwcl(:,jm2)
      return
      end subroutine gettcll

!     =================
!     SUBROUTINE GETALB
!     =================

      subroutine getalb
      use landmod
      implicit none
      integer jstep,jm1,jm2
      integer nmin,nhour,nday,nmonth,nyear
      real zgw1,zgw2
!
!     get surface background albedo from  annual cycle
!
      jstep=nstep+1    !advance time step (ts = ts(t)+ dtsdt)
      call ntomin(jstep,nmin,nhour,nday,nmonth,nyear)
!
      if(nperpetual > 0) then
       nmonth=(nperpetual-1)/30.+1
       nday=nperpetual-nmonth*30
      endif
!
      jm1=nmonth
      if(nday.gt.15) then
       jm2=jm1+1
       if(jm1.eq.12) jm2=1
      else
       jm2=jm1-1
       if(jm1.eq.1) jm2=12
      endif
!
!*    interpolate
!
      zgw2=abs(nday-15)/30.
      zgw1=1.-zgw2
      dalbclim(:)=zgw1*dalbcl(:,jm1)+zgw2*dalbcl(:,jm2)
!
      return
      end subroutine getalb


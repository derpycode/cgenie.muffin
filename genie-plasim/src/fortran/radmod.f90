      module radmod
!
!     radiation module for PUMA
!
!**   1) make global PUMA variables available
!
      use pumamod
      implicit none
!
!**   2) define global parameters for *subs* included in radmod
!
!*    2.0) version identifier (date)
!
      character(len=80) :: version = '27.09.2006 by Larry'
!
!*    2.1)  constant parameters
!
      real sbk
      parameter(SBK = 5.67E-8)  ! Stefan-Bolzman Const.

!
!*    2.2) namelist parameters (see *sub* radini)
!

      real    :: gsol0   = SOLCON_EARTH  ! solar constant
      real    :: solclat = 1.0    ! cos of lat of insolation if ncstsol=1
      real    :: solcdec = 1.0    ! cos of dec of insolation if ncstsol=1
      real    :: clgray  = -1.0   ! cloud grayness (-1 = computed)
      real    :: th2oc   = 0.04   ! absorption coefficient h2o continuum (lwr)
      real    :: tswr1   = 0.0641 ! tuning of cloud albedo range1
      real    :: tswr2   = 0.048  ! tuning of cloud back scattering c. range2
      real    :: tswr3   = 0.0045 ! tuning of cloud s. scattering alb. range2
      real    :: tpofmt  = 1.00   ! tuning of point of mean transmittance
      real    :: acllwr  = 0.100  ! mass absorption coefficient for clouds (lwr)
      real    :: a0o3    = 0.25   ! parameter to define o3 profile
      real    :: a1o3    = 0.11   ! parameter to define o3 profile
      real    :: aco3    = 0.08   ! parameter to define o3 profile
      real    :: bo3     = 20000. ! parameter to define o3 profile
      real    :: co3     = 5000.  ! parameter to define o3 profile
      real    :: toffo3  = 90.    ! parameter to define o3 profile
      integer :: no3     = 1      ! switch for ozon (1/0=yes/no)
      integer :: nsol    = 1      ! switch for solang (1/0=yes/no)
      integer :: nswr    = 1      ! switch for swr (1/0=yes/no)
      integer :: nlwr    = 1      ! switch for lwr (1/0=yes/no)
      integer :: nswrcl  = 1      ! switch for computed cloud props.(1/0=y/n)
      integer :: nrscat  = 1      ! switch for rayleigh scat. (1/0=yes/no)
      integer :: ndcycle = 0      ! switch for daily cycle of insolation
                                  !  0 = daily mean insolation)
      integer :: ntco2   = 0      ! switch for time-series input
                                  ! fixed co2 (=0) is default. Overwritten (=1) if namelist input for co2<0
      integer :: ncstsol = 0      ! switch to set constant insolation
                                  ! on the whole planet (0/1)=(off/on)
      integer :: iyrbp   = -50    ! Year before present (1950 AD)
                                  ! default = 2000 AD

      real :: rcl1(3)=(/0.15,0.30,0.60/) ! cloud albedos spectral range 1
      real :: rcl2(3)=(/0.15,0.30,0.60/) ! cloud albedos spectral range 2
      real :: acl2(3)=(/0.05,0.10,0.20/) ! cloud absorptivities spectral range 2

!
!*    2.3) arrays
!

      real gmu0(NHOR)                ! cosine of solar zenit angle
      real gmu1(NHOR)                ! cosine of solar zenit angle
      real dqo3(NHOR,NLEV)           ! ozon concentration (kg/kg)
      real dtdtlwr(NHOR,NLEV)        ! lwr temperature tendencies
      real dtdtswr(NHOR,NLEV)        ! swr temperature tendencies
      real dqo3cl(NHOR,NLEV,0:13)    ! climatological O3

!
!*    2.4) scalars
!

      real :: gdist2 = 1.        ! Earth-sun distance factor ( i.e. (1/r)**2 )
      real :: time4rad = 0.      ! CPU time for radiation
      real :: time4swr = 0.      ! CPU time for short wave radiation
      real :: time4lwr = 0.      ! CPU time for long wave radiation

!
!     2.5 orbital parameters
!
      integer, parameter :: ORB_UNDEF_INT  = 2000000000  
      real :: obliqr   ! Earth's obliquity in radians
      real :: lambm0   ! Mean longitude of perihelion at the
                       ! vernal equinox (radians)
      real :: mvelpp   ! Earth's moving vernal equinox longitude
                       ! of perihelion plus pi (radians)
      real :: eccf     ! Earth-sun distance factor ( i.e. (1/r)**2 )
      integer :: iyrad ! Year AD to calculate orbit for
      logical, parameter :: log_print = .true.
                       ! Flag to print-out status information or not.
                       ! (This turns off ALL status printing including)
                       ! (error messages.)
!
!     2.6 extended entropy/energy diagnostics
!
      real :: dftde1(NHOR,NLEP),dftde2(NHOR,NLEP)
      real :: dftue1(NHOR,NLEP),dftue2(NHOR,NLEP)
      real :: dftu0(NHOR,NLEP),dftd0(NHOR,NLEP)
!
!     auxiliary variables for solar zenit angle calculations
!
      real :: solclatcdec      ! cos(lat)*cos(decl) 
      real :: solslat          ! sin(lat)
      real :: solsdec          ! sin(decl)
      real :: solslatsdec      ! sin(lat)*sin(decl) 
      real :: zmuz             ! temporary zenit angle   
!
! PBH co2 temporal data
      real :: co2eff(100000)
      real :: co2act(100000)

      logical :: atchem_radfor

      character :: co2filename*100
      character :: co2emitfilename*100
      character :: radforfilename*100


      end module radmod

!
!     radiation subroutines
!

!     =================
!     SUBROUTINE RADINI
!     =================

      subroutine radini
      use radmod
      implicit none
      integer i,jtune,iyea
!
!     initialize radiation
!     this *sub* is called by PUMA (PUMA-interface)
!
!     this *sub* reads the radiation namelist *radpar*
!     and broadcasts the parameters
!
!     the following PUMA *subs* are used:
!
!     mpbci  : broadcasts 1 integer
!     mpbcr  : broadcasts 1 real
!
!     the following PUMA variables are used:
!
!     mypid  : process id (used for mpp)
!     nroot  : id of root process (used for mpp)
!
!**   0) define namelist
!
      namelist/radpar/ncstsol,solclat,solcdec,no3     &
     &               ,iyrbp,nswr,nlwr                                   &
     &               ,a0o3,a1o3,aco3,bo3,co3,toffo3                     &
     &               ,nsol,nswrcl,nrscat,rcl1,rcl2,acl2,clgray,tpofmt   &
     &               ,tswr3,dawn

      namelist/radpar_plasim_nml/                           &
               ndcycle,co2,tswr1,tswr2,acllwr,th2oc,gsol0,co2filename,  &
               atchem_radfor,                                           &
               ntco2e,co2emitfilename,nradfor,radforfilename

!     namelist parameter:
!
!     ndcycle : switch for daily cycle 1=on/0=off
!     ncstsol : switch to set constant insolation 
!     solclat : constant cosine of latitude of insolation 
!     solcdec : constant solar declination
!     no3     : switch for ozon 1=on/0=off
!     co2     : co2 concentration (ppmv)
!     gsol0   : solar constant (w/m2)
!     iyrbp   : Year before present (1950 AD); default = 2000 AD
!     nswr    : switch for short wave radiation (dbug) 1=on/0=off
!     nlwr    : switch for long wave radiation (dbug) 1=on/0=off
!     nsol    : switch for solar insolation (dbug) 1=on/0=off
!     nswrcl  : switch for computed or prescribed cloud props. 1=com/0=pres
!     nrscat  : switch for rayleigh scattering (dbug) 1=on/0=off
!     rcl1(3) : cloud albedos spectral range 1
!     rcl2(3) : cloud albedos spectral range 2
!     acl2(3) : cloud absorptivities spectral range 2
!     clgray  : cloud grayness
!     tpofmt  ! tuning of point of mean (lwr) transmissivity in layer
!     acllwr  ! mass absorption coefficient for clouds (lwr)
!     tswr1   ! tuning of cloud albedo range1
!     tswr2   ! tuning of cloud back scattering c. range2
!     tswr3   ! tuning of cloud s. scattering alb. range2
!     th2oc   ! absorption coefficient for h2o continuum
!     dawn    : zenith angle threshhold for night
!
!**   0) preset namelist parameter depending on model setup
!

      if (mars == 1) then
         no3   = 0            ! switch ozone off for Mars
         gsol0 = SOLCON_MARS
      endif

      jtune=0
      if(ndheat > 0) then 
       if(NTRU==21 .or. NTRU==1) then
        if(NLEV==5) then
         if(NDCYCLE==1) then
          jtune=0
         else
          if(NEQSIG==1) then
           jtune=0
          else
           tswr1=0.02
           tswr3=0.004
           th2oc=0.035
           jtune=1
          endif
         endif 
        elseif(NLEV==10) then
         if(NDCYCLE==1) then
          jtune=0
         else
          if(NEQSIG==1) then
           jtune=0
          else
           th2oc=0.03
           tswr1=0.0641
           tswr3=0.0045 
           jtune=1
          endif
         endif 
        endif
       elseif(NTRU==42) then
        if(NLEV==10) then
         if(NDCYCLE==1) then
          jtune=0
         else
          if(NEQSIG==1) then
           jtune=0
          else
           tswr1=0.085
           tswr3=0.0048
           th2oc=0.035
           jtune=1
          endif
         endif
        endif
       endif
      endif
!
      if(jtune==0) then
       if(mypid==NROOT) then
        print*,'No radiation setup for this resolution (NTRU,NLEV)'
        print*,'using default setup. You may need to tune the radiation'
       endif
      endif
!
!**   1) read and print version & namelist parameters
!
      iyrbp = 1950 - n_start_year

      if (mypid==NROOT) then
         read(11,radpar)
         write (*,'(/," ****************************************")')
         write (*,'(" * RADMOD ",a29," *")') trim(version)
         write (*,'(" ****************************************")')
         write (*,'(" * Namelist RADPAR from <puma_namelist> *")')
         write (*,'(" ****************************************")')
         write(*,radpar)
      endif ! (mypid==NROOT)

      open(unit=56,file='data_PLASIM')
      read(unit=56,NML=radpar_plasim_nml)
      write(*,radpar_plasim_nml)
      close(56)

!note atchem coupling does not apply first time step as plasim
!is initialised before biogem. Ensure co2 set to initial CO2
!even in a coupled experiment        

      co2veg=co2

      if(co2.lt.0) then
        print*,trim(co2filename)
!switch to turn on time series input
        ntco2=1
!PBH read co2 from input file
        print*,co2
        open(unit=54,file=trim(indir_name)//'/'//trim(co2filename))
        read(54,*) iyea
        print*,"CO2eq and CO2act timeseries"
        do i=1,iyea
          read(54,*) co2eff(i),co2act(i)
          print*,i,co2eff(i),co2act(i)
        enddo
        close(54)
        co2=co2eff(1)
        co2veg=co2act(1)
        print*,"Initial CO2 eff from timeseries input",co2
        print*,"Initial CO2 actual from timeseries input",co2veg
      else
        co2veg=co2
        print*,"Fixed CO2 effective",co2
        print*,"Fixed CO2 actual",co2veg
      endif
      if(ntco2e.eq.1) then
        print*,"co2 emissions"
        open(unit=54,file=trim(indir_name)//'/'//trim(co2emitfilename))
        read(54,*) iyea
        print*,"CO2eq emissions timeseries"
        do i=1,iyea
          read(54,*) co2emissions(i)
! covert GtC to moles
          co2emissions(i)=co2emissions(i)*(1.0e15/12.0)
!convert to /m2 NB using GENIE radius for carbon conservation
          co2emissions(i)=co2emissions(i)/(4.0*PI*plarad**2)
! convert /year to /second        
          co2emissions(i)=co2emissions(i)/(60.*60.*24.*n_days_per_year)
          print*,i,co2emissions(i)
        enddo
        close(54)
       else
         co2emissions=0.0
       endif
      if(nradfor.eq.1) then
        print*,"non-co2 forcing"
        open(unit=54,file=trim(indir_name)//'/'//trim(radforfilename))
        read(54,*) iyea
        print*,"non-CO2 forcing timeseries"
        do i=1,iyea
          read(54,*) radfor(i)
          print*,i,radfor(i)
        enddo
        close(54)
       else
         radfor=0.0
       endif


      

!
!     broadcast namelist parameter
!
      call mpbci(ndcycle)
      call mpbci(ncstsol)
      call mpbci(no3)
      call mpbcr(a0o3)
      call mpbcr(a1o3)
      call mpbcr(aco3)
      call mpbcr(bo3)
      call mpbcr(co3)
      call mpbcr(toffo3)
      call mpbcr(co2)
      call mpbcr(co2veg)
      call mpbcr(gsol0)
      call mpbcr(solclat)
      call mpbcr(solcdec)
      call mpbcr(clgray)
      call mpbcr(dawn)
      call mpbcr(th2oc)
      call mpbcr(tpofmt)
      call mpbcr(acllwr)
      call mpbcr(tswr1)
      call mpbcr(tswr2)
      call mpbcr(tswr3)
      call mpbcrn(rcl1,3)
      call mpbcrn(rcl2,3)
      call mpbcrn(acl2,3)
      call mpbci(iyrbp)
      call mpbci(nswr)
      call mpbci(nlwr)
      call mpbci(nsol)
      call mpbci(nrscat)
      call mpbci(nswrcl)

!
!     determine orbital parameters
!

      iyrad = 1950 - iyrbp
      if (nfixorb == 1) then ! fixed orbital params (default AMIP II)
         iyrad = ORB_UNDEF_INT
      endif
      call orb_params(iyrad, eccen, obliq, mvelp                          &
     &               ,obliqr, lambm0, mvelpp, log_print, mypid, nroot)

!
!     read climatological ozone
!
      if (no3 == 2) then
         call mpsurfgp('dqo3cl',dqo3cl,NHOR,NLEV*14)
      endif
!
      return
      end subroutine radini

!     ==================
!     SUBROUTINE RADSTEP
!     ==================

      subroutine radstep
      use radmod
      implicit none
      real zsec,zsec1
      integer jlev,jlep
!
!     do the radiation calculations
!     this *sub* is called by PUMA (PUMA-interface)
!
!     no PUMA *subs* are used
!
!     the following PUMA variables are used/modified:
!
!     ga               : gravity accelleration (m/s2) (used)
!     acpd             : specific heat of dry air (J/kgK) (used)
!     ADV              : ACPV/acpd - 1  (used)
!     sigma(NLEV)      : sigma of T-levels (used)
!     dp(NHOR)         : surface pressure (Pa) (used)
!     dq(NHOR,NLEP)    : specific humidity (kg/kg) (used)
!     dtdt(NHOR,NLEP)  : temperature tendencies (K/s) (modified)
!     dswfl(NHOR,NLEP) : short wave radiation (W/m2)  (modified)
!     dlwfl(NHOR,NLEP) : long wave radiation (W/m2)   (modified)
!     dfu(NHOR,NLEP)   : short wave radiation upward (W/m2) (modified)
!     dfd(NHOR,NLEP)   : short wave radiation downward (W/m2) (modified)
!     dftu(NHOR,NLEP)  : long wave radiation upward (W/m2) (modified)
!     dftd(NHOR,NLEP)  : long wave radiation downward (W/m2) (modified)
!     dflux(NHOR,NLEP) : total radiation (W/m2) (modified)
!
!     the following radiation *subs* are called:
!
!     solang           : calc. cosine of solar zenit angle
!     mko3             : calc. ozon distribution
!     swr              : calc. short wave radiation fluxes
!     lwr              : calc. long wave radiation fluxes
!
!
!**   0) define local arrays
!

      real zdtdt(NHOR,NLEV)    ! temperature tendency due to rad (K/s)

!
!     allocatable arrays for diagnostic
!

      real, allocatable :: zprf1(:,:)
      real, allocatable :: zprf2(:,:)
      real, allocatable :: zprf3(:,:)
      real, allocatable :: zprf4(:,:)
      real, allocatable :: zprf5(:,:)
      real, allocatable :: zprf6(:,:)
      real, allocatable :: zprf7(:,:)
      real, allocatable :: zprf8(:,:)
      real, allocatable :: zprf9(:,:)
      real, allocatable :: zprf10(:,:)
      real, allocatable :: zprf11(:,:)
      real, allocatable :: zprf12(:,:)
      real, allocatable :: zcc(:,:)
      real, allocatable :: zdtdte(:,:)
!
!     cpu time estimates
!
      if(ntime == 1) call mksecond(zsec,0.)

!PBH get co2 I PROBABLY NEED TO BROADCAST THIS??
      if (ntco2.eq.1) then
        co2=co2eff(ndatim(1))
        co2veg=co2act(ndatim(1))
      endif
      if(atchem_radfor) then
        co2=genie_co2
        co2veg=genie_co2
      endif
      if(nradfor.eq.1) then
!CO2eff for non-CO2 radfor
        co2=5.35*log(co2/280.0)
        co2=co2+radfor(ndatim(1))
        co2=280.0*exp(co2/5.35)
      endif

!
!**   1) set all fluxes to zero
!

      dfu(:,:)   = 0.0         ! short wave radiation upward
      dfd(:,:)   = 0.0         ! short wave radiation downward
      dftu(:,:)  = 0.0         ! long wave radiation upward
      dftd(:,:)  = 0.0         ! long wave radiation downward
      dswfl(:,:) = 0.0         ! total short wave radiation
      dlwfl(:,:) = 0.0         ! total long wave radiation
      dftue1(:,:)=0.0          ! entropy
      dftue2(:,:)=0.0          ! entropy
!
!**   2) compute cosine of solar zenit angle for each gridpoint
!
      if(nsol==1) call solang_plasim
!
!**   3) compute ozon distribution
!
      if(no3<3) call mko3
!
!**   4) short wave radiation
!
!     a) if clear sky diagnostic is switched on:
!

      if(ndiagcf > 0) then
       allocate(zcc(NHOR,NLEP))
       zcc(:,:)=dcc(:,:)
       dcc(:,:)=0.
       if(nswr==1) call swr
       dclforc(:,1)=dswfl(:,NLEP)
       dclforc(:,3)=dswfl(:,1)
       dclforc(:,5)=dfu(:,1)
       dclforc(:,6)=dfu(:,NLEP)
       dcc(:,:)=zcc(:,:)
      end if

!
!     b) normal computation
!

      if(ntime == 1) call mksecond(zsec1,0.)
      if(nswr==1) call swr
      if(ntime == 1) then
       call mksecond(zsec1,zsec1)
       time4swr=time4swr+zsec1
      endif

!
!**   5) long wave radiation
!
!
!     a) if clear sky diagnostic is switched on:
!

      if(ndiagcf > 0) then
       zcc(:,:)=dcc(:,:)
       dcc(:,:)=0.
       if(nlwr==1) call lwr
       dclforc(:,2)=dlwfl(:,NLEP)
       dclforc(:,4)=dlwfl(:,1)
       dclforc(:,7)=dftu(:,NLEP)
       dcc(:,:)=zcc(:,:)
       deallocate(zcc)
      end if

!
!     b) normal computation
!

      if(ntime == 1) call mksecond(zsec1,0.)
      if(nlwr==1) call lwr
      if(ntime == 1) then
       call mksecond(zsec1,zsec1)
       time4lwr=time4lwr+zsec1
      endif

!
!**   6) Total flux
!

      dflux(:,:)=dlwfl(:,:)+dswfl(:,:)

!
!**   7) compute tendencies and add them to PUMA dtdt
!

      do jlev = 1 , NLEV
       jlep=jlev+1
       zdtdt(:,jlev)=-ga*(dflux(:,jlep)-dflux(:,jlev))                  &
     &              /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
       dtdt(:,jlev)=dtdt(:,jlev)+zdtdt(:,jlev)
       dtdtswr(:,jlev)=-ga*(dswfl(:,jlep)-dswfl(:,jlev))                &
     &              /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
       dtdtlwr(:,jlev)=-ga*(dlwfl(:,jlep)-dlwfl(:,jlev))                &
     &              /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
      enddo

!
!**   8) dbug printout if nprint=2 (see pumamod)
!

      if (nprint==2) then
       allocate(zprf1(NLON*NLAT,NLEP))
       allocate(zprf2(NLON*NLAT,NLEP))
       allocate(zprf3(NLON*NLAT,NLEP))
       allocate(zprf4(NLON*NLAT,NLEP))
       allocate(zprf5(NLON*NLAT,NLEP))
       allocate(zprf6(NLON*NLAT,NLEP))
       allocate(zprf7(NLON*NLAT,NLEP))
       allocate(zprf8(NLON*NLAT,NLEV))
       allocate(zprf9(NHOR,NLEV))
       allocate(zprf10(NHOR,NLEV))
       allocate(zprf11(NLON*NLAT,NLEV))
       allocate(zprf12(NLON*NLAT,NLEV))
       call mpgagp(zprf1,dfd,NLEP)
       call mpgagp(zprf2,dfu,NLEP)
       call mpgagp(zprf3,dswfl,NLEP)
       call mpgagp(zprf4,dftd,NLEP)
       call mpgagp(zprf5,dftu,NLEP)
       call mpgagp(zprf6,dlwfl,NLEP)
       call mpgagp(zprf7,dflux,NLEP)
       call mpgagp(zprf8,zdtdt,NLEV)
       do jlev = 1 , NLEV
        jlep=jlev+1
        zprf9(:,jlev)=-ga*(dswfl(:,jlep)-dswfl(:,jlev))                 &
     &               /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        zprf10(:,jlev)=-ga*(dlwfl(:,jlep)-dlwfl(:,jlev))                &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
       enddo
       call mpgagp(zprf11,zprf9,NLEV)
       call mpgagp(zprf12,zprf10,NLEV)
       if(mypid==NROOT) then
        do jlev=1,NLEP
         print*,'L= ',jlev,' swd= ',zprf1(nprhor,jlev)                  &
     &                    ,' swu= ',zprf2(nprhor,jlev)                  &
     &                    ,' swt= ',zprf3(nprhor,jlev)
         print*,'L= ',jlev,' lwd= ',zprf4(nprhor,jlev)                  &
     &                    ,' lwu= ',zprf5(nprhor,jlev)                  &
     &                    ,' lwt= ',zprf6(nprhor,jlev)
         print*,'L= ',jlev,' totalflux= ',zprf7(nprhor,jlev)
        enddo
        do jlev=1,NLEV
         print*,'L= ',jlev,' dtdt= ',zprf8(nprhor,jlev)                 &
     &                    ,' dtsw= ',zprf11(nprhor,jlev)                &
     &                    ,' dtlw= ',zprf12(nprhor,jlev)
        enddo
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
       deallocate(zprf10)
       deallocate(zprf11)
       deallocate(zprf12)
      endif

!
!     franks dbug
!

      if(ndiaggp==1) then
       do jlev = 1 , NLEV
        jlep=jlev+1
        dgp3d(:,jlev,5)=-ga*(dlwfl(:,jlep)-dlwfl(:,jlev))               &
     &               /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        dgp3d(:,jlev,6)=-ga*(dswfl(:,jlep)-dswfl(:,jlev))               &
     &               /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
       enddo
      end if
!
!     entropy/energy diagnostics
!
      if(nentropy > 0) then
       dentropy(:,16)=dlwfl(:,NLEP)/dt(:,NLEP)
       dentropy(:,17)=dswfl(:,NLEP)/dt(:,NLEP)
       dentropy(:,27)=dftd(:,NLEP)/dt(:,NLEP)
       dentropy(:,28)=dftu(:,NLEP)/dt(:,NLEP)
       dentropy(:,29)=0.
       dentropy(:,30)=0.
       do jlev=1,NLEV
        dentropy(:,29)=dentropy(:,29)+dftu0(:,jlev)/dentrot(:,jlev)
        dentropy(:,30)=dentropy(:,30)+dftd0(:,jlev)/dentrot(:,jlev)
       enddo
       allocate(zdtdte(NHOR,NLEV))
       dentropy(:,9)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dlwfl(:,jlep)-dlwfl(:,jlev))                &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        if(nentropy > 2) then
        dentropy(:,9)=dentropy(:,9)                                     &
     &         +zdtdte(:,jlev)/dt(:,jlev)                               &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
        dentropy(:,9)=dentropy(:,9)                                     &
     &         +zdtdte(:,jlev)/dentrot(:,jlev)                          &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       dentropy(:,21)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftd(:,jlep)-dftd(:,jlev))                  &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        if(nentropy > 2) then
        dentropy(:,21)=dentropy(:,21)                                   &
     &         +zdtdte(:,jlev)/dt(:,jlev)                               &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
        dentropy(:,21)=dentropy(:,21)                                   &
     &         +zdtdte(:,jlev)/dentrot(:,jlev)                          &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       dentropy(:,22)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftu(:,jlep)-dftu(:,jlev))                  &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        if(nentropy > 2) then
        dentropy(:,22)=dentropy(:,22)                                   &
     &         +zdtdte(:,jlev)/dt(:,jlev)                               &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
        dentropy(:,22)=dentropy(:,22)                                   &
     &         +zdtdte(:,jlev)/dentrot(:,jlev)                          &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       dentropy(:,23)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftue1(:,jlep)-dftue1(:,jlev))              &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        if(nentropy > 2) then
        dentropy(:,23)=dentropy(:,23)                                   &
     &         +zdtdte(:,jlev)/dt(:,jlev)                               &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
        dentropy(:,23)=dentropy(:,23)                                   &
     &         +zdtdte(:,jlev)/dentrot(:,jlev)                          &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       dentropy(:,24)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftue2(:,jlep)-dftue2(:,jlev))              &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        if(nentropy > 2) then
        dentropy(:,24)=dentropy(:,24)                                   &
     &         +zdtdte(:,jlev)/dt(:,jlev)                               &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
        dentropy(:,24)=dentropy(:,24)                                   &
     &         +zdtdte(:,jlev)/dentrot(:,jlev)                          &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       dentropy(:,26)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftue2(:,jlep)-dftue2(:,jlev))              &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        dentropy(:,26)=dentropy(:,26)                                   &
     &              +zdtdte(:,jlev)                                     &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       dentropy(:,25)=(dftu(:,NLEP)+dentropy(:,26))/dt(:,NLEP)
       dentropy(:,26)=-dentropy(:,26)/dt(:,NLEP)
!
       dentropy(:,10)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dswfl(:,jlep)-dswfl(:,jlev))                &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        if(nentropy > 2) then
        dentropy(:,10)=dentropy(:,10)                                   &
     &         +zdtdte(:,jlev)/dt(:,jlev)                               &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
        dentropy(:,10)=dentropy(:,10)                                   &
     &         +zdtdte(:,jlev)/dentrot(:,jlev)                          &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       deallocate(zdtdte)
      endif
      if(nenergy > 0) then
       allocate(zdtdte(NHOR,NLEV))
       denergy(:,9)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dlwfl(:,jlep)-dlwfl(:,jlev))                &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        denergy(:,9)=denergy(:,9)                                       &
     &              +zdtdte(:,jlev)                                     &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       denergy(:,17)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftd(:,jlep)-dftd(:,jlev))                  &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        denergy(:,17)=denergy(:,17)                                     &
     &              +zdtdte(:,jlev)                                     &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       denergy(:,18)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftu(:,jlep)-dftu(:,jlev))                  &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        denergy(:,18)=denergy(:,18)                                     &
     &              +zdtdte(:,jlev)                                     &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       denergy(:,19)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftue1(:,jlep)-dftue1(:,jlev))              &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        denergy(:,19)=denergy(:,19)                                     &
     &              +zdtdte(:,jlev)                                     &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       denergy(:,20)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dftue2(:,jlep)-dftue2(:,jlev))              &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        denergy(:,20)=denergy(:,20)                                     &
     &              +zdtdte(:,jlev)                                     &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       denergy(:,24)=0.
       do jlev=1,NLEV
        denergy(:,24)=denergy(:,24)+dt(:,jlev)*dsigma(jlev)
       enddo
       denergy(:,10)=0.
       do jlev=1,NLEV
        jlep=jlev+1
        zdtdte(:,jlev)=-ga*(dswfl(:,jlep)-dswfl(:,jlev))                &
     &                /(dsigma(jlev)*dp(:)*acpd*(1.+ADV*dq(:,jlev)))
        denergy(:,10)=denergy(:,10)                                     &
     &               +zdtdte(:,jlev)                                    &
     &               *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       deallocate(zdtdte)
      endif

      if(ntime == 1) then
       call mksecond(zsec,zsec)
       time4rad=time4rad+zsec
      endif

      return
      end subroutine radstep

!     ==================
!     SUBROUTINE RADSTOP
!     ==================

      subroutine radstop
      use radmod
      implicit none
!
!     finalizes radiation
!     this *sub* is called by PUMA (PUMA-interface)
!
!     for the inclosed parameterizations only a dummy *sub* is needed
!
!     no PUMA *subs* are used
!
!     no PUMA variables are used
!
      if(mypid == NROOT .and. ntime == 1) then
       print*,'******************************************'
       print*,' CPU usage in RADSTEP (ROOT process only):  '
       print*,'    All routines : ',time4rad,' s'
       print*,'    Short wave   : ',time4swr,' s'
       print*,'    Long  wave   : ',time4lwr,' s'
       print*,'******************************************'
      endif

      return
      end subroutine radstop

!     =================
!     SUBROUTINE SOLANG
!     =================

      subroutine solang_plasim
      use radmod
      implicit none
      real zcday,zdecl,zdawn,zrlon,zrtim,zmins,zhangle
      integer imin,ihou,iday,imon,iyea,jhor,jlat,jlon
!
!     compute cosine of zenit angle including daily cycle
!
!     the following PUMA *subs* are used:
!
!     ndayofyear : compute date and time from PUMA timestep
!
!     the following PUMA variables are used:
!
!     PI         : pi=3.14...
!     nstep      : PUMA time step
!     sid(NLPP)  : sines of gaussian latitudes
!     csq(NLPP)  : cosine**2 of gaussian latitudes
!     cola(NLPP) : cosine of latitude
!
!
!**   1) compute day of the year and hour of the day
!
      interface
         integer function ndayofyear(kstep)
            integer kstep
         end function ndayofyear
      end interface

      if (nperpetual > 0) then
         zcday = nperpetual
      else
         zcday = ndayofyear(nstep) ! from calmod
      endif

      call ntomin(nstep,imin,ihou,iday,imon,iyea)
!
!**   2) compute declination [radians]
!
      call orb_decl(zcday, eccen, mvelpp, lambm0, obliqr, zdecl, eccf)
!
!**   3) compute zenith angle
!
      gmu0(:) = 0.0
      zmuz    = 0.0
      zdawn = sin(dawn * PI / 180.0) ! compute dawn/dusk angle 
      zrlon = TWOPI / NLON           ! scale lambda to radians
      zrtim = TWOPI / 1440.0         ! scale time   to radians
      zmins = ihou * 60 + imin
      jhor = 0
      if (ncstsol==0) then
       do jlat = 1 , NLPP
        do jlon = 0 , NLON-1
         jhor = jhor + 1
         zhangle = zmins * zrtim + jlon * zrlon - PI
         zmuz=sin(zdecl)*sid(jlat)+cola(jlat)*cos(zdecl)*cos(zhangle)
         if (zmuz > zdawn) gmu0(jhor) = zmuz
        enddo
       enddo
      else
       solclatcdec=solclat*solcdec
       solslat=sqrt(1-solclat*solclat)
       solsdec=sqrt(1-solcdec*solcdec)
       solslatsdec=solslat*solsdec
       do jlat = 1 , NLPP
        do jlon = 0 , NLON-1
         jhor = jhor + 1
         if (ndcycle == 1) then 
          zhangle = zmins * zrtim - PI
          zmuz=solslatsdec+solclatcdec*cos(zhangle)
         else
          zmuz=solslatsdec+solclatcdec/PI
         endif
         if (zmuz > zdawn) gmu0(jhor) = zmuz
        enddo
       enddo
      endif
!
!**  4) copy earth-sun distance (1/r**2) to radmod
!
      gdist2=eccf

      return
      end subroutine solang_plasim


!     ===============
!     SUBROUTINE MKO3
!     ===============

      subroutine mko3
      use radmod
      implicit none
      integer jlat,jh1,jh2,jlev
      real zconst
!
!     compute ozon distribution
!
!     the following PUMA *subs* are used:
!
!     ntomin : compute date and time from PUMA timestep
!
!     the following PUMA variables are used:
!
!     TWOPI    : 2*PI
!     nstep    : PUMA time step
!     sid(NLAT): sines of gaussian latitudes
!
!     local parameter and arrays
!
      interface
         integer function ndayofyear(kstep)
            integer kstep
         end function ndayofyear
      end interface

      real zt0,zro3,zfo3,zcday
      parameter(zt0=255.)
      parameter(zro3=2.14)
      parameter(zfo3=100./zro3)
!
      real za(NHOR)
      real zh(NHOR),zdh(NHOR)
      real zo3t(NHOR),zo3(NHOR)

!**   1) compute day of the year

      if(no3==1) then

       zcday = ndayofyear(nstep) ! see calmod
!
       do jlat=1,NLPP
        jh1=(jlat-1)*NLON+1
        jh2=(jlat)*NLON
        za(jh1:jh2)=a0o3+a1o3*ABS(sid(jlat))                              &
     &           +aco3*sid(jlat)*cos(TWOPI*(zcday-toffo3)/n_days_per_year)
       enddo

       zconst=EXP(-bo3/co3)
       zo3t(:)=za(:)
       zh(:)=0.
       do jlev=NLEV,2,-1
        zdh(:)=-dt(:,jlev)*GASCON/ga*ALOG(sigmah(jlev-1)/sigmah(jlev))
        zh(:)=zh(:)+zdh(:)
        zo3(:)=-(za(:)+za(:)*zconst)/(1.+exp((zh(:)-bo3)/co3))            &
     &        +zo3t(:)
        dqo3(:,jlev)=zo3(:)*ga/(zfo3*dsigma(jlev)*dp(:))
        zo3t(:)=zo3t(:)-zo3(:)
       enddo
       dqo3(:,1)=zo3t(:)*ga/(zfo3*dsigma(1)*dp(:))

      elseif(no3==2) then

       call geto3(dqo3)

      else

       dqo3(:,:)=0.

      end if

      return
      end subroutine mko3

!     ================
!     SUBROUTINE GETO3
!     ================

      subroutine geto3(pqo3)
      use radmod
      implicit none
      integer nmin,nhour,nday,nmonth,nyear,jm
      real zw
!
      real pqo3(NHOR,NLEV)
!
!     get ozone annual cycle for time step (ts = ts(t)+ dtsdt)
!
      if (nperpetual > 0) then
         call yday2mmdd(nperpetual,nmonth,nday)
      else
         call ntomin(nstep+1,nmin,nhour,nday,nmonth,nyear)
      endif

      if (nday > 15) then
         jm = mod(nmonth   ,12) + 1
         zw = (nday - 15) / 30.0
      else
         jm = mod(nmonth+10,12) + 1
         zw = (15 - nday) / 30.0
      endif

      pqo3(:,:) = (1.0 - zw) * dqo3cl(:,:,nmonth) + zw * dqo3cl(:,:,jm)

      return
      end subroutine geto3

!     ==============
!     SUBROUTINE SWR
!     ==============

      subroutine swr
      use radmod
      implicit none
      integer js,je,jlat,jlev
      integer icnt
      real zsum,zmu00,zb3,zb4,zb5
!
!     calculate short wave radiation fluxes
!
!     this parameterization of sw-radiation bases on transmissivities
!     from Lacis & Hansen (1974) for clear sky (H2O,O3,Rayleigh)
!     and Stephens (1978) + Stephens et al. (1984) for clouds.
!     for the verical integration, the adding method is used.
!     some aspects of the realisation are taken from
!     'a simple radiation parameterization for use in mesoscale models'
!     by S. Bakan (Max-Planck Institut fuer Meteorologie)
!     (unfortunately neither published nor finished)
!
!     no PUMA *subs* are used
!
!     the following PUMA variables are used/modified:
!
!     ga               : gravity acceleration (m/s2) (used)
!     sigma(NLEV)      : full level sigma  (used)
!     sigmah(NLEV)     : half level sigma  (used)
!     dsigma(NLEV)     : delta sigma (half level)  (used)
!     dp(NHOR)         : surface pressure (Pa) (used)
!     dalb(NHOR)       : surface albedo (used)
!     dq(NHOR,NLEP)    : specific humidity (kg/kg) (used)
!     dql(NHOR,NLEP)   : cloud liquid water content (kg/kg) (used)
!     dcc(NHOR,NLEP)   : cloud cover (frac.) (used)
!     dswfl(NHOR,NLEP) : short wave radiation (W/m2)  (modified)
!     dfu(NHOR,NLEP)   : short wave radiation upward (W/m2) (modified)
!     dfd(NHOR,NLEP)   : short wave radiation downward (W/m2) (modified)
!
!     0) define local parameters and arrays
!
      real zero,zsolar1,zsolar2,zbetta,zmbar,zro3,zfo3
      parameter(zero=1.E-6)     ! if insolation < zero : fluxes=0.
      parameter(zsolar1=0.517)  ! spectral partitioning 1 (wl < 0.75mue)
      parameter(zsolar2=0.483)  ! spectral partitioning 2 (wl > 0.75mue)
      parameter(zbetta=1.66)    ! magnification factor water vapour
      parameter(zmbar=1.9)      ! magnification factor ozon
      parameter(zro3=2.14)      ! ozon density (kg/m**3 STP)
      parameter(zfo3=100./zro3) ! transfere o3 to cm STP
!
      real zt1(NHOR,NLEP),zt2(NHOR,NLEP)    ! transmissivities 1-l
      real zr1s(NHOR,NLEP),zr2s(NHOR,NLEP)  ! reflexivities l-1 (scattered)
      real zrl1(NHOR,NLEP),zrl2(NHOR,NLEP)  ! reflexivities l-NL (direct)
      real zrl1s(NHOR,NLEP),zrl2s(NHOR,NLEP)! reflexivities l-NL (scattered)
!
      real ztb1(NHOR,NLEV),ztb2(NHOR,NLEV)   ! layer transmissivity (down)
      real ztb1u(NHOR,NLEV),ztb2u(NHOR,NLEV) ! layer transmissivity (up)
      real zrb1(NHOR,NLEV),zrb2(NHOR,NLEV)   ! layer reflexivity (direct)
      real zrb1s(NHOR,NLEV),zrb2s(NHOR,NLEV) ! layer reflexibity (scattered)
!
      real zo3l(NHOR,NLEV)   ! ozon amount (top-l)
      real zxo3l(NHOR,NLEV)  ! effective ozon amount (top-l)
      real zwvl(NHOR,NLEV)   ! water vapor amount (top-l)
      real zywvl(NHOR,NLEV)  ! effective water vapor amount (top-l)
      real zrcs(NHOR,NLEV)   ! clear sky reflexivity (downward beam)
      real zrcsu(NHOR,NLEV)  ! clear sky reflexivity (upward beam)
!
      real zftop1(NHOR),zftop2(NHOR) ! top solar radiation
      real zfu1(NHOR),zfu2(NHOR)     ! upward fluxes
      real zfd1(NHOR),zfd2(NHOR)     ! downward fluxes
!
      real zmu0(NHOR)              ! zenit angle
      real zmu1(NHOR)              ! zenit angle
      real zcs(NHOR)               ! clear sky part
      real zm(NHOR)                ! magnification factor
      real zo3(NHOR)               ! ozon amount
      real zo3t(NHOR)              ! total ozon amount (top-sfc)
      real zxo3t(NHOR)             ! effective total ozon amount (top-sfc)
      real zto3(NHOR),zto3u(NHOR)  ! ozon transmissivity (downward/upward beam)
      real zto3t(NHOR),zto3tu(NHOR)! total ozon transmissivities (d/u)
      real zwv(NHOR)               ! water vapor amount
      real zwvt(NHOR)              ! total water vapor amount (top-sfc)
      real zywvt(NHOR)             ! total effective water vapor amount (top-sfc)
      real ztwv(NHOR),ztwvu(NHOR)  ! water vapor trasmissivity (d/u)
      real ztwvt(NHOR),ztwvtu(NHOR)! total water vapor transmissivities (d/u)
!
      real zra1(NHOR),zra2(NHOR)   ! reflexivities combined layer (direct)
      real zra1s(NHOR),zra2s(NHOR) ! reflexivities combined layer (scatterd)
      real zta1(NHOR),zta2(NHOR)   ! transmissivities combined layer (di)
      real zta1s(NHOR),zta2s(NHOR) ! transmissivities combined layer (sc)
      real z1mrabr(NHOR)           ! 1/(1.-rb*ra(*))
!
      real zrcl1(NHOR,NLEV),zrcl2(NHOR,NLEV)  ! cloud reflexivities (direct)
      real zrcl1s(NHOR,NLEV),zrcl2s(NHOR,NLEV)! cloud reflexivities (scattered)
      real ztcl2(NHOR,NLEV),ztcl2s(NHOR,NLEV) ! cloud transmissivities
!
!     arrays for diagnostic cloud properties
!
      real :: zlwp(NHOR)
      real :: ztau(NHOR)
      real :: zlog(NHOR)
      real zb2(NHOR),zom0(NHOR),zuz(NHOR),zun(NHOR),zr(NHOR)
      real zexp(NHOR),zu(NHOR),zb1(NHOR)
!
      logical losun(NHOR)         ! flag for gridpoints with insolation
!
!     cosine of zenith angle
!
      if (ndcycle == 0) then ! compute zonal means
         js = 1
         je = NLON
         do jlat = 1 , NLPP
            icnt = count(gmu0(js:je) > 0.0)
            if (icnt > 0) then
               zsum = sum(gmu0(js:je))
               zmu0(js:je) = zsum / icnt ! used for clouds
               zmu1(js:je) = zsum / NLON ! used for insolation
            else
               zmu0(js:je) = 0.0
               zmu1(js:je) = 0.0
            endif
            js = js + NLON
            je = je + NLON
         enddo ! jlat
      else
         zmu0(:) = gmu0(:)
         zmu1(:) = gmu0(:)
      endif ! (ndcycle == 0)
!
!     top solar radiation downward
!
      zftop1(:) = zsolar1 * gsol0 * gdist2 * zmu1(:)
      zftop2(:) = zsolar2 * gsol0 * gdist2 * zmu1(:)

!     from this point on, all computations are made only for
!     points with solar insolation > zero
!
      losun(:) = (zftop1(:) + zftop2(:) > zero)
!
!     cloud properites
!
      zcs(:) = 1.0 ! Clear sky fraction (1.0 = clear sky)
      zmu00  = 0.5
      zb3    = tswr1 * SQRT(zmu00) / zmu00
      zb4    = tswr2 * SQRT(zmu00)
      zb5    = tswr3 * zmu00 * zmu00
!
!     prescribed
!
      if (nswrcl == 0) then
       do jlev=1,NLEV
        if(sigma(jlev) <= 1./3.) then
         zrcl1s(:,jlev)=rcl1(1)/(rcl1(1)+zmu00)
         zrcl1(:,jlev)=zcs(:)*rcl1(1)/(rcl1(1)+zmu0(:))                 &
     &                +(1.-zcs(:))*zrcl1s(:,jlev)
         zrcl2s(:,jlev)=AMIN1(1.-acl2(1),rcl2(1)/(rcl2(1)+zmu00))
         zrcl2(:,jlev)=AMIN1(1.-acl2(1),zcs(:)*rcl2(1)/(rcl2(1)+zmu0(:))&
     &                                +(1.-zcs(:))*zrcl2s(:,jlev))
         ztcl2s(:,jlev)=1.-zrcl2s(:,jlev)-acl2(1)
         ztcl2(:,jlev)=1.-zrcl2(:,jlev)-acl2(1)
        elseif(sigma(jlev) > 1./3. .and. sigma(jlev) <= 2./3.) then
         zrcl1s(:,jlev)=rcl1(2)/(rcl1(2)+zmu00)
         zrcl1(:,jlev)=zcs(:)*rcl1(2)/(rcl1(2)+zmu0(:))                 &
     &                +(1.-zcs(:))*zrcl1s(:,jlev)
         zrcl2s(:,jlev)=AMIN1(1.-acl2(2),rcl2(2)/(rcl2(2)+zmu00))
         zrcl2(:,jlev)=AMIN1(1.-acl2(2),zcs(:)*rcl2(2)/(rcl2(2)+zmu0(:))&
     &                                 +(1.-zcs(:))*zrcl2s(:,jlev))
         ztcl2s(:,jlev)=1.-zrcl2s(:,jlev)-acl2(2)
         ztcl2(:,jlev)=1.-zrcl2(:,jlev)-acl2(2)
        else
         zrcl1s(:,jlev)=rcl1(3)/(rcl1(3)+zmu00)
         zrcl1(:,jlev)=zcs(:)*rcl1(3)/(rcl1(3)+zmu0(:))                 &
     &                +(1.-zcs(:))*zrcl1s(:,jlev)
         zrcl2s(:,jlev)=AMIN1(1.-acl2(3),rcl2(3)/(rcl2(3)+zmu00))
         zrcl2(:,jlev)=AMIN1(1.-acl2(3),zcs(:)*rcl2(3)/(rcl2(3)+zmu0(:))&
     &                                 +(1.-zcs(:))*zrcl2s(:,jlev))
         ztcl2s(:,jlev)=1.-zrcl2s(:,jlev)-acl2(3)
         ztcl2(:,jlev)=1.-zrcl2(:,jlev)-acl2(3)
        endif
        zcs(:)=zcs(:)*(1.-dcc(:,jlev))
       enddo
      else
       zrcl1(:,:)=0.0
       zrcl2(:,:)=0.0
       ztcl2(:,:)=1.0
       zrcl1s(:,:)=0.0
       zrcl2s(:,:)=0.0
       ztcl2s(:,:)=1.0
       do jlev=1,NLEV
        where(losun(:) .and. (dcc(:,jlev) > 0.))
         zlwp(:) = min(1000.0,1000.*dql(:,jlev)*dp(:)/ga*dsigma(jlev))
         ztau(:) = 2.0 * ALOG10(zlwp(:)+1.5)**3.9
         zlog(:) = log(1000.0 / ztau(:))
         zb2(:)  = zb4 / ALOG(3.+0.1*ztau(:))
         zom0(:) = min(0.9999,1.0 - zb5 * zlog(:))
         zun(:)  = 1.0 - zom0(:)
         zuz(:)  = zun(:) + 2.0 * zb2(:) * zom0(:)
         zu(:)   = SQRT(zuz(:)/zun(:))
         zexp(:) = exp(min(25.0,ztau(:)*SQRT(zuz(:)*zun(:))/zmu00))
         zr(:)   = (zu(:)+1.)*(zu(:)+1.)*zexp(:)                      &
     &           - (zu(:)-1.)*(zu(:)-1.)/zexp(:)
         zrcl1s(:,jlev)=1.-1./(1.+zb3*ztau(:))
         ztcl2s(:,jlev)=4.*zu(:)/zr(:)
         zrcl2s(:,jlev)=(zu(:)*zu(:)-1.)/zr(:)*(zexp(:)-1./zexp(:))

         zb1(:)  = tswr1*SQRT(zmu0(:))
         zb2(:)  = tswr2*SQRT(zmu0(:))/ALOG(3.+0.1*ztau(:))
         zom0(:) = min(0.9999,1.-tswr3*zmu0(:)*zmu0(:)*zlog(:))
         zun(:)  = 1.0 - zom0(:)
         zuz(:)  = zun(:) + 2.0 * zb2(:) * zom0(:)
         zu(:)   = SQRT(zuz(:)/zun(:))
         zexp(:) = exp(min(25.0,ztau(:)*SQRT(zuz(:)*zun(:))/zmu0(:)))
         zr(:)   = (zu(:)+1.)*(zu(:)+1.)*zexp(:)                      &
     &           - (zu(:)-1.)*(zu(:)-1.)/zexp(:)
         zrcl1(:,jlev)=1.-1./(1.+zb1(:)*ztau(:)/zmu0(:))
         ztcl2(:,jlev)=4.*zu(:)/zr(:)
         zrcl2(:,jlev)=(zu(:)*zu(:)-1.)/zr(:)*(zexp(:)-1./zexp(:))
         zrcl1(:,jlev)=zcs(:)*zrcl1(:,jlev)+(1.-zcs(:))*zrcl1s(:,jlev)
         ztcl2(:,jlev)=zcs(:)*ztcl2(:,jlev)+(1.-zcs(:))*ztcl2s(:,jlev)
         zrcl2(:,jlev)=zcs(:)*zrcl2(:,jlev)+(1.-zcs(:))*zrcl2s(:,jlev)
        endwhere
        zcs(:)=zcs(:)*(1.-dcc(:,jlev))
       enddo ! jlev
      endif ! (nswrcl == 0)
!
!     magnification factor
!
      where(losun(:))
       zm(:)=35./SQRT(1.+1224.*zmu0(:)*zmu0(:))
!
!     absorber amount and clear sky fraction
!
       zcs(:)=1.
       zo3t(:)=0.
       zxo3t(:)=0.
       zwvt(:)=0.
       zywvt(:)=0.
      endwhere
      do jlev=1,NLEV
       where(losun(:))
        zo3(:)=zfo3*dsigma(jlev)*dp(:)*dqo3(:,jlev)/ga
        zo3t(:)=zo3t(:)+zo3(:)
        zxo3t(:)=zcs(:)*(zxo3t(:)+zm(:)*zo3(:))                         &
     &          +(1.-zcs(:))*(zxo3t(:)+zmbar*zo3(:))
        zo3l(:,jlev)=zo3t(:)
        zxo3l(:,jlev)=zxo3t(:)
        zwv(:)=0.1*dsigma(jlev)*dq(:,jlev)*dp(:)/ga                     &
     &        *SQRT(273./dt(:,jlev))*sigma(jlev)*dp(:)/100000.
        zwvt(:)=zwvt(:)+zwv(:)
        zywvt(:)=zcs(:)*(zywvt(:)+zm(:)*zwv(:))                         &
     &          +(1.-zcs(:))*(zywvt(:)+zbetta*zwv(:))
        zwvl(:,jlev)=zwvt(:)
        zywvl(:,jlev)=zywvt(:)
        zcs(:)=zcs(:)*(1.-dcc(:,jlev))
        zrcs(:,jlev)=0.
        zrcsu(:,jlev)=0.
       endwhere
      end do
!
!     compute optical properties
!
!     downward loop
!
!     preset
!
      where(losun(:))
       zta1(:)=1.
       zta1s(:)=1.
       zra1(:)=0.
       zra1s(:)=0.
       zta2(:)=1.
       zta2s(:)=1.
       zra2(:)=0.
       zra2s(:)=0.
!
       zto3t(:)=1.
       zo3(:)=zxo3t(:)+zmbar*zo3t(:)
       zto3tu(:)=1.                                                     &
     &          -(0.02118*zo3(:)/(1.+0.042*zo3(:)+0.000323*zo3(:)**2)   &
     &           +1.082*zo3(:)/((1.+138.6*zo3(:))**0.805)               &
     &           +0.0658*zo3(:)/(1.+(103.6*zo3(:))**3))/zsolar1
       ztwvt(:)=1.
       zwv(:)=zywvt(:)+zbetta*zwvt(:)
       ztwvtu(:)=1.-2.9*zwv(:)/((1.+141.5*zwv(:))**0.635+5.925*zwv(:))  &
     &            /zsolar2
!
!     clear sky scattering (Rayleigh scatterin lower most level only)
!
       zrcs(:,NLEV)=(0.219/(1.+0.816*zmu0(:))*zcs(:)                    &
     &              +0.144*(1.-zcs(:)-dcc(:,NLEV)))*nrscat
       zrcsu(:,NLEV)=0.144*(1.-dcc(:,NLEV))*nrscat
      endwhere
!
      do jlev=1,NLEV
       where(losun(:))
        zt1(:,jlev)=zta1(:)
        zt2(:,jlev)=zta2(:)
        zr1s(:,jlev)=zra1s(:)
        zr2s(:,jlev)=zra2s(:)
!
!     set single layer R and T:
!
!     1. spectral range 1:
!
!     a) R
!     clear part: rayleigh scattering (only lowermost level)
!     cloudy part: cloud albedo
!
        zrb1(:,jlev)=zrcs(:,jlev)+zrcl1(:,jlev)*dcc(:,jlev)
        zrb1s(:,jlev)=zrcsu(:,jlev)+zrcl1s(:,jlev)*dcc(:,jlev)
!
!     b) T
!
!     ozon absorption
!
!     downward beam
!
        zo3(:)=zxo3l(:,jlev)
        zto3(:)=(1.                                                     &
     &          -(0.02118*zo3(:)/(1.+0.042*zo3(:)+0.000323*zo3(:)**2)   &
     &           +1.082*zo3(:)/((1.+138.6*zo3(:))**0.805)               &
     &           +0.0658*zo3(:)/(1.+(103.6*zo3(:))**3))/zsolar1)        &
     &         /zto3t(:)
        zto3t(:)=zto3t(:)*zto3(:)
!
!     upward scattered beam
!
        zo3(:)=zxo3t(:)+zmbar*(zo3t(:)-zo3l(:,jlev))
        zto3u(:)=zto3tu(:)                                              &
     &         /(1.-(0.02118*zo3(:)/(1.+0.042*zo3(:)+0.000323*zo3(:)**2)&
     &              +1.082*zo3(:)/((1.+138.6*zo3(:))**0.805)            &
     &              +0.0658*zo3(:)/(1.+(103.6*zo3(:))**3))/zsolar1)
        zto3tu(:)=zto3tu(:)/zto3u(:)
!
!     total T = 1-(A(ozon)+R(rayl.))*(1-dcc)-R(cloud)*dcc
!
        ztb1(:,jlev)=1.-(1.-zto3(:))*(1.-dcc(:,jlev))-zrb1(:,jlev)
        ztb1u(:,jlev)=1.-(1.-zto3u(:))*(1.-dcc(:,jlev))-zrb1s(:,jlev)
!
!     make combined layer R_ab, R_abs, T_ab and T_abs
!
        z1mrabr(:)=1./(1.-zra1s(:)*zrb1s(:,jlev))
        zra1(:)=zra1(:)+zta1(:)*zrb1(:,jlev)*zta1s(:)*z1mrabr(:)
        zta1(:)=zta1(:)*ztb1(:,jlev)*z1mrabr(:)
        zra1s(:)=zrb1s(:,jlev)+ztb1u(:,jlev)*zra1s(:)*ztb1(:,jlev)      &
     &                        *z1mrabr(:)
        zta1s(:)=ztb1u(:,jlev)*zta1s(:)*z1mrabr(:)
!
!     2. spectral range 2:
!
!     a) R
!
!     cloud albedo
!
        zrb2(:,jlev)=zrcl2(:,jlev)*dcc(:,jlev)
        zrb2s(:,jlev)=zrcl2s(:,jlev)*dcc(:,jlev)
!
!     b) T
!
!     water vapor absorption
!
!     downward beam
!
       zwv(:)=zywvl(:,jlev)
       ztwv(:)=(1.-2.9*zwv(:)/((1.+141.5*zwv(:))**0.635+5.925*zwv(:))   &
     &            /zsolar2)                                             &
     &        /ztwvt(:)
       ztwvt(:)=ztwvt(:)*ztwv(:)
!
!     upward scattered beam
!
       zwv(:)=zywvt(:)+zbetta*(zwvt(:)-zwvl(:,jlev))
       ztwvu(:)=ztwvtu(:)                                               &
     &         /(1.-2.9*zwv(:)/((1.+141.5*zwv(:))**0.635+5.925*zwv(:))  &
     &            /zsolar2)
       ztwvtu(:)=ztwvtu(:)/ztwvu(:)
!
!     total T = 1-A(water vapor)*(1.-dcc)-(A(cloud)+R(cloud))*dcc
!
        ztb2(:,jlev)=1.-(1.-ztwv(:))*(1.-dcc(:,jlev))                   &
     &              -(1.-ztcl2(:,jlev))*dcc(:,jlev)
        ztb2u(:,jlev)=1.-(1.-ztwvu(:))*(1.-dcc(:,jlev))                 &
     &               -(1.-ztcl2s(:,jlev))*dcc(:,jlev)
!
!     make combined layer R_ab, R_abs, T_ab and T_abs
!
        z1mrabr(:)=1./(1.-zra2s(:)*zrb2s(:,jlev))
        zra2(:)=zra2(:)+zta2(:)*zrb2(:,jlev)*zta2s(:)*z1mrabr(:)
        zta2(:)=zta2(:)*ztb2(:,jlev)*z1mrabr(:)
        zra2s(:)=zrb2s(:,jlev)+ztb2u(:,jlev)*zra2s(:)*ztb2(:,jlev)      &
     &                       *z1mrabr(:)
        zta2s(:)=ztb2u(:,jlev)*zta2s(:)*z1mrabr(:)
       endwhere
      enddo
      where(losun(:))
       zt1(:,NLEP)=zta1(:)
       zt2(:,NLEP)=zta2(:)
       zr1s(:,NLEP)=zra1s(:)
       zr2s(:,NLEP)=zra2s(:)
!
!     upward loop
!
!     make upward R
!
       zra1s(:)=dalb(:)
       zra2s(:)=dalb(:)
!
!      set albedo for the direct beam (for ocean use ECHAM3 param)
!
!PBH removed as tunable albedo for direct beam is now incorporated
!       dalb(:)=dls(:)*dalb(:)+(1.-dls(:))*dicec(:)*dalb(:)              &
!     &        +(1.-dls(:))*(1.-dicec(:))*AMIN1(0.05/(zmu0(:)+0.15),0.15)

       zra1(:)=dalb(:)
       zra2(:)=dalb(:)
      endwhere
      do jlev=NLEV,1,-1
       where(losun(:))
        zrl1(:,jlev+1)=zra1(:)
        zrl2(:,jlev+1)=zra2(:)
        zrl1s(:,jlev+1)=zra1s(:)
        zrl2s(:,jlev+1)=zra2s(:)
        zra1(:)=zrb1(:,jlev)+ztb1(:,jlev)*zra1(:)*ztb1u(:,jlev)         &
     &                      /(1.-zra1s(:)*zrb1s(:,jlev))
        zra1s(:)=zrb1s(:,jlev)+ztb1u(:,jlev)*zra1s(:)*ztb1u(:,jlev)     &
     &                        /(1.-zra1s(:)*zrb1s(:,jlev))
        zra2(:)=zrb2(:,jlev)+ztb2(:,jlev)*zra2(:)*ztb2u(:,jlev)         &
     &                      /(1.-zra2s(:)*zrb2s(:,jlev))
        zra2s(:)=zrb2s(:,jlev)+ztb2u(:,jlev)*zra2s(:)*ztb2u(:,jlev)     &
     &                        /(1.-zra2s(:)*zrb2s(:,jlev))
       endwhere
      enddo
      where(losun(:))
       zrl1(:,1)=zra1(:)
       zrl2(:,1)=zra2(:)
       zrl1s(:,1)=zra1s(:)
       zrl2s(:,1)=zra2s(:)
      endwhere
!
!     fluxes at layer interfaces
!
      do jlev=1,NLEP
       where(losun(:))
        z1mrabr(:)=1./(1.-zr1s(:,jlev)*zrl1s(:,jlev))
        zfd1(:)=zt1(:,jlev)*z1mrabr(:)
        zfu1(:)=-zt1(:,jlev)*zrl1(:,jlev)*z1mrabr(:)
        z1mrabr(:)=1./(1.-zr2s(:,jlev)*zrl2s(:,jlev))
        zfd2(:)=zt2(:,jlev)*z1mrabr(:)
        zfu2(:)=-zt2(:,jlev)*zrl2(:,jlev)*z1mrabr(:)
        dfu(:,jlev)=zfu1(:)*zftop1(:)+zfu2(:)*zftop2(:)
        dfd(:,jlev)=zfd1(:)*zftop1(:)+zfd2(:)*zftop2(:)
        dswfl(:,jlev)=dfu(:,jlev)+dfd(:,jlev)
       endwhere
      enddo
!
      return
      end subroutine swr

!     ==============
!     SUBROUTINE LWR
!     ==============

      subroutine lwr
      use radmod
      implicit none
      integer jlev,jlem,jlep,jlev2,jlep2
      real zero,zco20,zo30,zh2o0,zh2o0a,zqco2,zzf1,zzf2,zzf3
!
!     compute long wave radiation
!
!     o3-, co2-, h2o- and cloud-absorption is considered
!
!     clear sky absorptivities from Sasamori 1968
!     (J. Applied Meteorology, 7, 721-729)
!
!     no PUMA *subs* are used
!
!     the following PUMA variables are used/modified:
!
!     ga               : gravity accelleration (m/s2) (used)
!     sigma(NLEV)      : full level sigma  (used)
!     sigmah(NLEV)     : half level sigma  (used)
!     dsigma(NLEV)     : delta sigma (half level)  (used)
!     dp(NHOR)         : surface pressure (Pa) (used)
!     dq(NHOR,NLEP)    : specific humidity (kg/kg) (used)
!     dt(NHOR,NLEP)    : temperature (K) (used)
!     dcc(NHOR,NLEP)   : cloud cover (frac.) (used)
!     dlwfl(NHOR,NLEP) : long wave radiation (W/m2)  (modified)
!     dftu(NHOR,NLEP)  : long wave radiation upward (W/m2) (modified)
!     dftd(NHOR,NLEP)  : long wave radiation downward (W/m2) (modified)
!
!     0) define local parameters
!
      real zmmair,zmmco2,zpv2pm,zrco2,zro3,zttop
      parameter(zmmair=0.0289644)      ! molecular weight air (kg/mol)
      parameter(zmmco2=0.0440098)      ! molecular weight co2 (kg/mol)
      parameter(zpv2pm=zmmco2/zmmair)  ! transfere co2 ppvol to ppmass
      parameter(zrco2=1.9635)          ! co2 density (kg/m3 stp)
      parameter(zro3=2.14)             ! o3 density (kg/m3 stp)
      parameter(zttop=0.)              ! t at top of atmosphere
!
!     scaling factors for transmissivities:
!
!     uh2o in g/cm**2 (zfh2o=0.1=1000/(100*100) *kg/m**2)
!     uco2 in cm-STP  (zfco2=100./rco2 * kg/m**2)
!     uo3  in cm-STP  (zfo3=100./ro3 * kg/m**2)
!
      real zfh2o,zfco2,zfo3,zt0
      parameter(zfh2o=0.1)
      parameter(zfco2=100./zrco2)
      parameter(zfo3=100./zro3)
      parameter(zt0=295.)
!
!**   local arrays
!
      real zbu(NHOR,0:NLEP)     ! effective SBK*T**4 for upward radiation
      real zbd(NHOR,0:NLEP)     ! effective SBK*T**4 for downward radiation
      real zst4h(NHOR,NLEP)     ! SBK*T**4  on half levels
      real zst4(NHOR,NLEP)      ! SBK*T**4  on ull levels
      real ztau(NHOR,NLEV)      ! total transmissivity
      real zq(NHOR,NLEV)        ! modified water vapour
      real zqo3(NHOR,NLEV)      ! modified ozon
      real ztausf(NHOR,NLEV)    ! total transmissivity to surface
      real ztaucs(NHOR,NLEV)    ! clear sky transmissivity
      real ztaucc0(NHOR,NLEV)   ! layer transmissivity cloud
      real ztaucc(NHOR)         ! cloud transmissivity
      real ztau0(NHOR)          ! approx. layer transmissivity
      real zsumwv(NHOR)         ! effective water vapor amount
      real zsumo3(NHOR)         ! effective o3 amount
      real zsumco2(NHOR)        ! effective co2 amount
      real zsfac(NHOR)          ! scaling factor
      real zah2o(NHOR)          ! water vapor absorptivity
      real zaco2(NHOR)          ! co2 absorptivity
      real zao3(NHOR)           ! o3 absorptivity
      real zth2o(NHOR)          ! water vapor - co2 overlap transmissivity
      real zbdl(NHOR)           ! layer evective downward rad.
      real zeps(NHOR)           ! surface emissivity
      real zps2(NHOR)           ! ps**2
      real zsigh2(NLEP)         ! sigmah**2
!
!     entropy
!
      real zbue1(NHOR,0:NLEP),zbue2(NHOR,0:NLEP)

!
!**   1) set some necessary (helpful) bits
!

      zero=1.E-6                     ! small number

      zco20=0.0676*(0.01022)**0.421  ! to get a(co2)=0 for co2=0
      zo30=0.0122*ALOG10(6.5E-4)     ! to get a(o3)=0 for o3=0
      zh2o0a=0.846*(3.59E-5)**0.243  ! to get a(h2o)=0 for h2o=0
      zh2o0=0.832*0.0286**0.26       ! to get t(h2o)=1 for h2o=0
      zqco2=co2*zpv2pm*1.E-6         ! co2 pp mass needed for transmissivities
                                     ! note: co2 in ppmv not ppv
      zsigh2(1)=0.
      zsigh2(2:NLEP)=sigmah(1:NLEV)**2

      zps2(:)=dp(:)*dp(:)
!
!**   2) calc. stb*t**4 + preset fluxes
!
      dftu(:,:)=0.
      dftd(:,:)=0.
!
!*    stb*t**4 on full and half levels
!
!     full levels and surface
!
      zst4(:,1:NLEP)=SBK*dt(:,1:NLEP)**4
!
!     half level (incl. toa and near surface)
!
      zst4h(:,1)=zst4(:,1)                                              &
     &          -(zst4(:,1)-zst4(:,2))*sigma(1)/(sigma(1)-sigma(2))
      do jlev=2,NLEV
       jlem=jlev-1
       zst4h(:,jlev)=(zst4(:,jlev)*(sigma(jlem)-sigmah(jlem))           &
     &               +zst4(:,jlem)*(sigmah(jlem)-sigma(jlev)))          &
     &              /(sigma(jlem)-sigma(jlev))
      enddo
      where((zst4(:,NLEV)-zst4h(:,NLEV))                                &
     &     *(zst4(:,NLEV)-zst4(:,NLEP)) > 0.)
       zst4h(:,NLEP)=zst4(:,NLEP)
      elsewhere
       zst4h(:,NLEP)=zst4(:,NLEV)                                       &
     &              +(zst4(:,NLEV)-zst4(:,NLEM))*(1.-sigma(NLEV))       &
     &              /(sigma(NLEV)-sigma(NLEM))
      endwhere
!
!*    top downward flux, surface grayness and surface upward flux
!
      zbd(:,0)=SBK*zttop**4
      zeps(:)=dls(:)+0.98*(1.-dls(:))
      zbu(:,NLEP)=zeps(:)*zst4(:,NLEP)
      zbue1(:,NLEP)=0.
      zbue2(:,NLEP)=zbu(:,NLEP)

!
!**   3) vertical loops
!
!
!     a) modified absorber amounts and cloud transmissivities
!
      do jlev=1,NLEV
       jlep=jlev+1
       zzf1=sigma(jlev)*dsigma(jlev)/ga/100000.
       zzf2=zfco2*zqco2*(zsigh2(jlep)-zsigh2(jlev))*0.5/ga/100000.
       zzf3=-1.66*acllwr*1000.*dsigma(jlev)/ga
       zsfac(:)=zzf1*zps2(:)
       zq(:,jlev)=zfh2o*zsfac(:)*dq(:,jlev)
       zqo3(:,jlev)=zfo3*zsfac(:)*dqo3(:,jlev)
       zsumco2(:)=zzf2*zps2(:)
       if(clgray > 0) then
        ztaucc0(:,jlev)=1.-dcc(:,jlev)*clgray
       else
        ztaucc0(:,jlev)=1.-dcc(:,jlev)*(1.-exp(zzf3*dql(:,jlev)*dp(:)))
       endif
      enddo
!
!     b) transmissivities, effective radiations and fluxes
!
      do jlev=1,NLEV
       jlem=jlev-1
       ztaucc(:)=1.
       zsumwv(:)=0.
       zsumo3(:)=0.
!
!     transmissivities
!
       do jlev2=jlev,NLEV
        jlep2=jlev2+1
        zzf2=zfco2*zqco2*(zsigh2(jlep2)-zsigh2(jlev))*0.5/ga/100000.
        zsumwv(:)=zsumwv(:)+zq(:,jlev2)
        zsumo3(:)=zsumo3(:)+zqo3(:,jlev2)
        zsumco2(:)=zzf2*zps2(:)
!
!     clear sky transmisivity
!
!     h2o absorption:
!
!     a) 6.3mu
!
        where(zsumwv(:) <= 0.01)
         zah2o(:)=0.846*(zsumwv(:)+3.59E-5)**0.243-zh2o0a
        elsewhere
         zah2o(:)=0.24*ALOG10(zsumwv(:)+0.01)+0.622
        endwhere
!
!     b) continuum
!
        if(th2oc > 0.) then
         zah2o(:)=AMIN1(zah2o(:)+(1.-exp(-th2oc*zsumwv(:))),1.)
        endif
!
!     co2 absorption:
!
        where(zsumco2(:) <= 1.0)
         zaco2(:)=0.0676*(zsumco2(:)+0.01022)**0.421-zco20
        elsewhere
         zaco2(:)=0.0546*ALOG10(zsumco2(:))+0.0581
        endwhere
!
!     h2o-co2 overlap transmission:
!
        zth2o(:)=1.-(0.832*(zsumwv(:)+0.0286)**0.26-zh2o0)
!
!     o3 absorption:
!
        zao3(:)=0.0122*ALOG10(zsumo3(:)+6.5E-4)-zo30
!
!     total clear sky transmissivity
!
        ztaucs(:,jlev2)=1.-zah2o(:)-zao3(:)-zaco2(:)*zth2o(:)
!
!     bound transmissivity:
!
        ztaucs(:,jlev2)=AMIN1(1.-zero,AMAX1(zero,ztaucs(:,jlev2)))
!
!     cloud transmisivity assuming random overlap
!
        ztaucc(:)=ztaucc(:)*ztaucc0(:,jlev2)
!
!     total transmissivity
!
        ztau(:,jlev2)=ztaucs(:,jlev2)*ztaucc(:)
       enddo
!
!     upward and downward effective SBK*T**4
!
       if(jlev == 1) then
        ztau0(:)=1.-zero
        do jlev2=1,NLEV
         jlep2=jlev2+1
         ztau0(:)=ztaucs(:,jlev2)/ztau0(:)*ztaucc0(:,jlev2)*tpofmt
         ztau0(:)=AMIN1(1.-zero,MAX(zero,ztau0(:)))
         where((zst4(:,jlev2)-zst4h(:,jlev2))                           &
     &        *(zst4(:,jlev2)-zst4h(:,jlep2)) > 0.)
          zbd(:,jlev2)=0.5*zst4(:,jlev2)                                &
     &                +0.25*(zst4h(:,jlev2)+zst4h(:,jlep2))
          zbu(:,jlev2)=zbd(:,jlev2)
          zbue1(:,jlev2)=zbu(:,jlev2)
          zbue2(:,jlev2)=0.
         elsewhere
          zbd(:,jlev2)=(zst4h(:,jlep2)-ztau0(:)*zst4h(:,jlev2))         &
     &                /(1.-ztau0(:))                                    &
     &                -(zst4h(:,jlev2)-zst4h(:,jlep2))/ALOG(ztau0(:))
          zbu(:,jlev2)=zst4h(:,jlev2)+zst4h(:,jlep2)-zbd(:,jlev2)
          zbue1(:,jlev2)=zbu(:,jlev2)
          zbue2(:,jlev2)=0.
         endwhere
         ztau0(:)=ztaucs(:,jlev2)
        enddo
       endif
!
!     fluxes
!
       dftu(:,jlev)=dftu(:,jlev)-zbu(:,jlev)
       dftd(:,jlev)=dftd(:,jlev)+zbd(:,jlem)
       dftue1(:,jlev)=dftue1(:,jlev)-zbue1(:,jlev)
       dftue2(:,jlev)=dftue2(:,jlev)-zbue2(:,jlev)
       zbdl(:)=zbd(:,jlem)-zbd(:,jlev)
       do jlev2=jlev,NLEV
        jlep2=jlev2+1
        dftu(:,jlev)=dftu(:,jlev)                                       &
     &              -(zbu(:,jlep2)-zbu(:,jlev2))*ztau(:,jlev2)
        dftue1(:,jlev)=dftue1(:,jlev)                                   &
     &              -(zbue1(:,jlep2)-zbue1(:,jlev2))*ztau(:,jlev2)
        dftue2(:,jlev)=dftue2(:,jlev)                                   &
     &              -(zbue2(:,jlep2)-zbue2(:,jlev2))*ztau(:,jlev2)
        dftd(:,jlep2)=dftd(:,jlep2)                                     &
     &               +zbdl(:)*ztau(:,jlev2)
       enddo
       if(jlev == 1) then
        dftu0(:,1)=-zbu(:,1)*(1.-ztau(:,1))
        do jlev2=2,NLEV
         jlem=jlev2-1
         dftu0(:,jlev2)=-zbu(:,jlev2)*(ztau(:,jlem)-ztau(:,jlev2))
        enddo
       endif
!
!     collect transmissivity to surface
!
       ztausf(:,jlev)=ztau(:,NLEV)
      enddo
      do jlev=1,NLEV-1
       jlep=jlev+1
       dftd0(:,jlev)=-zbd(:,jlev)*(ztausf(:,jlep)-ztausf(:,jlev))
      enddo
      dftd0(:,NLEV)=-zbd(:,jlev)*(1.-ztausf(:,NLEV))
!
!     complite surface lwr
!
      dftu(:,NLEP)=dftu(:,NLEP)-zbu(:,NLEP)
      dftd(:,NLEP)=dftd(:,NLEP)+zbd(:,NLEV)
      dftue1(:,NLEP)=dftue1(:,NLEP)-zbue1(:,NLEP)
      dftue2(:,NLEP)=dftue2(:,NLEP)-zbue2(:,NLEP)
!
!     correct for non black suface
!
      zeps(:)=(1.-zeps(:))*dftd(:,NLEP)
      do jlev=1,NLEV
       dftu(:,jlev)=dftu(:,jlev)                                        &
     &             -ztausf(:,jlev)*zeps(:)
       dftue1(:,jlev)=dftue1(:,jlev)                                    &
     &             -ztausf(:,jlev)*zeps(:)
       dftue2(:,jlev)=dftue2(:,jlev)                                    &
     &             -ztausf(:,jlev)*zeps(:)
      enddo
!
!     total longwave radiation
!
      dlwfl(:,:)=dftu(:,:)+dftd(:,:)
!
      return
      end subroutine lwr

!     ====================
!     Earth Orbit Routines (form CCM3)
!     ====================
!
!     Routines for calculation of oribital parameters and solar
!     declination angle.
!
!     Based on f77 routines in NCAR CCM3:
!
!     Subroutines contained
!
!       orb_params --- Calculate the orbital parameters for a given
!                               situation/year.
!       orb_decl ----- Calculate the solar declination angle and
!                               Earth/Sun distance factor for a given
!                               time of the year.
!       orb_print ---- Print out information on the orbital parameters
!                               to use.
!
!     Code history
!
!         Original version:  Erik Kluzek
!         Date:              Oct/1997
!
!
!     Version information:
!
!         CVS: $Id: orb.F,v 1.8.8.1 1998/12/02 17:29:07 erik Exp $
!         CVS: $Source: /fs/cgd/csm/models/CVS.REPOS/shared/csm_share/orb.F,v $
!         CVS: $Name: ccm3_6_16_brnchT_amip2_9 $
!

       module orbconst
!
!         parameters for orbital calculations
!
          implicit none
          real, parameter :: ORB_ECCEN_MIN  =   0.0                ! minimum value for eccen
          real, parameter :: ORB_ECCEN_MAX  =   0.1                ! maximum value for eccen
          real, parameter :: ORB_OBLIQ_MIN  = -90.0                ! minimum value for obliq
          real, parameter :: ORB_OBLIQ_MAX  = +90.0                ! maximum value for obliq
          real, parameter :: ORB_MVELP_MIN  =   0.0                ! minimum value for mvelp
          real, parameter :: ORB_MVELP_MAX  = 360.0                ! maximum value for mvelp
          real, parameter :: ORB_UNDEF_REAL = 1.e36                ! undefined/unset/invalid value
          real, parameter :: ORB_DEFAULT    = ORB_UNDEF_REAL       ! flag to use default orbit
          integer, parameter :: ORB_UNDEF_INT  = 2000000000        ! undefined/unset/invalid value
          integer, parameter :: ORB_NOT_YEAR_BASED = ORB_UNDEF_INT ! flag to not use input year
       end module orbconst

!     =====================
!     SUBROUTINE ORB_PARAMS
!     =====================

      subroutine orb_params(iyear_AD, eccen, obliq, mvelp,              &
     &                      obliqr, lambm0, mvelpp, log_print,          &
     &                      mypid, nroot)
!
!      Calculate earth's orbital parameters using Dave Threshers
!      formula which came from Berger, Andre.  1978
!      "A Simple Algorithm to Compute Long-Term Variations
!      of Daily Insolation".  Contribution 18, Institute of Astronomy and
!      Geophysics, Universite Catholique de Louvain, Louvain-la-Neuve,
!      Belgium.
!
!      Original Author: Erik Kluzek
!      Date:            Oct/97
!
      use orbconst
      implicit none

!     Input Arguments
!     ---------------
      real :: eccen        ! Earth's orbital eccentricity
      real :: obliq        ! Earth's obliquity in degree's
      real :: mvelp        ! Earth's moving vernal equinox longitude
      integer :: iyear_AD  ! Year to calculate orbit for..
      logical :: log_print ! Flag to print-out status information or not.
                           ! (This turns off ALL status printing including)
                           ! (error messages.)
      integer :: mypid     ! process id (PUMA MPI)
      integer :: nroot     ! process id of root (PUMA MPI)

!     Output Arguments
!     ----------------
      real :: obliqr  ! Earth's obliquity in radians
      real :: lambm0  ! Mean longitude of perihelion at the
!                     ! vernal equinox (radians)
      real :: mvelpp  ! Earth's moving vernal equinox longitude
!                     ! of perihelion plus pi (radians)
!
! Parameters for calculating earth's orbital characteristics
! ----------
      integer, parameter :: poblen = 47  ! number of elements in the series to calc obliquity
      integer, parameter :: pecclen = 19 ! number of elements in the series to calc eccentricity
      integer, parameter :: pmvelen = 78 ! number of elements in the series to calc vernal equinox
      real :: degrad          ! degrees to radians conversion factor
      real :: obamp(poblen)   ! amplitudes for obliquity cosine series
      real :: obrate(poblen)  ! rates for obliquity cosine series
      real :: obphas(poblen)  ! phases for obliquity cosine series
      real :: ecamp(pecclen)  ! amplitudes for eccentricity/fvelp cosine/sine series
      real :: ecrate(pecclen) ! rates for eccentricity/fvelp cosine/sine series
      real :: ecphas(pecclen) ! phases for eccentricity/fvelp cosine/sine series
      real :: mvamp(pmvelen)  ! amplitudes for mvelp sine series
      real :: mvrate(pmvelen) ! rates for mvelp sine series
      real :: mvphas(pmvelen) ! phases for mvelp sine series
      real :: yb4_1950AD      ! number of years before 1950 AD
!
      real, parameter :: psecdeg = 1./3600. ! arc seconds to degrees conversion
!
!  Cosine series data for computation of obliquity:
!  amplitude (arc seconds), rate (arc seconds/year), phase (degrees).
!
      data obamp /-2462.2214466D0, -857.3232075D0, -629.3231835D0,      &
     &             -414.2804924D0, -311.7632587D0,  308.9408604D0,      &
     &             -162.5533601D0, -116.1077911D0,  101.1189923D0,      &
     &              -67.6856209D0,   24.9079067D0,   22.5811241D0,      &
     &              -21.1648355D0,  -15.6549876D0,   15.3936813D0,      &
     &               14.6660938D0,  -11.7273029D0,   10.2742696D0,      &
     &                6.4914588D0,    5.8539148D0,   -5.4872205D0,      &
     &               -5.4290191D0,    5.1609570D0,    5.0786314D0,      &
     &               -4.0735782D0,    3.7227167D0,    3.3971932D0,      &
     &               -2.8347004D0,   -2.6550721D0,   -2.5717867D0,      &
     &               -2.4712188D0,    2.4625410D0,    2.2464112D0,      &
     &               -2.0755511D0,   -1.9713669D0,   -1.8813061D0,      &
     &               -1.8468785D0,    1.8186742D0,    1.7601888D0,      &
     &               -1.5428851D0,    1.4738838D0,   -1.4593669D0,      &
     &                1.4192259D0,   -1.1818980D0,    1.1756474D0,      &
     &               -1.1316126D0,    1.0896928D0/
!
      data obrate /31.609974D0, 32.620504D0, 24.172203D0,               &
     &             31.983787D0, 44.828336D0, 30.973257D0,               &
     &             43.668246D0, 32.246691D0, 30.599444D0,               &
     &             42.681324D0, 43.836462D0, 47.439436D0,               &
     &             63.219948D0, 64.230478D0,  1.010530D0,               &
     &              7.437771D0, 55.782177D0,  0.373813D0,               &
     &             13.218362D0, 62.583231D0, 63.593761D0,               &
     &             76.438310D0, 45.815258D0,  8.448301D0,               &
     &             56.792707D0, 49.747842D0, 12.058272D0,               &
     &             75.278220D0, 65.241008D0, 64.604291D0,               &
     &              1.647247D0,  7.811584D0, 12.207832D0,               &
     &             63.856665D0, 56.155990D0, 77.448840D0,               &
     &              6.801054D0, 62.209418D0, 20.656133D0,               &
     &             48.344406D0, 55.145460D0, 69.000539D0,               &
     &             11.071350D0, 74.291298D0, 11.047742D0,               &
     &              0.636717D0, 12.844549D0/
!
      data obphas /251.9025D0, 280.8325D0, 128.3057D0,                  &
     &             292.7252D0,  15.3747D0, 263.7951D0,                  &
     &             308.4258D0, 240.0099D0, 222.9725D0,                  &
     &             268.7809D0, 316.7998D0, 319.6024D0,                  &
     &             143.8050D0, 172.7351D0,  28.9300D0,                  &
     &             123.5968D0,  20.2082D0,  40.8226D0,                  &
     &             123.4722D0, 155.6977D0, 184.6277D0,                  &
     &             267.2772D0,  55.0196D0, 152.5268D0,                  &
     &              49.1382D0, 204.6609D0,  56.5233D0,                  &
     &             200.3284D0, 201.6651D0, 213.5577D0,                  &
     &              17.0374D0, 164.4194D0,  94.5422D0,                  &
     &             131.9124D0,  61.0309D0, 296.2073D0,                  &
     &             135.4894D0, 114.8750D0, 247.0691D0,                  &
     &             256.6114D0,  32.1008D0, 143.6804D0,                  &
     &              16.8784D0, 160.6835D0,  27.5932D0,                  &
     &             348.1074D0,  82.6496D0/
!
!  Cosine/sine series data for computation of eccentricity and
!  fixed vernal equinox longitude of perihelion (fvelp):
!  amplitude, rate (arc seconds/year), phase (degrees).
!
      data ecamp /0.01860798D0,  0.01627522D0, -0.01300660D0,           &
     &            0.00988829D0, -0.00336700D0,  0.00333077D0,           &
     &           -0.00235400D0,  0.00140015D0,  0.00100700D0,           &
     &            0.00085700D0,  0.00064990D0,  0.00059900D0,           &
     &            0.00037800D0, -0.00033700D0,  0.00027600D0,           &
     &            0.00018200D0, -0.00017400D0, -0.00012400D0,           &
     &            0.00001250D0/
!
      data ecrate /4.2072050D0,  7.3460910D0, 17.8572630D0,             &
     &            17.2205460D0, 16.8467330D0,  5.1990790D0,             &
     &            18.2310760D0, 26.2167580D0,  6.3591690D0,             &
     &            16.2100160D0,  3.0651810D0, 16.5838290D0,             &
     &            18.4939800D0,  6.1909530D0, 18.8677930D0,             &
     &            17.4255670D0,  6.1860010D0, 18.4174410D0,             &
     &             0.6678630D0/
!
      data ecphas /28.620089D0, 193.788772D0, 308.307024D0,             &
     &            320.199637D0, 279.376984D0,  87.195000D0,             &
     &            349.129677D0, 128.443387D0, 154.143880D0,             &
     &            291.269597D0, 114.860583D0, 332.092251D0,             &
     &            296.414411D0, 145.769910D0, 337.237063D0,             &
     &            152.092288D0, 126.839891D0, 210.667199D0,             &
     &             72.108838D0/
!
!  Sine series data for computation of moving vernal equinox
!  longitude of perihelion:
!  amplitude (arc seconds), rate (arc seconds/year), phase (degrees).
!
      data mvamp /7391.0225890D0, 2555.1526947D0, 2022.7629188D0,       &
     &           -1973.6517951D0, 1240.2321818D0,  953.8679112D0,       &
     &            -931.7537108D0,  872.3795383D0,  606.3544732D0,       &
     &            -496.0274038D0,  456.9608039D0,  346.9462320D0,       &
     &            -305.8412902D0,  249.6173246D0, -199.1027200D0,       &
     &             191.0560889D0, -175.2936572D0,  165.9068833D0,       &
     &             161.1285917D0,  139.7878093D0, -133.5228399D0,       &
     &             117.0673811D0,  104.6907281D0,   95.3227476D0,       &
     &              86.7824524D0,   86.0857729D0,   70.5893698D0,       &
     &             -69.9719343D0,  -62.5817473D0,   61.5450059D0,       &
     &             -57.9364011D0,   57.1899832D0,  -57.0236109D0,       &
     &             -54.2119253D0,   53.2834147D0,   52.1223575D0,       &
     &             -49.0059908D0,  -48.3118757D0,  -45.4191685D0,       &
     &             -42.2357920D0,  -34.7971099D0,   34.4623613D0,       &
     &             -33.8356643D0,   33.6689362D0,  -31.2521586D0,       &
     &             -30.8798701D0,   28.4640769D0,  -27.1960802D0,       &
     &              27.0860736D0,  -26.3437456D0,   24.7253740D0,       &
     &              24.6732126D0,   24.4272733D0,   24.0127327D0,       &
     &              21.7150294D0,  -21.5375347D0,   18.1148363D0,       &
     &             -16.9603104D0,  -16.1765215D0,   15.5567653D0,       &
     &              15.4846529D0,   15.2150632D0,   14.5047426D0,       &
     &             -14.3873316D0,   13.1351419D0,   12.8776311D0,       &
     &              11.9867234D0,   11.9385578D0,   11.7030822D0,       &
     &              11.6018181D0,  -11.2617293D0,  -10.4664199D0,       &
     &              10.4333970D0,  -10.2377466D0,   10.1934446D0,       &
     &             -10.1280191D0,   10.0289441D0,  -10.0034259D0/
!
      data mvrate /31.609974D0, 32.620504D0, 24.172203D0,               &
     &              0.636717D0, 31.983787D0,  3.138886D0,               &
     &             30.973257D0, 44.828336D0,  0.991874D0,               &
     &              0.373813D0, 43.668246D0, 32.246691D0,               &
     &             30.599444D0,  2.147012D0, 10.511172D0,               &
     &             42.681324D0, 13.650058D0,  0.986922D0,               &
     &              9.874455D0, 13.013341D0,  0.262904D0,               &
     &              0.004952D0,  1.142024D0, 63.219948D0,               &
     &              0.205021D0,  2.151964D0, 64.230478D0,               &
     &             43.836462D0, 47.439436D0,  1.384343D0,               &
     &              7.437771D0, 18.829299D0,  9.500642D0,               &
     &              0.431696D0,  1.160090D0, 55.782177D0,               &
     &             12.639528D0,  1.155138D0,  0.168216D0,               &
     &              1.647247D0, 10.884985D0,  5.610937D0,               &
     &             12.658184D0,  1.010530D0,  1.983748D0,               &
     &             14.023871D0,  0.560178D0,  1.273434D0,               &
     &             12.021467D0, 62.583231D0, 63.593761D0,               &
     &             76.438310D0,  4.280910D0, 13.218362D0,               &
     &             17.818769D0,  8.359495D0, 56.792707D0,               &
     &              8.448301D0,  1.978796D0,  8.863925D0,               &
     &              0.186365D0,  8.996212D0,  6.771027D0,               &
     &             45.815258D0, 12.002811D0, 75.278220D0,               &
     &             65.241008D0, 18.870667D0, 22.009553D0,               &
     &             64.604291D0, 11.498094D0,  0.578834D0,               &
     &              9.237738D0, 49.747842D0,  2.147012D0,               &
     &              1.196895D0,  2.133898D0,  0.173168D0/
!
      data mvphas /251.9025D0, 280.8325D0, 128.3057D0,                  &
     &             348.1074D0, 292.7252D0, 165.1686D0,                  &
     &             263.7951D0,  15.3747D0,  58.5749D0,                  &
     &              40.8226D0, 308.4258D0, 240.0099D0,                  &
     &             222.9725D0, 106.5937D0, 114.5182D0,                  &
     &             268.7809D0, 279.6869D0,  39.6448D0,                  &
     &             126.4108D0, 291.5795D0, 307.2848D0,                  &
     &              18.9300D0, 273.7596D0, 143.8050D0,                  &
     &             191.8927D0, 125.5237D0, 172.7351D0,                  &
     &             316.7998D0, 319.6024D0,  69.7526D0,                  &
     &             123.5968D0, 217.6432D0,  85.5882D0,                  &
     &             156.2147D0,  66.9489D0,  20.2082D0,                  &
     &             250.7568D0,  48.0188D0,   8.3739D0,                  &
     &              17.0374D0, 155.3409D0,  94.1709D0,                  &
     &             221.1120D0,  28.9300D0, 117.1498D0,                  &
     &             320.5095D0, 262.3602D0, 336.2148D0,                  &
     &             233.0046D0, 155.6977D0, 184.6277D0,                  &
     &             267.2772D0,  78.9281D0, 123.4722D0,                  &
     &             188.7132D0, 180.1364D0,  49.1382D0,                  &
     &             152.5268D0,  98.2198D0,  97.4808D0,                  &
     &             221.5376D0, 168.2438D0, 161.1199D0,                  &
     &              55.0196D0, 262.6495D0, 200.3284D0,                  &
     &             201.6651D0, 294.6547D0,  99.8233D0,                  &
     &             213.5577D0, 154.1631D0, 232.7153D0,                  &
     &             138.3034D0, 204.6609D0, 106.5938D0,                  &
     &             250.4676D0, 332.3345D0,  27.3039D0/
!
!     Local variables
!     ---------------
      integer i        ! Index for series summations
      real :: obsum    ! Obliquity series summation
      real :: cossum   ! Cosine series summation for eccentricity/fvelp
      real :: sinsum   ! Sine series summation for eccentricity/fvelp
      real :: fvelp    ! Fixed vernal equinox longitude of perihelion
      real :: mvsum    ! mvelp series summation
      real :: beta     ! Intermediate argument for lambm0
      real :: years    ! Years to time of interest (negative = past;
!                      ! positive = future)
      real :: eccen2   ! eccentricity squared
      real :: eccen3   ! eccentricity cubed
      real :: pi       ! pi
!
! radinp and algorithms below will need a degrees to radians conversion
! factor.
!
      pi     =  4.*atan(1.)
      degrad = pi/180.
!
! Check for flag to use input orbit parameters
!
      if ( iyear_AD .eq. ORB_NOT_YEAR_BASED ) then
!
! Check input obliq, eccen, and mvelp to ensure reasonable
!
         if( obliq .eq. ORB_UNDEF_REAL )then
          if ( log_print ) then
           if(mypid==nroot) then
            write(*,*)'(orb_params) Have to specify orbital parameters:'
            write(*,*) 'Either set: '                                   &
     &                ,'iyear_AD, OR [obliq, eccen, and mvelp]:'
            write(*,*)'iyear_AD is the year to simulate the orbit for ' &
     &                ,'(ie. 1950): '
            write(*,*)'obliq, eccen, mvelp specify the orbit directly:'
            write(*,*)'The AMIP II settings (for a 1995 orbit) are: '
            write(*,*)' obliq = 23.4441'
            write(*,*)' eccen = 0.016715'
            write(*,*)' mvelp = 102.7'
           end if
          end if
          stop 999
        else if ( log_print ) then
          if(mypid==nroot) then
           write(*,*)'(orb_params) Use input orbital parameters: '
          end if
         end if
         if( (obliq.lt.ORB_OBLIQ_MIN).or.(obliq.gt.ORB_OBLIQ_MAX) ) then
          if ( log_print ) then
           if(mypid==nroot) then
             write(*,*) '(orb_params): Input obliquity unreasonable: '  &
     &                  ,obliq
           end if
          end if
          stop 999
         end if
         if( (eccen.lt.ORB_ECCEN_MIN).or.(eccen.gt.ORB_ECCEN_MAX) ) then
          if ( log_print ) then
           if(mypid==nroot) then
            write(*,*) '(orb_params): Input eccentricity unreasonable: '&
     &                 ,eccen
           end if
          end if
          stop 999
         end if
         if( (mvelp.lt.ORB_MVELP_MIN).or.(mvelp.gt.ORB_MVELP_MAX) ) then
          if ( log_print ) then
           if(mypid==nroot) then
             write(*,*)'(orb_params): Input mvelp unreasonable: ', mvelp
           endif
          end if
          stop 999
         end if
        eccen2 = eccen*eccen
        eccen3 = eccen2*eccen
      else
!
! Otherwise calculate based on years before present
!
        yb4_1950AD = 1950.0 - float(iyear_AD)
        if ( abs(yb4_1950AD) .gt. 1000000.0 )then
          if ( log_print ) then
           if(mypid==nroot) then
            write(*,*)'(orb_params) orbit only valid for years+-1000000'
            write(*,*)'(orb_params) Relative to 1950 AD'
            write(*,*)'(orb_params) # of years before 1950: ',yb4_1950AD
            write(*,*)'(orb_params) Year to simulate was  : ',iyear_AD
           end if
          end if
          stop 999
        end if
!
!
! The following calculates the earth's obliquity, orbital eccentricity
! (and various powers of it) and vernal equinox mean longitude of
! perihelion for years in the past (future = negative of years past),
! using constants (see parameter section) given in the program of:
!
! Berger, Andre.  1978  A Simple Algorithm to Compute Long-Term Variations
! of Daily Insolation.  Contribution 18, Institute of Astronomy and
! Geophysics, Universite Catholique de Louvain, Louvain-la-Neuve, Belgium.
!
! and formulas given in the paper (where less precise constants are also
! given):
!
! Berger, Andre.  1978.  Long-Term Variations of Daily Insolation and
! Quaternary Climatic Changes.  J. of the Atmo. Sci. 35:2362-2367
!
! The algorithm is valid only to 1,000,000 years past or hence.
! For a solution valid to 5-10 million years past see the above author.
! Algorithm below is better for years closer to present than is the
! 5-10 million year solution.
!
! Years to time of interest must be negative of years before present
! (1950) in formulas that follow.
!
        years = - yb4_1950AD
!
! In the summations below, cosine or sine arguments, which end up in
! degrees, must be converted to radians via multiplication by degrad.
!
! Summation of cosine series for obliquity (epsilon in Berger 1978) in
! degrees. Convert the amplitudes and rates, which are in arc seconds, into
! degrees via multiplication by psecdeg (arc seconds to degrees conversion
! factor).  For obliq, first term is Berger 1978's epsilon star; second
! term is series summation in degrees.
!
        obsum = 0.0
        do i = 1, poblen
          obsum = obsum +                                               &
     &           obamp(i)*psecdeg*cos((obrate(i)*psecdeg*years +        &
     &                                obphas(i))*degrad)
        end do
        obliq = 23.320556 + obsum
!
! Summation of cosine and sine series for computation of eccentricity
! (eccen; e in Berger 1978) and fixed vernal equinox longitude of perihelion
! (fvelp; pi in Berger 1978), which is used for computation of moving vernal
! equinox longitude of perihelion.  Convert the rates, which are in arc
! seconds, into degrees via multiplication by psecdeg.
!
        cossum = 0.0
        do i = 1, pecclen
          cossum = cossum +                                             &
     &            ecamp(i)*cos((ecrate(i)*psecdeg*years +               &
     &                          ecphas(i))*degrad)
        end do
!
        sinsum = 0.0
        do i = 1, pecclen
          sinsum = sinsum +                                             &
     &            ecamp(i)*sin((ecrate(i)*psecdeg*years +               &
     &                          ecphas(i))*degrad)
        end do
!
! Use summations to calculate eccentricity
!
        eccen2 = cossum*cossum + sinsum*sinsum
        eccen = sqrt(eccen2)
        eccen3 = eccen2*eccen
!
! A series of cases for fvelp, which is in radians.
!
        if (abs(cossum) .le. 1.0E-8) then
          if (sinsum .eq. 0.0) then
            fvelp = 0.0
          else if (sinsum .lt. 0.0) then
            fvelp = 1.5*pi
          else if (sinsum .gt. 0.0) then
            fvelp = .5*pi
          endif
        else if (cossum .lt. 0.0) then
          fvelp = atan(sinsum/cossum) + pi
        else if (cossum .gt. 0.0) then
          if (sinsum .lt. 0.0) then
            fvelp = atan(sinsum/cossum) + 2.0*pi
          else
            fvelp = atan(sinsum/cossum)
          endif
        endif
!
! Summation of sine series for computation of moving vernal equinox longitude
! of perihelion (mvelp; omega bar in Berger 1978) in degrees.  For mvelp,
! first term is fvelp in degrees; second term is Berger 1978's psi bar times
! years and in degrees; third term is Berger 1978's zeta; fourth term is
! series summation in degrees.  Convert the amplitudes and rates, which are
! in arc seconds, into degrees via multiplication by psecdeg.  Series summation
! plus second and third terms constitute Berger 1978's psi, which is the
! general precession.
!
        mvsum = 0.0
        do i = 1, pmvelen
          mvsum = mvsum +                                               &
     &           mvamp(i)*psecdeg*sin((mvrate(i)*psecdeg*years +        &
     &                                mvphas(i))*degrad)
        end do
        mvelp = fvelp/degrad + 50.439273*psecdeg*years + 3.392506       &
     &  + mvsum
!
! Cases to make sure mvelp is between 0 and 360.
!
        do while (mvelp .lt. 0.0)
          mvelp = mvelp + 360.0
        end do
        do while (mvelp .ge. 360.0)
          mvelp = mvelp - 360.0
        end do
      end if  ! end of test on whether to calculate or use input orbital params
!
! Orbit needs the obliquity in radians
!
      obliqr = obliq*degrad
!
! 180 degrees must be added to mvelp since observations are made from the
! earth and the sun is considered (wrongly for the algorithm) to go around
! the earth. For a more graphic explanation see Appendix B in:
!
! A. Berger, M. Loutre and C. Tricot. 1993.  Insolation and Earth's Orbital
! Periods.  J. of Geophysical Research 98:10,341-10,362.
!
! Additionally, orbit will need this value in radians. So mvelp becomes
! mvelpp (mvelp plus pi)
!
      mvelpp = (mvelp + 180.)*degrad
!
! Set up an argument used several times in lambm0 calculation ahead.
!
      beta = sqrt(1. - eccen2)
!
! The mean longitude at the vernal equinox (lambda m nought in Berger
! 1978; in radians) is calculated from the following formula given in
! Berger 1978.  At the vernal equinox the true longitude (lambda in Berger
! 1978) is 0.
!
      lambm0 = 2.*((.5*eccen + .125*eccen3)*(1. + beta)*sin(mvelpp)     &
     &            - .25*eccen2*(.5 + beta)*sin(2.*mvelpp)               &
     &            + .125*eccen3*(1./3. + beta)*sin(3.*mvelpp))
!
      if ( log_print ) then
       if(mypid==nroot) then
        write(*,'(/," *****************************************")')
        write(*,'(" *     Computed Orbital Parameters       *")')
        write(*,'(" *****************************************")')
        write(*,'(" * Year AD           =  ",i16  ," *")') iyear_AD
        write(*,'(" * Eccentricity      =  ",f16.6," *")') eccen
        write(*,'(" * Obliquity (deg)   =  ",f16.6," *")') obliq
        write(*,'(" * Obliquity (rad)   =  ",f16.6," *")') obliqr
        write(*,'(" * Long of perh(deg) =  ",f16.6," *")') mvelp
        write(*,'(" * Long of perh(rad) =  ",f16.6," *")') mvelpp
        write(*,'(" * Long at v.e.(rad) =  ",f16.6," *")') lambm0
        write(*,'(" *****************************************")')
       end if
      end if
!
!
      return
      end subroutine orb_params


!     ===================
!     SUBROUTINE ORB_DECL
!     ===================

      subroutine orb_decl(calday,eccen,mvelpp,lambm0,obliqr,delta,eccf)
      use pumamod, only: n_days_per_year
!
!     Compute earth/orbit parameters using formula suggested by
!     Duane Thresher.
!
!     Original version:  Erik Kluzek
!     Date:              Oct/1997
!
!     Modification: 22-Feb-2006 (ek) - get days/yr from pumamod
!
      implicit none
!
!     Input arguments
!     ---------------
      real :: calday     ! Calendar day, including fraction
      real :: eccen      ! Eccentricity
      real :: obliqr     ! Earth's obliquity in radians
      real :: lambm0     ! Mean longitude of perihelion at the
!                        ! vernal equinox (radians)
      real :: mvelpp     ! Earth's moving vernal equinox longitude
!                        ! of perihelion plus pi (radians)
!
!     Output arguments
!     ----------------
      real :: delta      ! Solar declination angle in radians
      real :: eccf       ! Earth-sun distance factor ( i.e. (1/r)**2 )
!
!     Local variables
!     ---------------
      real, parameter :: ve = 80.5 ! Calday of vernal equinox
!                                  ! correct for Jan 1 = calday 1
      real, parameter :: pie = 3.141592653589793D0
!
      real :: lambm   ! Lambda m, earth's mean longitude of perihelion (radians)
      real :: lmm     ! Intermediate argument involving lambm
      real :: lamb    ! Lambda, the earth's longitude of perihelion
      real :: invrho  ! Inverse normalized sun/earth distance
      real :: sinl    ! Sine of lmm
!
! Compute eccentricity factor and solar declination using
! day value where a round day (such as 213.0) refers to 0z at
! Greenwich longitude.
!
! Use formulas from Berger, Andre 1978: Long-Term Variations of Daily
! Insolation and Quaternary Climatic Changes. J. of the Atmo. Sci.
! 35:2362-2367.
!
! To get the earth's true longitude (position in orbit; lambda in Berger 1978),
! which is necessary to find the eccentricity factor and declination,
! must first calculate the mean longitude (lambda m in Berger 1978) at
! the present day.  This is done by adding to lambm0 (the mean longitude
! at the vernal equinox, set as March 21 at noon, when lambda = 0; in radians)
! an increment (delta lambda m in Berger 1978) that is the number of
! days past or before (a negative increment) the vernal equinox divided by
! the days in a model year times the 2*pi radians in a complete orbit.
!

      lambm  = lambm0 + (calday - ve)*2.*pie/n_days_per_year
      lmm    = lambm  - mvelpp
!
! The earth's true longitude, in radians, is then found from
! the formula in Berger 1978:
!
      sinl   = sin(lmm)
      lamb   = lambm  + eccen*(2.*sinl                                  &
     &         + eccen*(1.25*sin(2.*lmm)                                &
     &         + eccen*((13.0/12.0)*sin(3.*lmm) - 0.25*sinl)))
!
! Using the obliquity, eccentricity, moving vernal equinox longitude of
! perihelion (plus), and earth's true longitude, the declination (delta)
! and the normalized earth/sun distance (rho in Berger 1978; actually inverse
! rho will be used), and thus the eccentricity factor (eccf), can be calculated
! from formulas given in Berger 1978.
!
      invrho = (1. + eccen*cos(lamb - mvelpp))                          &
     &         / (1. - eccen*eccen)
!
! Set solar declination and eccentricity factor
!
      delta  = asin(sin(obliqr)*sin(lamb))
      eccf   = invrho*invrho
!
      return
      end subroutine orb_decl

!     ====================
!     SUBROUTINE ORB_PRINT
!     ====================

      subroutine orb_print( iyear_AD, eccen, obliq, mvelp ,mypid,nroot)
!
!     Print out the information on the input orbital characteristics
!
!     Original version:  Erik Kluzek
!     Date:              Oct/1997
!
      use orbconst
      implicit none
!
!     Input Arguments
!     ---------------
      real :: eccen  ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
      real :: obliq  ! Earth's obliquity angle (degree's) (-90 to +90) (typically 22-26)
      real :: mvelp  ! Earth's moving vernal equinox at perhelion (degree's) (0 to 360.0)
      integer :: iyear_AD ! Year (AD) to simulate above earth's orbital parameters for
      integer :: mypid    ! process id (PUMA MPI)
      integer :: nroot    ! process id of root (PUMA MPI)
!
      if ( iyear_AD .eq. ORB_NOT_YEAR_BASED )then
        if ( obliq .eq. ORB_UNDEF_REAL )then
         if(mypid==nroot) then
          write(6,*) 'Orbit parameters not set!'
         end if
        else
         if(mypid==nroot) then
          write(6,*) 'Orbital parameters: '
          write(6,*) 'Obliquity (degree):              ', obliq
          write(6,*) 'Eccentricity (unitless):         ', eccen
          write(6,*) 'Long. of moving Perhelion (deg): ', mvelp
         end if
        end if
      else
        if ( iyear_AD .gt. 0 )then
         if(mypid==nroot) then
          write(6,*) 'Orbital parameters calculated for given year: '   &
     &               ,iyear_AD,' AD'
         end if
        else
         if(mypid==nroot) then
          write(6,*) 'Orbital parameters calculated for given year: '   &
     &               ,iyear_AD,' BC'
         end if
        end if
      end if
!
      return
      end subroutine orb_print

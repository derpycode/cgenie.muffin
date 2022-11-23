      module oceanmod

      use geniemod
      implicit none
!
!     version identifier (date)
!
      character(len=80) :: version = '13.10.2005 by Larry'
!
!     Parameter
!
      integer NPRO,NLAT
      parameter(NPRO = NPRO_ATM)        ! Number of processes (resmod)
      parameter(NLAT = NLAT_ATM)        ! Number of latitudes (resmod)
      integer NLEV_OCE,NLON,NLPP,NHOR,NROOT
      parameter(NLEV_OCE = 1)           ! Number of Layers
      parameter(NLON = NLAT + NLAT)     ! Number of longitudes
      parameter(NLPP = NLAT / NPRO)     ! Latitudes per process
      parameter(NHOR = NLON * NLPP)     ! Horizontal part
      parameter(NROOT = 0)              ! Master node
      real CRHOS,CRHOI,CPS,TFREEZE,PLARAD
      parameter(CRHOS=1030.)            ! Density of sea water (kg/m**3)
      parameter(CRHOI=920.)             ! Density of sea ice (kg/m**3)
      parameter(CPS=4180.)              ! Specific heat of sea water (J/kg*K)
      parameter(TFREEZE=271.25)         ! Freezing point (K)
      parameter(PLARAD=6.371E6)         ! Earth radius (m)
      real*8 PI
      parameter(PI = 3.14159265359D0)   ! PI
!
!     namelist parameter
!
      integer :: nfluko           = 0   ! switch for flux correction
                                        ! (0=none,1=heat-budget,2=newtonian)
      integer :: ndiag            = 480 ! diagnostics each ndiag timesteps
      integer :: noutput          = 1   ! master switch for output: 0= no output
      integer :: nout             = 32  ! afterburner output each nout timesteps
      integer :: nocean           = 1   ! compute ocean yes/no
      integer :: newsurf          = 0   ! update surface arrays at restart
      integer :: ntspd            = 32  ! ocean timesteps per day
      integer :: nperpetual_ocean = 0   ! perpetual climate conditions
      integer :: nprint           = 0   ! print debug information
      integer :: nprhor           = 0   ! gp to print debug information
      integer :: nhdiff           = 0   ! switch for horizontal heat diffusion
      integer :: nentropy         = 0   ! switch for entropy diagnostics
      integer :: nlsg             = 0   ! coupling flag to lsg   
      integer :: naomod           = 320 ! atmos/ocean(lsg) ration    
! PBH
      integer :: nflukoavg        = 115200  ! timesteps for averaging flux correction

!
      real :: dlayer(NLEV_OCE)  = 50.   ! layer depth (m)
      real :: taunc             =  0.   ! newtonian cooling timescale (d)
      real :: vdiffkl(NLEV_OCE) = 1.E-4 ! vertikal diffusion coeff. [m**2/s]
      real :: hdiffk(NLEV_OCE)  = 1.E3  ! horizontal diffusion coeff. [m**2/s]
!
!     global integer
!
      integer :: nstep    = 0           ! time step
      integer :: naccuout = 0           ! counter for accumulated output
      integer :: nrestart = 0           ! switch for restart
!
      integer :: ndatim(6)              ! array containig calendar info
!
!     global reals
!
      real :: dtmix                     ! time step (s)
      real :: solar_day    = 86400.0    ! 24 * 60 * 60 (for Earth)
!
      real :: dlam                      ! delta longitude
      real :: dphi(NLAT)                ! delta latitude
      real :: cphi(NLAT)                ! cos(latitude)
      real :: cphih(0:NLAT)             ! cos(latitude) at grid bounds
      real :: dmue(NLAT)                ! delta sin(phi) (phi at bound) = -gw
      real :: vdiffk(NLEV_OCE)          ! vertikal diffusion coeff interpol.
!
      real :: gw(NHOR)                  ! gaussian weights
!
      real :: ysst(NHOR,NLEV_OCE) = 0.  ! temperature (K)
      real :: ymld(NHOR,NLEV_OCE) = 0.  ! layer depth (m)
      real :: yls(NHOR)    = 0.         ! land sea mask (frac.)
      real :: yicec(NHOR)  = 0.         ! ice cover (frac.)
      real :: yiced(NHOR)  = 0.         ! ice thickness (m)
      real :: ycliced(NHOR)= 0.         ! climatological ice thickness (m)
      real :: yheat(NHOR)  = 0.         ! heat flux from ice/atm. (W/m**2)
      real :: yfldo(NHOR)  = 0.         ! flux from deep ocean (LSG) (W/m**2)
      real :: ypme(NHOR)   = 0.         ! fresh water flux (p-e only, m/s)
      real :: yroff(NHOR)  = 0.         ! runoff (m/s)
      real :: ytaux(NHOR)  = 0.         ! zonal wind stress (pa)
      real :: ytauy(NHOR)  = 0.         ! meridional wind stress (pa)
      real :: yust3(NHOR)  = 0.         ! friction velocity**3 (m**3/s**3)
      real :: yiflux(NHOR) = 0.         ! heat flux into ice (residual) (W/m2)
      real :: yifluxr(NHOR)= 0.         ! heat flux into ice (from fluko) (W/m2)
      real :: ydsst(NHOR)  = 0.         ! heat flux from vdiff (w/m2)
      real :: yqhd(NHOR)   = 0.         ! heat flux from hdiff (w/m2)

      real :: yclsst(NHOR,0:13)         ! climatological sst (K)
      real :: yfsst(NHOR,0:13) = 0.     ! flux corr. sst (W/m**2)

      real :: yclsst2(NHOR) = 0.        ! climatological sst (K)
      real :: yfsst2(NHOR) = 0.         ! flux corr. sst (W/m**2)
!
!     for lsg: ice+snow
!
      real :: yicesnow(NHOR) = 0.       ! depth of ice plus snow
!
!     diagnostics (accum.)
!
      real :: yheata(NHOR) = 0.        ! accum. heat flux from atm/ice
      real :: yfssta(NHOR) = 0.        ! accum. flux correction
      real :: yifluxa(NHOR)= 0.        ! accum. heat flux into ice
      real :: ydssta(NHOR)= 0.         ! accum. heat flux from vdiff
      real :: yfldoa(NHOR)= 0.         ! accum. heat flux from deep oce (lsg)
      real :: yqhda(NHOR)= 0.          ! accum. heat flux from hdiff
! PBH
      real :: yfsst2a(NHOR,0:13) = 0.  ! accum. flux correction for output (when nocean=0)
!
!     additional diagnostics
!
      real,allocatable :: yentro(:,:)  ! entropy diagnostics
!
!     Parallel Stuff
!
      integer :: mpinfo  = 0
      integer :: mypid   = 0
      integer :: myworld = 0
      integer :: nproc   = NPRO

      end module oceanmod


!     ===================================
!     SUBROUTINE OCEANINI
!     ===================================
!
      subroutine oceanini(kstep,krestart,koutput,kdpy,psst,pmld    &
     &                   ,piflux,ktspd,psolday)
      use oceanmod
      implicit none
!
      real :: psst(NHOR),pmld(NHOR),piflux(NHOR)
      real (kind=8) :: zsi(NLAT)
      real (kind=8) :: zgw(NLAT)
      real :: zgw2(NLON,NLAT)
      real :: zls(NLON*NLAT)
      integer :: nlem_oce = NLEV_OCE - 1 
      real zzsi,psolday
      integer jlat,jlev,kstep,krestart,koutput,ktspd,kdpy
! PBH added nflukoavg
      namelist/oceanpar/ndiag,nout,nfluko,ntspd,nocean,nprint,nprhor    &
    &                  ,nperpetual_ocean,naomod,nlsg,taunc,dlayer       &
    &                  ,vdiffkl,newsurf,hdiffk,nentropy,nhdiff          &
    &                  ,nflukoavg
!
!     get process id
!
      call mpi_info(nproc,mypid)
!
!     compute grids properties
!
      if(mypid == NROOT) then
       dlam=2.*pi/real(NLON)
       call inigau_plasim(NLAT,zsi,zgw)
       do jlat=1,NLAT
        zgw2(:,jlat)=zgw(jlat)
       enddo
       cphi(:)=cos(asin(zsi(:)))
       dphi(1:NLAT-1)=asin(zsi(2:NLAT))-asin(zsi(1:NLAT-1))
       cphih(0)=0.
       zzsi=1.
       do jlat=1,NLAT/2
        zzsi=zzsi-zgw(jlat)
        cphih(jlat)=cos(asin(zzsi))
        cphih(NLAT-jlat)=cphih(jlat)
        dmue(jlat)=-zgw(jlat)
        dmue(NLAT+1-jlat)=-zgw(jlat)
       enddo
       cphih(NLAT)=0.
      endif
      call mpscgp(zgw2,gw,1)
      call mpbcr(dlam)
      call mpscrn(cphi,NLPP)
      call mpscrn(cphih(1:NLAT),NLPP)
      call mpscrn(dphi,NLPP)
      call mpscrn(dmue,NLPP)
!
!     copy input from icemod
!
      nstep     = kstep
      nrestart  = krestart
      noutput   = koutput
      ntspd     = ktspd
      solar_day = psolday
!
!     read and print namelist and distribute it
!
      if (mypid == NROOT) then
         open(12,file=trim(runtime_root)//'/genie-plasim/config/ocean_namelist',form='formatted')
         read(12,oceanpar)
         write (*,'(/," *******************************************")')
         write (*,'(" * OCEANMOD ",a30," *")') trim(version)
         write (*,'(" *******************************************")')
         write (*,'(" * Namelist OCEANPAR from <ocean_namelist> *")')
         write (*,'(" *******************************************")')
         write(*,oceanpar)
         close(12)
      endif

      call mpbci(ndiag)
      call mpbci(nout)
      call mpbci(nfluko)
      call mpbci(nocean)
      call mpbci(newsurf)
      call mpbci(nprint)
      call mpbci(nprhor)
      call mpbci(ntspd)
      call mpbci(nperpetual_ocean)
      call mpbci(nlsg)
      call mpbci(naomod)
      call mpbci(nentropy)
      call mpbci(nhdiff)
      call mpbcr(taunc)
      call mpbcr(solar_day)
      call mpbcrn(vdiffkl,NLEV_OCE)
      call mpbcrn(hdiffk,NLEV_OCE)
      call mpbcrn(dlayer,NLEV_OCE)
!
      do jlev=1,NLEV_OCE
         ymld(:,jlev) = dlayer(jlev)
      enddo
      if(NLEV_OCE > 1) then
       do jlev=1,nlem_oce
          vdiffk(jlev)=(dlayer(jlev)*vdiffkl(jlev)                      &
     &                 +dlayer(jlev+1)*vdiffkl(jlev+1))                 &
     &                /(dlayer(jlev)+dlayer(jlev+1)) 
       enddo
      endif
!
!     compute time step
!
      dtmix = solar_day / real(ntspd)
!
!     compute taunc in s
!
      taunc = solar_day  * taunc
!
      if (nrestart == 0) then ! new start (read start file)
         call mpsurfgp('yls',yls,NHOR,1)
         call mpsurfgp('yclsst',yclsst,NHOR,14)

!        make sure, that land sea mask values are 0 or 1

         where (yls(:) > 0.5)
            yls(:) = 1.0
         elsewhere
            yls(:) = 0.0
         endwhere
!
!        make clsst >= tfreeze
!
         yclsst(:,:) = MAX(yclsst(:,:),TFREEZE)
!
!        initialize sst
!
         call oceanget

         do jlev=1,NLEV_OCE
            ysst(:,jlev) = yclsst2(:)
         enddo

      else ! restart from restart file 
         if (mypid == NROOT) then
            call get_restart_integer('nstep'   ,nstep   )
            call get_restart_integer('naccuoce',naccuout)
         endif
         call mpbci(nstep)
         call mpbci(naccuout)

         call mpgetgp('yls'    ,yls    ,NHOR,   1)
         call mpgetgp('ysst'   ,ysst   ,NHOR,NLEV_OCE)
         call mpgetgp('yiflux' ,yiflux ,NHOR,   1)
         call mpgetgp('yclsst' ,yclsst ,NHOR,  14)
         call mpgetgp('yheata' ,yheata ,NHOR,   1)
         call mpgetgp('yfssta' ,yfssta ,NHOR,   1)
         call mpgetgp('yifluxa',yifluxa,NHOR,   1)
         call mpgetgp('ydssta' ,ydssta ,NHOR,   1)
         call mpgetgp('yfldoa' ,yfldoa ,NHOR,   1)
         call mpgetgp('yqhda'  ,yqhda  ,NHOR,   1)
         call mpgetgp('yfldo'  ,yfldo  ,NHOR,   1)

         if (newsurf == 1) then ! Read new surface data
            call mpsurfgp('yclsst',yclsst,NHOR,14)
            yclsst(:,:) = MAX(yclsst(:,:),TFREEZE)
         endif
      endif ! (nrestart == 0)
!
!     read flux correction
!
      if (nfluko == 1) then
         call mpsurfgp('yfsst',yfsst,NHOR,14)
      endif
!
!     initialize lsg coupling
!
      if (nlsg > 0) then
       call mpgagp(zls,yls,1)
       call ntomin(nstep,ndatim(5),ndatim(4),ndatim(3),ndatim(2)        &
     &            ,ndatim(1))
       if(mypid == nroot) then
!
!      check if day_per_year are right (lsg coupling needs 360)
! 
        if(kdpy .ne. 360.) then
         write(*,*) '!ERROR! for LSG coupling you need to set '         
         write(*,*) '        n_days_per_year in plasim namelist INP '
         write(*,*) '        to 360 !'
         write(*,*) '        at the moment, n_days_per_year= ',kdpy
         write(*,*) 'Model stoped!'
         stop 'ERROR'
        endif
        call clsgini(ndatim,ntspd,naomod,zls,nlon,nlat)
       endif
      endif
!
!     open output file
!
      if (mypid == NROOT) then
        open  (72,file=trim(outdir_name)//'/ocean_output',form='unformatted')
      endif
!
!     copy output to icemod
!
      psst(:)=ysst(:,1)
      pmld(:)=ymld(:,1)
      piflux(:)=-yiflux(:)
!
!     allocate space for entropy diagnostics
!
      if (nentropy > 0) then
       allocate(yentro(NHOR,7))
       yentro(:,:)=0.
      endif
!
      return
      end subroutine oceanini

!     ===================================================================
!     SUBROUTINE oceanstep
!     ===================================================================

      subroutine oceanstep(picec,piced,pheat,ppme,proff,ptaux,ptauy     &
     &                    ,pust3,psnow,psst,pmld,piflux,pcliced         &
     &                    ,ngenie)
      use oceanmod
      implicit none
      integer ngenie
!
      real :: picec(NHOR),piced(NHOR),pheat(NHOR),ppme(NHOR),proff(NHOR)
      real :: ptaux(NHOR),ptauy(NHOR),pust3(NHOR)
      real :: psst(NHOR),pmld(NHOR),piflux(NHOR),pcliced(NHOR)
      real :: psnow(NHOR)                        ! snow only used for lsg
!
!     arrays for lsg coupling
!
      real :: zsst(NLON*NLAT)
      real :: ztaux(NLON*NLAT)
      real :: ztauy(NLON*NLAT)
      real :: zpme(NLON*NLAT)
      real :: zroff(NLON*NLAT)
      real :: zheat(NLON*NLAT)
      real :: zice(NLON*NLAT)
      real :: zfldo(NLON*NLAT)
!
!     dbug arrays
!
      real,allocatable :: zprf1(:),zprf2(:),zprf3(:),zprf4(:),zprf5(:)

      real zcmdt
      integer imin,ihou,iday,imon,iyea 
!
!     set helpful bits
!
      zcmdt=CRHOS*CPS/dtmix
!
!     copy input to seamod
!
      yicec(:)=picec(:)
      yiced(:)=piced(:)
      yheat(:)=pheat(:)
      ypme(:)=ppme(:)
      yroff(:)=proff(:)
      ytaux(:)=ptaux(:)
      ytauy(:)=ptauy(:)
      yust3(:)=pust3(:)
      yicesnow(:)=piced(:)*CRHOI/1000.+psnow(:)
      ycliced(:)=pcliced(:)
!
!     get annual cycle
!
      call oceanget
!
!     print dbug information if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       allocate(zprf4(NLON*NLAT))
       allocate(zprf5(NLON*NLAT))
       call mpgagp(zprf1,yicec,1)
       call mpgagp(zprf2,yiced,1)
       call mpgagp(zprf3,yheat,1)
       call mpgagp(zprf4,yclsst2,1)
       call mpgagp(zprf5,ycliced,1)
       if(mypid==NROOT) then
        print*,'in oceanstep: nstep= ',nstep
        print*,' '
        print*,'seaice c and d from ice: ',zprf1(nprhor),zprf2(nprhor)
        print*,'heatflux from ice: ',zprf3(nprhor)
        print*,'climatological sst: ',zprf4(nprhor)
        print*,'climatological iced: ',zprf5(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
       deallocate(zprf4)
       deallocate(zprf5)
      endif
!
!     do the lsg coupling
!
      if (nlsg > 0) then
       if(nlsg < 2) call mpgagp(zsst,ysst,1)
       call mpgagp(ztaux,ytaux,1)
       call mpgagp(ztauy,ytauy,1)
       call mpgagp(zpme,ypme,1)
       call mpgagp(zroff,yroff,1)
       call mpgagp(zice,yicesnow,1)
       call mpgagp(zheat,yheat,1)
       call ntomin(nstep,ndatim(5),ndatim(4),ndatim(3),ndatim(2)        &
     &            ,ndatim(1))
       if(mypid == nroot) then
        call clsgstep(ndatim,nstep,zsst,ztaux,ztauy,zpme,zroff,zice     &
     &               ,zheat,zfldo,nlon,nlat)
       endif
       if(nlsg < 2) then
        if(mod(nstep,naomod) == naomod-1) call mpscgp(zfldo,yfldo,1)
       else
        yfldo(:)=0.
       endif
      endif
!
!     compute new sst
!
      call mksst

!
!     if lsg coupling due to ahfl and osst, replace ssts 
!
      if(nlsg > 1) then
       if(mod(nstep,naomod) == naomod-1) call mpscgp(zsst,ysst,1)
      endif
!
!     a) climatological ocean
!
      if(nocean == 0) then
!
!     flux correction diagnostics
!
       where(yls(:) < 1.)
        yfsst2(:)=zcmdt*(yclsst2(:)-ysst(:,1))*ymld(:,1)
       endwhere

! PBH accumulate monthly flux correction
       call ntomin(nstep,imin,ihou,iday,imon,iyea)
       yfsst2a(:,imon)=yfsst2a(:,imon)+yfsst2(:)
! output flux correction every nflukoavg timesteps
       if(mod(nstep,nflukoavg)==0) call outflux_ocean
! end PBH

!
!     entropy diagnostics
!
       if(nentropy > 0) then
        where(yls(:) < 1.)
         yentro(:,3)=yfsst2(:)/ysst(:,1)
        endwhere
       endif
!
!     set sst
!
       where(yls(:) < 1.)
        ysst(:,1)=yclsst2(:)
       endwhere

!GENIE COUPLING!!!!!!!!!!!!!!!!!!!!!!!!!!
!needs nocean=1 i.e. assumes GENIE provides climatological ocean
      if(ngenie.eq.1) call mpgagp(ysst,genie_sst,1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!     print dbug information if needed
!
       if (nprint==2) then
        allocate(zprf1(NLON*NLAT))
        call mpgagp(zprf1,yfsst2,1)
        if(mypid==NROOT) then
         print*,'computed flux correction: ',zprf1(nprhor)
        endif
        deallocate(zprf1)
       endif
!
      else
!
!     b) interactive ocean
!
!     get flux correction
!
       if(nfluko == 0) then
        yfsst2(:)=0.
       elseif(nfluko == 1) then
        call getflxco
        call addfc
       elseif(nfluko == 2) then
        call mkfc
        call addfc
       endif
!
      endif
!
!     make global average of flux into ice 
!
      if (nfluko > 0) call mkiflx
!
!     output
!
      yfssta(:)=yfssta(:)+yfsst2(:)
      yheata(:)=yheata(:)+yheat(:)
      yfldoa(:)=yfldoa(:)+yfldo(:)
      yifluxa(:)=yifluxa(:)+yiflux(:)
      ydssta(:)=ydssta(:)+ydsst(:)
      yqhda(:)=yqhda(:)+yqhd(:)
      naccuout=naccuout+1
      if(mod(nstep,nout) == 0) then
       yheata(:)=yheata/real(naccuout)
       yfldoa(:)=yfldoa/real(naccuout)
       yfssta(:)=yfssta/real(naccuout)
       yifluxa(:)=yifluxa/real(naccuout)
       ydssta(:)=ydssta/real(naccuout)
       yqhda(:)=yqhda/real(naccuout)
       if(noutput > 0) call oceanout
       yfssta(:)=0.
       yheata(:)=0.
       yfldoa(:)=0.
       yifluxa(:)=0.
       ydssta(:)=0.
       yqhda(:)=0.
       naccuout=0
      endif
!
!     copy output to seamod
!
      psst(:)=ysst(:,1)
      pmld(:)=ymld(:,1)
      piflux(:)=-yiflux(:)
!
!     advance time step
!
      nstep=nstep+1
!
!
!     print dbug information if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       call mpgagp(zprf1,psst,1)
       call mpgagp(zprf2,piflux,1)
       if(mypid==NROOT) then
        print*,'final sst to ice: ',zprf1(nprhor)
        print*,'final heatflux to ice: ',zprf2(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
      endif
!
      return
      end subroutine oceanstep

!     ===================================
!     SUBROUTINE OCEAN_REST
!     ===================================

      subroutine ocean_rest
      use oceanmod
      implicit none
!
!     write restart file
!
      if (mypid == NROOT) then
         call put_restart_integer('nlev_oce',NLEV_OCE)
         call put_restart_integer('naccuoce',naccuout)
      endif

      call mpputgp('yls'    ,yls    ,NHOR,   1)
      call mpputgp('ysst'   ,ysst   ,NHOR,NLEV_OCE)
      call mpputgp('yiflux' ,yiflux ,NHOR,   1)
      call mpputgp('yclsst' ,yclsst ,NHOR,  14)
      call mpputgp('yheata' ,yheata ,NHOR,   1)
      call mpputgp('yfssta' ,yfssta ,NHOR,   1)
      call mpputgp('yifluxa',yifluxa,NHOR,   1)
      call mpputgp('ydssta' ,ydssta ,NHOR,   1)
      call mpputgp('yfldoa' ,yfldoa ,NHOR,   1)
      call mpputgp('yqhda'  ,yqhda  ,NHOR,   1)
      call mpputgp('yfldo'  ,yfldo  ,NHOR,   1)
!
      return
      end subroutine ocean_rest

!     =====================================================================
!     SUBROUTINE oceanout

!     ===================================
!     SUBROUTINE OCEANSTOP
!     ===================================

      subroutine oceanstop
      use oceanmod
      implicit none
!
!     write restart file
!
      if (mypid == NROOT) then
         call put_restart_integer('nlev_oce',NLEV_OCE)
         call put_restart_integer('naccuoce',naccuout)
      endif

      call mpputgp('yls'    ,yls    ,NHOR,   1)
      call mpputgp('ysst'   ,ysst   ,NHOR,NLEV_OCE)
      call mpputgp('yiflux' ,yiflux ,NHOR,   1)
      call mpputgp('yclsst' ,yclsst ,NHOR,  14)
      call mpputgp('yheata' ,yheata ,NHOR,   1)
      call mpputgp('yfssta' ,yfssta ,NHOR,   1)
      call mpputgp('yifluxa',yifluxa,NHOR,   1)
      call mpputgp('ydssta' ,ydssta ,NHOR,   1)
      call mpputgp('yfldoa' ,yfldoa ,NHOR,   1)
      call mpputgp('yqhda'  ,yqhda  ,NHOR,   1)
      call mpputgp('yfldo'  ,yfldo  ,NHOR,   1)
!
!     close output file
!
      if (mypid == NROOT) then
       close(72)
      endif
!
!     stop lsg coupling
!
      if(nlsg > 0) then
       if(mypid == NROOT) then
        call clsgstop
       endif
      endif
!
!     deallocate entropy space
!
      if(nentropy > 0) deallocate(yentro)
!
      return
      end subroutine oceanstop

!     =====================================================================
!     SUBROUTINE oceanout
!     =====================================================================

      subroutine oceanout
      use oceanmod
      implicit none
      integer nmin,nhour,nday,nmonth,nyear
      integer jlev,je

      integer :: ih(8)

      call ntomin(nstep,nmin,nhour,nday,nmonth,nyear)

      ih(2) = 0
      ih(3) = nyear*10000 +nmonth*100 +nday
      ih(4) = nhour*100 +nmin
      ih(5) = NLON
      ih(6) = NLAT
      ih(7) = 0
      ih(8) = 0
!
!     heat flux from atm/ice
!
      ih(1) = 901
      call mpwritegph(72,yheata,NHOR,1,ih)
!
!     heat flux into ice
!
      ih(1) = 902
      call mpwritegph(72,yifluxa,NHOR,1,ih)
!
!     flux correction
!
      ih(1) = 903
      call mpwritegph(72,yfssta,NHOR,1,ih)
!
!     flux used for dsst/dt (vertical diffusion)
!
      ih(1) = 904
      call mpwritegph(72,ydssta,NHOR,1,ih)
!
!     flux used for dsst/dt (horizontal diffusion)
!
      ih(1) = 905
      call mpwritegph(72,yqhda,NHOR,1,ih)
!
!     flux from deep ocean (LSG)
!
      ih(1) = 906
      call mpwritegph(72,yfldoa,NHOR,1,ih)
!
!     sea ice cover
!
      ih(1) = 910
      call mpwritegph(72,yicec,NHOR,1,ih)
!
!     sst
!
      ih(1) = 939
      do jlev=1,NLEV_OCE
       ih(2)=(jlev-1)
       call mpwritegph(72,ysst(1,jlev),NHOR,1,ih)
      enddo
      ih(2)=0
!
!     land sea mask
!
      ih(1) = 972
      call mpwritegph(72,yls,NHOR,1,ih)
!
!     clim. sst
!
      ih(1) = 990
      call mpwritegph(72,yclsst2,NHOR,1,ih)
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       do je=1,7
        ih(1) = 990+je
        call mpwritegph(72,yentro(1,je),NHOR,1,ih)
       enddo
      endif
!
      return
      end subroutine oceanout

!====================================================================
!      SUBROUTINE OUTFLUX
!====================================================================
! PBH
      subroutine outflux_ocean
! outputs ocean flux correction
      use oceanmod
      implicit none
      integer i,nmin,nhour,nday,nmonth,nyear

      integer :: ih(8)

      call ntomin(nstep,nmin,nhour,nday,nmonth,nyear)

      ih(1) = 903
      ih(2) = 0
      ih(3) = nyear*10000 +nmonth*100 +nday
      ih(4) = nhour*100 +nmin
      ih(5) = NLON
      ih(6) = NLAT
      ih(7) = 0
      ih(8) = 0

! calculate average
      yfsst2a(:,:)=yfsst2a(:,:)*12.0/real(nflukoavg)
! copy january (to 13) and december (to 0) -  see subroutine oceanget
      yfsst2a(:,0)=yfsst2a(:,12)
      yfsst2a(:,13)=yfsst2a(:,1)

! temporary path and filename (generalise this)
      IF(NLAT.EQ.32) THEN
        open(10,file=trim(runtime_root)//'/genie-plasim/data/input/T21/N032_surf_0903.sra',form='formatted')
      ELSE IF(NLAT.EQ.64) THEN
        open(10,file=trim(runtime_root)//'/genie-plasim/data/input/T42/N064_surf_0903.sra',form='formatted')
      ELSE
        print*,"look at outflux_ocean"
        stop
      ENDIF

      do i=0,13
       write(10,*) ih
       write(10,*) yfsst2a(:,i)
      enddo
      close(10)

! reset accumulated flux
      yfsst2a(:,:)=0.0

      return
      end



!====================================================================
!      SUBROUTINE OCEANGET
!====================================================================

      subroutine oceanget
      use oceanmod
      implicit none
      integer imin,ihou,iday,imon,iyea
      integer jm1,jm2
      real zgw1,zgw2

!     *********************
!     * get  annual cycle *
!     *********************

!     modified for 14 months version of SST
!     jm2: 0=Dec, 1-12:Months, 13=Jan

      if (nperpetual_ocean > 0) then
         imon = mod(nperpetual_ocean-1,30) + 1
         iday = nperpetual_ocean - imon * 30
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

      yclsst2(:) = zgw1 * yclsst(:,jm1) + zgw2 * yclsst(:,jm2) ! SST

      return
      end subroutine oceanget


!======================================================================
!     SUBROUTINE GETFLXCO
!======================================================================

      subroutine getflxco
      use oceanmod
      implicit none
      integer imin,ihou,iday,imon,iyea,jm1,jm2
      real zgw1,zgw2
!
!     dbug arrays
!
      real,allocatable :: zprf1(:),zprf2(:),zprf3(:)
!
!     get  annual cycle
!

      if (nperpetual_ocean > 0) then
         imon = mod(nperpetual_ocean-1,30) + 1
         iday = nperpetual_ocean - imon * 30
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

      yfsst2(:)=zgw1*yfsst(:,jm1)+zgw2*yfsst(:,jm2)
!
!
!     print dbug information if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,yfsst2,1)
       call mpgagp(zprf2,yfsst(:,jm1),1)
       call mpgagp(zprf3,yfsst(:,jm2),1)
       if(mypid==NROOT) then
        print*,'interpolated flux correction : ',zprf1(nprhor)
        print*,'from: ',zprf2(nprhor),' and ',zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
      return
      end subroutine getflxco

!======================================================================
!     SUBROUTINE MKSST
!======================================================================
!
      subroutine mksst
      use oceanmod
      implicit none
      real zcpsdt
      integer jlev
!
!     compute new temperatures
!
      real :: zflx(NHOR)
      real(kind=8) :: zold(NHOR,NLEV_OCE)
      real(kind=8) :: zsst(NHOR,NLEV_OCE) ! to obtain the needed precision
!
!     dbug arrays
!
      real,allocatable :: zprf1(:),zprf2(:),zprf3(:)
!
!     entropy
!
      real,allocatable :: ztentro(:)
!
!     use high precision
!
      zsst(:,:)=ysst(:,:)
!
!     set some useful bits
!
      zcpsdt=dtmix/(CRHOS*CPS)
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       where(yls(:) < 1.)
        yentro(:,1)=CPS*CRHOS*ymld(:,1)*log(zsst(:,1))
        yentro(:,2)=yheat(:)/zsst(:,1)
       endwhere
      endif
!
!     add heat fluxes from the atmosphere and the deep ocean
!     if there is ice, use the deep ocean hfl directly for ice change
!
      yiflux(:)=0.
      where(yls(:) < 1. .and. yiced(:) <= 0.)
       zsst(:,1)=zsst(:,1)+(yheat(:)+yfldo(:))*zcpsdt/ymld(:,1)
      endwhere
      where(yls(:) < 1. .and. yiced(:) > 0.)
       yiflux(:)=yiflux(:)-yfldo(:)-yheat(:)
      endwhere
!
!     print dbug information if needed
!
      if (nprint==2) then
       ysst(:,1)=zsst(:,1)
       allocate(zprf1(NLON*NLAT))
       call mpgagp(zprf1,ysst(:,1),1)
       if(mypid==NROOT) then
        print*,'in mksst:'
        print*,' '
        print*,'new sst from heatflx: ',zprf1(nprhor)
       endif
       deallocate(zprf1)
      endif
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       allocate(ztentro(NHOR))
       ztentro(:)=zsst(:,1)
      endif
!
!     comput residual flux going into sea ice (if sst < tfreeze)
!
      call mkiflux(zsst(:,1))
!
!     make horizontal diffusion (new temperatures)
!
      if(nhdiff > 0) then
       where(yls(:) < 1.)
        zold(:,1)=zsst(:,1)
       endwhere
       call hdiffo(zsst)
!
!     diagnose hdiff flux used for dsst/dt
!
       where(yls(:) < 1.)
        yqhd(:)=(zsst(:,1)-zold(:,1))*ymld(:,1)/zcpsdt
       endwhere
!
!     use residual flux (into ice) to cool water again (if > tfreeze)
!
       where(zsst(:,1) > TFREEZE .and. yiflux(:) > 0. .and. yls(:) < 1.)
        zflx(:)=(TFREEZE-zsst(:,1))*ymld(:,1)/zcpsdt
        zflx(:)=AMAX1(zflx(:),-yiflux(:))
        zsst(:,1)=zsst(:,1)+zflx(:)*zcpsdt/ymld(:,1)
        yiflux(:)=yiflux(:)+zflx(:)
       endwhere
      endif
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       where(yls(:) < 1.)
        yentro(:,4)=yqhd(:)/zold(:,1)
       endwhere
      endif
!
!     print dbug information if needed
!
      if (nprint==2) then
       ysst(:,1)=zsst(:,1)
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,ysst(:,1),1)
       call mpgagp(zprf2,yqhd,1)
       call mpgagp(zprf3,yiflux,1)
       if(mypid==NROOT) then
        print*,'new sst from horizonral diffusion: ',zprf1(nprhor)
        print*,'heat from horizontal diffusion: ',zprf2(nprhor)
        print*,'new residual heat flux: ',zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
!     make vertical diffusion (new temperatures) if more than 1 layer
!
      do jlev=1,NLEV_OCE
       where(yls(:) < 1.)
        zold(:,jlev)=zsst(:,jlev)
       endwhere
      enddo
!
!     vertical diffusion if more than 1 layer
!
      if(NLEV_OCE > 1) then
       call vdiffo(zsst)
!
!     diagnose vdiff flux used for dsst/dt
!
       where(yls(:) < 1.)
        ydsst(:)=(zsst(:,1)-zold(:,1))*ymld(:,1)/zcpsdt
       endwhere
!
!     use residual flux (into ice) to cool water again (if > tfreeze)
!
       where(zsst(:,1) > TFREEZE .and. yiflux(:) > 0. .and. yls(:) < 1.)
        zflx(:)=(TFREEZE-zsst(:,1))*ymld(:,1)/zcpsdt
        zflx(:)=AMAX1(zflx(:),-yiflux(:))
        zsst(:,1)=zsst(:,1)+zflx(:)*zcpsdt/ymld(:,1)
        yiflux(:)=yiflux(:)+zflx(:)
       endwhere
      endif
!
!     entropy diagnostics
!
      if(nentropy > 0) then
       yentro(:,5)=0.
       do jlev=1,NLEV_OCE
        where(yls(:) < 1.)
         yentro(:,5)=yentro(:,5)                                        &
     &              +(zsst(:,jlev)-zold(:,jlev))*ymld(:,jlev)           &
     &              /zcpsdt/zold(:,jlev)
        endwhere
       enddo
       where(yls(:) < 1.)
         yentro(:,6)=yiflux(:)/ztentro(:)
       endwhere
       deallocate(ztentro)
      endif
!
!     if coupled without flux correction:
!     force sst to be TFREEZE and use hfl to change ice
!
      if(nocean > 0 .and. nfluko == 0) then
       where (yiced(:) > 0. .and. yls(:) < 1.)
        zflx(:)=(TFREEZE-zsst(:,1))*ymld(:,1)/zcpsdt
        zsst(:,1)=zsst(:,1)+zflx(:)*zcpsdt/ymld(:,1)
        yiflux(:)=yiflux(:)+zflx(:)
       endwhere
      endif
!
!     normal precision
!
      ysst(:,:)=zsst(:,:)
!
!     print dbug information if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,ysst(:,1),1)
       call mpgagp(zprf2,ydsst,1)
       call mpgagp(zprf3,yiflux,1)
       if(mypid==NROOT) then
        print*,'new sst from diffusion: ',zprf1(nprhor)
        print*,'heat from diffusion: ',zprf2(nprhor)
        print*,'new residual heat flux: ',zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
      return
      end subroutine mksst
!
!======================================================================
!     SUBROUTINE MKIFLUX
!======================================================================
!
      subroutine mkiflux(psst)
      use oceanmod
      implicit none
      real zcpsdt
!
      real(kind=8) :: psst(NHOR)
      real :: zsst(NHOR)
!
!     dbug arrays
!
      real,allocatable :: zprf1(:),zprf2(:)
!
!     set some useful bits
!
      zcpsdt=dtmix/(CRHOS*CPS)
!
!     comput residual flux going into sea ice (if sst < tfreeze)
!
      where(ysst(:,1) < TFREEZE .and. yls(:) < 1.)
       yiflux(:)=yiflux(:)+(TFREEZE-psst(:))*ymld(:,1)/zcpsdt
       psst(:)=TFREEZE
      endwhere
!
!     print dbug information if needed
!
      if (nprint==2) then
       zsst(:)=psst(:)
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       call mpgagp(zprf1,zsst(:),1)
       call mpgagp(zprf2,yiflux,1)
       if(mypid==NROOT) then
        print*,'new sst from correction sst < tfreeze: ',zprf1(nprhor)
        print*,'residual heat flux into ice: ',zprf2(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
      endif
!
      return
      end subroutine mkiflux

!======================================================================
!     SUBROUTINE ADDFC
!======================================================================

      subroutine addfc
      use oceanmod
      implicit none
      integer jhor
      real zcpsdt
!
!     add oceanic flux correction (prescribed advection)
!
      real :: zflx(NHOR)
      real(kind=8) :: zsst(NHOR)    ! to obtain the precision needed
      real(kind=8) :: zssto(NHOR)   ! old t for entropy diagnostics
!
!     dbug arrays
!
      real,allocatable :: zprf1(:),zprf2(:),zprf3(:)
!
      zcpsdt=dtmix/(CRHOS*CPS)
!
      yifluxr(:)=0.
!
!     use high precesion
!
      zsst(:)=ysst(:,1)
      if(nentropy > 0) zssto(:)=zsst(:)
!
!     distinguish diffent cases to treat sea ice 
!
      do jhor=1,NHOR
       if(yls(jhor) < 1.) then
        if(yiced(jhor) <= 0.) then
!
!       a) no ice: add fluco
!
         zsst(jhor)=zsst(jhor)+yfsst2(jhor)*zcpsdt/ymld(jhor,1)
        else
         if(ycliced(jhor) > 0.) then
!
!       b) ice and climatological ice: use fluco to get T -> Tclim
!          (i.e. Tclim is used as 'freezing temperature') and 
!          give the residuum to the ice (melt/freeze).
!
          if(zsst(jhor) > yclsst2(jhor) .and. yfsst2(jhor) < 0.) then
           zflx(jhor)=(yclsst2(jhor)-zsst(jhor))*ymld(jhor,1)/zcpsdt
           zflx(jhor)=AMAX1(zflx(jhor),yfsst2(jhor))
           zsst(jhor)=zsst(jhor)+zflx(jhor)*zcpsdt/ymld(jhor,1)
           yifluxr(jhor)=yifluxr(jhor)-(yfsst2(jhor)-zflx(jhor))
          elseif(zsst(jhor) < yclsst2(jhor) .and. yfsst2(jhor) > 0.) then
           zflx(jhor)=(yclsst2(jhor)-zsst(jhor))*ymld(jhor,1)/zcpsdt
           zflx(jhor)=AMIN1(zflx(jhor),yfsst2(jhor))
           zsst(jhor)=zsst(jhor)+zflx(jhor)*zcpsdt/ymld(jhor,1)
           yifluxr(jhor)=yifluxr(jhor)-(yfsst2(jhor)-zflx(jhor))
          else
           yifluxr(jhor)=yifluxr(jhor)-yfsst2(jhor)
          endif
         else 
!
!       c) ice but no climatological ice: use fluco to get T -> TFREEZE
!          and give the residuum to the ice (melt/freeze).
!

          if(zsst(jhor) > TFREEZE .and. yfsst2(jhor) < 0.) then
           zflx(jhor)=(TFREEZE-zsst(jhor))*ymld(jhor,1)/zcpsdt
           zflx(jhor)=AMAX1(zflx(jhor),yfsst2(jhor))
           zsst(jhor)=zsst(jhor)+zflx(jhor)*zcpsdt/ymld(jhor,1)
           yifluxr(jhor)=yifluxr(jhor)-(yfsst2(jhor)-zflx(jhor))
          else
           yifluxr(jhor)=yifluxr(jhor)-yfsst2(jhor)
          endif
         endif
        endif
       endif
      enddo
!
!     use residual flux (into ice) to cool water again (if > tfreeze)
!
      where(zsst(:) > TFREEZE .and. yiflux(:) > 0. .and. yls(:) < 1.)
       zflx(:)=(TFREEZE-zsst(:))*ymld(:,1)/zcpsdt
       zflx(:)=AMAX1(zflx(:),-yiflux(:))
       zsst(:)=zsst(:)+zflx(:)*zcpsdt/ymld(:,1)
       yiflux(:)=yiflux(:)+zflx(:)
      endwhere
!
!     omit temperatures below tfreeze
!
      where(zsst(:) < TFREEZE .and. yls(:) < 1.)
       yifluxr(:)=yifluxr(:)+(TFREEZE-zsst(:))*ymld(:,1)/zcpsdt
       zsst(:)=TFREEZE
      endwhere
!
!      entropy diagnostics
!
      if(nentropy > 0) then 
       where(yls < 1.)
        yentro(:,3)=(zsst(:)-zssto(:))*ymld(:,1)/zcpsdt/zssto(:)
       endwhere
      endif
!
!     print dbug information if needed
!
      if (nprint==2) then
       ysst(:,1)=zsst(:)
       allocate(zprf1(NLON*NLAT))
       allocate(zprf2(NLON*NLAT))
       allocate(zprf3(NLON*NLAT))
       call mpgagp(zprf1,ysst(:,1),1)
       call mpgagp(zprf2,yiflux(:),1)
       call mpgagp(zprf3,yifluxr(:),1)
       if(mypid==NROOT) then
        print*,'in addfc: '
        print*,' '
        print*,'new sst from fluko: ',zprf1(nprhor)
        print*,'new heat flux into ice: ',zprf2(nprhor)
        print*,'new residual (fluko) heat flux into ice: ',zprf3(nprhor)
       endif
       deallocate(zprf1)
       deallocate(zprf2)
       deallocate(zprf3)
      endif
!
!     normal precesion
!
      ysst(:,1)=zsst(:)
!
      return
      end subroutine addfc

!======================================================================
!     SUBROUTINE MKIFLX
!======================================================================
!
      subroutine mkiflx
      use oceanmod
      implicit none
      real :: zsum(2)
!
!     debug arrays
!
      real,allocatable :: zprf1(:)
!
!     since ifluxr could be locally large a global avergage 
!     ifluxr is computed which goes into the sea ice model
!
      zsum(1)=SUM(yifluxr(:)*gw(:),MASK=(yls(:) < 1.))
      zsum(2)=SUM(gw(:),MASK=(yls(:) < 1.))
      call mpsumbcr(zsum,2)
      if(zsum(1) /= 0.) then
       where(yls(:) < 1.) 
        yifluxr(:)=zsum(1)/zsum(2)
        yiflux(:)=yiflux(:)+yifluxr(:)
       end where
      endif
!
!     print dbug information if needed
!
      if (nprint==2) then
       allocate(zprf1(NLON*NLAT))
       call mpgagp(zprf1,yiflux,1)
       if(mypid==NROOT) then
        print*,'in mkiflx: '
        print*,' '
        print*,' global sums 1 and 2: ',zsum(1:2)
        print*,'new residual heat flux into ice: ',zprf1(nprhor)
       endif
       deallocate(zprf1)
      endif
!
      return
      end subroutine mkiflx

!====================================================================
!     SUBROUTINE MKFC
!====================================================================

      subroutine mkfc
      use oceanmod
      implicit none
      real zcm
!
      zcm=CRHOS*CPS
!
      if (taunc > 0.) then
       where(yls(:) < 1.)
        yfsst2(:)=zcm*(yclsst2(:)-ysst(:,1))*ymld(:,1)/taunc
       endwhere
      else
       where(yls(:) < 1.)
        yfsst2(:)=zcm/dtmix*(yclsst2(:)-ysst(:,1))*ymld(:,1)-yheat(:)
       endwhere
      endif
!
      return
      end subroutine mkfc

!     =================
!     SUBROUTINE VDIFFO
!     =================

      subroutine vdiffo(psst)
!
!     compute new sst due to vertical diffusion
!
      use oceanmod
      implicit none
      integer jlev,jlem,jlep
      integer :: nlem_oce = NLEV_OCE - 1
!
!     note: bounds of local arrays due to compiler, only 1:nlem_oce used
!
      real(kind=8) psst(NHOR,NLEV_OCE)
!
      real(kind=8) :: zk(NHOR,0:NLEV_OCE)  = 0. ! modfied diffusion coefficient
      real(kind=8) :: ztn(NHOR,0:NLEV_OCE) = 0. ! new temperature
      real(kind=8) :: zebs(NHOR,0:NLEV_OCE)= 0. ! array for back substitution
!
!     modified diffusion coeffizient
!
      do jlev=1,nlem_oce
       where(yls(:) < 1.)
        zk(:,jlev)=dtmix*vdiffk(jlev)*2./(ymld(:,jlev+1)+ymld(:,jlev))
       endwhere
      enddo
!
!     semi implizit scheme:
!
!     a) top layer elimination
!
      where(yls(:) < 1.)
       zebs(:,1)=zk(:,1)/(ymld(:,1)+zk(:,1))
       ztn(:,1)=ymld(:,1)*psst(:,1)/(ymld(:,1)+zk(:,1))
      endwhere
!
!     b) middle layer elimination
!
      if(NLEV_OCE > 2) then
       do jlev=2,nlem_oce
        jlem=jlev-1
        where(yls(:) < 1.)
         zebs(:,jlev)=zk(:,jlev)/(ymld(:,jlev)+zk(:,jlev)               &
     &                           +zk(:,jlem)*(1.-zebs(:,jlem)))
         ztn(:,jlev)=(psst(:,jlev)*ymld(:,jlev)+zk(:,jlem)*ztn(:,jlem)) &
     &              /(ymld(:,jlev)+zk(:,jlev)                           &
     &               +zk(:,jlem)*(1.-zebs(:,jlem)))
        endwhere
       enddo
      endif
!
!     c) bottom layer elimination (new temperature)
!
      where(yls(:) < 1.)
       psst(:,NLEV_OCE)=(psst(:,NLEV_OCE)*ymld(:,NLEV_OCE)              &
     &                  +zk(:,nlem_oce)*ztn(:,nlem_oce))                &
     &                 /(ymld(:,NLEV_OCE)+zk(:,nlem_oce)                &
     &                                   *(1.-zebs(:,nlem_oce)))
      endwhere
!
!     d) back-substitution
!
      do jlev=nlem_oce,1,-1
       jlep=jlev+1
       where(yls(:) < 1.)
        psst(:,jlev)=ztn(:,jlev)+zebs(:,jlev)*psst(:,jlep)
       endwhere
      enddo
!
      return
      end

!     =================
!     SUBROUTINE HDIFFO
!     =================

      subroutine hdiffo(psst)
      use oceanmod
      implicit none
      integer nsub,jlev,jlat,jlon,jit,jlap,jlam,jlom
      real zdelt,zfac,zztm,zzgw
      parameter(nsub=100)
!
      real(kind=8) :: psst(NHOR,NLEV_OCE)
      real :: zsst(NHOR,NLEV_OCE)
!
      real :: zt(NLON,NLAT),zls(NLON,NLAT)
      real :: zdtx(0:NLON,NLAT),zdty(NLON,0:NLAT)
      real :: zdtdt(NLON,NLAT),zentro(NLON,NLAT)
      real :: zdtdtp(NHOR),zentrop(NHOR)
      real :: ztold(NLON,NLAT)
!
      zsst(:,:)=psst(:,:)
!
      if(nentropy > 0) then
       yentro(:,7)=0.
      endif
      zdelt=dtmix/real(nsub)
      call mpgagp(zls,yls,1)
!
      do jlev=1,NLEV_OCE
       zfac=hdiffk(jlev)/plarad/plarad
!
       call mpgagp(zt,zsst(:,jlev),1)
!
       if(mypid==NROOT) then
        if(nentropy > 0) then
         where(zls(:,:) < 1.) zentro(:,:)=0.
        endif
        zztm=0.
        zzgw=0.
        do jlat=1,NLAT
        do jlon=1,NLON
         zztm=zztm+zt(jlon,jlat)*dmue(jlat)
         zzgw=zzgw+dmue(jlat)
        enddo
        enddo
        zztm=zztm/zzgw
        zt(:,:)=zt(:,:)-zztm
        ztold(:,:)=zt(:,:)
!
        do jit=1,nsub
         where(zls(2:NLON,:) < 1. .and. zls(1:NLON-1,:) < 1.)
          zdtx(1:NLON-1,:)=(zt(2:NLON,:)-zt(1:NLON-1,:))/dlam
         elsewhere
          zdtx(1:NLON-1,:)=0.
         endwhere
         where(zls(1,:) < 1. .and. zls(NLON,:) < 1.)
          zdtx(0,:)=(zt(1,:)-zt(NLON,:))/dlam
         elsewhere
          zdtx(0,:)=0.
         endwhere
         zdtx(NLON,:)=zdtx(0,:)
!
         do jlat=1,NLAT-1
          jlap=jlat+1
          where(zls(:,jlap) < 1. .and. zls(:,jlat) < 1.)
           zdty(:,jlat)=(zt(:,jlap)-zt(:,jlat))*cphih(jlat)/dphi(jlat)
          elsewhere
           zdty(:,jlat)=0.
          endwhere
         enddo
         zdty(:,0)=0.
         zdty(:,NLAT)=0.
!
         do jlat=1,NLAT
          jlam=jlat-1
          do jlon=1,NLON
           jlom=jlon-1
           if(zls(jlon,jlat) < 1.) then
            zdtdt(jlon,jlat)=((zdtx(jlon,jlat)-zdtx(jlom,jlat))         &
     &                        /dlam/cphi(jlat)/cphi(jlat)               &
     &                       +(zdty(jlon,jlat)-zdty(jlon,jlam))         &
     &                        /dmue(jlat))                              &
     &                      *zfac
           endif
          enddo
         enddo
!
         if(nentropy > 0) then
          where(zls(:,:) < 1.)
           zentro(:,:)=zdtdt(:,:)/(zt(:,:)+zztm)+zentro(:,:)/real(nsub)
          endwhere
         endif
         where(zls(:,:) < 1.) zt(:,:)=zt(:,:)+zdtdt(:,:)*zdelt
        enddo
        where(zls(:,:) < 1.) zdtdt(:,:)=(zt(:,:)-ztold(:,:))/dtmix
       endif
!
       call mpscgp(zdtdt,zdtdtp,1)
       where(yls(:) < 1.)
        psst(:,jlev)=psst(:,jlev)+zdtdtp(:)*dtmix
       endwhere
       if(nentropy > 0) then
        call mpscgp(zentro,zentrop,1)
        where(yls(:) < 1)
         yentro(:,7)=yentro(:,7)+zentrop(:)*CRHOS*CPS*ymld(:,jlev)
        endwhere
       endif
      enddo
!
      end subroutine hdiffo


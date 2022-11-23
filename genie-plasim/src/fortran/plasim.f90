!     ******************************************************************
!     *         Portable University Model of the Atmosphere            *
!     ******************************************************************

!     Frank (Larry) Lunkeit & Edilbert Kirk
!     Meteorologisches Institut
!     Universitaet Hamburg

!     based on SGCM (Simple Global Circulation Model)
!     by Ian James and James Dodd (April 1993)
!     Dept of Meteorology, University of Reading, UK.

!     With relaxation to basic restoration temerature state
!     The state is set in SETZT(EX) for initial runs,
!     on a time scale which can vary with the level
!     and which is entered in the namelist in restim(NLEV).
!     There is a linear drag which can again vary with level,
!     the time scale is entered for each level in days in tfrc(NLEV).
!     the diffusion is del-ndel with a time scale for diffusion of
!     tdiss days on every variable at the truncation wavenumber.


!     **************
!     * Name rules *
!     **************

!     i - local integer
!     j - loop index
!     k - integer dummy parameter
!     N - integer constants

!     g - real gridpoint arrays
!     p - real dummy parameter
!     s - real spectral arrays
!     z - local real


!     =================
!     SUBROUTINE PROLOG
!     =================

      subroutine prolog(dum_gonyear,dum_plngo,atchem_co2)
      use pumamod
      implicit none
      integer jpro,jlat,i,j,jhor
      real zmeanoro,zdlnp,zsec
      real atchem_co2
      integer dum_gonyear ! number of goldtsein steps per year
      integer dum_plngo   ! number of plasim steps pe goldstein steps

      logical :: lrestart


! derive local variables from GENIE global variables
      mpstep = (60*24*n_days_per_year)/(dum_plngo*dum_gonyear)
      genie_co2=atchem_co2*1.0e6

!     ************************************************************
!     * Initializations that cannot be run on parallel processes *
!     ************************************************************

      print*,mypid,NROOT
      if (mypid == NROOT) then
         call cpu_time(tmstart)
         write(*,'(/," ****************************************************")')
         write(*,'(" * PLANET SIMULATOR ",a31," *")') trim(pumaversion)
         write(*,'(" ****************************************************")')
         write(*,'(" * NTRU =",i4,"  NLEV =",i4,"  NLON = ",i4,"   NLAT =",i4," *")') &
            NTRU,NLEV,NLON,NLAT
         write(*,'(" ****************************************************")')
         if (NPRO > 1) then
           write(*,'(/," ****************************************************")')
           do jpro = 1 , NPRO
              write (*,'(" * CPU",i4,1x,a40," *")') jpro-1,ympname(jpro)
           enddo
           write(*,'(" ****************************************************")')
         endif

         call readnl                ! Open and read <puma_namelist>

!APM moitsure flux correction. Note default apm=0.0, no flux correction
         open(646,file=trim(indir_name)//'/apm.dat')
         do j=nlat,1,-1
           read(646,*) (genie_apm(i,j),i=1,nlon)
         enddo
         close(646)
         genie_apm=genie_apm*scale_apm

         print*,trim(rstdir_name)//'/plasim_status'
         call restart_ini(lrestart,trim(rstdir_name)//'/plasim_status')
         if (lrestart) then
            nrestart = 1
            nkits    = 0
         endif
         call surface_ini           ! Read <surface.txt>
         call inigau_plasim(NLAT,sid,gwd)  ! Gaussian abscissas and weights
         call inilat                ! Set latitudinal arrays
         call initpm                ! Several initializations
         call initsi                ! Initialize semi implicit scheme
         call outini                ! Open output file <puma_output>
         if (nsela > 0) call tracer_ini0 ! initialize tracer data 

      endif ! (mypid == NROOT)

!     ***********************
!     * broadcast & scatter *
!     ***********************

      call mpscdn(sid ,NLPP)  ! sine of latitude (kind=8)
      call mpscdn(gwd ,NLPP)  ! gaussian weights (kind=8)
      call mpscrn(csq ,NLPP)  ! cosine squared of latitude


      do jlat = 1 , NLPP
         deglat(jlat) = 180.0 / PI * asin(sid(jlat))
         cola(jlat) = sqrt(csq(jlat))
         rcsq(1+(jlat-1)*NLON:jlat*NLON) = 1.0 / csq(jlat)
      enddo

      call mpbci(mars    ) ! switch for Mars parameter
      call mpbci(nfixorb ) ! Global switch to fix orbit
      call mpbci(ngenie)   ! Switch for 1-way/2-way coupling with GENIE
      call mpbci(kick    ) ! add noise for kick > 0
      call mpbci(noutput ) ! write data interval
      call mpbci(nafter  ) ! write data interval
      call mpbci(nwpd    ) ! number of writes per day
      call mpbci(ncoeff  ) ! number of modes to print
      call mpbci(ndiag   ) ! write diagnostics interval
      call mpbci(nexp    ) ! experiment number
      call mpbci(nexper  ) ! 1: dipole experiment
      call mpbci(sellon )  ! index of longitude for column mode
      call mpbci(nkits   ) ! number of initial timesteps
      call mpbci(nrestart) ! 1: read restart file 0: initial run
      call mpbci(nqspec  ) ! 1: spectral q 0: grodpoint q
      call mpbci(nsela   ) ! 1: semi lagrangian advection enabled

      call mpbci(nstep   ) ! current timestep
      call mpbci(mstep   ) ! current timestep in month
      call mpbci(ntspd   ) ! number of timesteps per day

      call mpbci(mpstep)   ! minutes per timestep
      call mpbci(n_days_per_month)
      call mpbci(n_days_per_year)
      call mpbci(n_start_step)
      call mpbci(n_start_year)
      call mpbci(n_start_month)

      call mpbci(nflux   ) !
      call mpbci(nadv    ) !
      call mpbci(nhordif ) !
      call mpbci(nrad    ) !
      call mpbci(neqsig  ) ! switch for equidistant sigma levels
      call mpbci(nhdiff  ) ! critical wavenumber for horizonal diffusion

      call mpbci(nprint    ) ! switch for extensive diagnostic pintout (dbug)
      call mpbci(nprhor    ) ! grid point to be printed (dbug)
      call mpbci(ndiaggp   ) ! switch for franks grid point diagnostics
      call mpbci(ndiagsp   ) ! switch for franks spectral diagnostics
      call mpbci(ndiagcf   ) ! switch for cloud forcing diagnostics
      call mpbci(nentropy  ) ! switch for entropy diagnostics
      call mpbci(nenergy  )  ! switch for energy diagnostics
      call mpbci(ndiaggp3d ) ! no of 3d gp diagnostic arrays
      call mpbci(ndiaggp2d ) ! no of 2d gp diagnostic arrays
      call mpbci(ndiagsp3d ) ! no of 3d sp diagnostic arrays
      call mpbci(ndiagsp2d ) ! no of 2d sp diagnostic arrays
      call mpbci(ntime     ) ! switch to activate time consuming estimate
      call mpbci(nperpetual) ! day of perpetual integration
      call mpbci(ndheat)     ! switch for heating due to momentum dissipation
      call mpbci(nsponge)    ! switch for top sponge layer

      call mpbcr(acpd    )   ! Specific heat for dry air
      call mpbcr(adv     )
      call mpbcr(akap    )
      call mpbcr(alr     )
      call mpbcr(cv      )
      call mpbcr(ct      )
      call mpbcr(dtep    )
      call mpbcr(dtns    )
      call mpbcr(dtrop   )
      call mpbcr(dttrp   )
      call mpbcr(ga      )
      call mpbcr(gascon  )
      call mpbcr(tgr     )
      call mpbcr(plarad  )
      call mpbcr(pnu     )
      call mpbcr(pnu21   )
      call mpbcr(psurf   )
      call mpbcr(ra1     )
      call mpbcr(ra2     )
      call mpbcr(ra4     )
      call mpbcr(rdbrv   )
      call mpbcr(ww      )
      call mpbcr(solar_day)
      call mpbcr(siderial_day)
      call mpbcr(rotspd)
      call mpbcr(eccen)
      call mpbcr(obliq)
      call mpbcr(mvelp)
      call mpbcr(plavor)
      call mpbcr(dampsp)

      call mpbcin(ndel  ,NLEV) ! ndel
      call mpbcin(ndl   ,NLEV)

      call mpbcrn(tdissd,NLEV)
      call mpbcrn(tdissz,NLEV)
      call mpbcrn(tdisst,NLEV)
      call mpbcrn(tdissq,NLEV)
      call mpbcrn(damp  ,NLEV)
      call mpbcrn(dsigma,NLEV)
      call mpbcrn(rdsig ,NLEV)
      call mpbcrn(restim,NLEV)
      call mpbcrn(sigma ,NLEV)
      call mpbcrn(sigmah,NLEV)
      call mpbcrn(t0    ,NLEV)
      call mpbcrn(t01s2 ,NLEV)
      call mpbcrn(tfrc  ,NLEV)
      call mpbcrn(tkp   ,NLEV)

      call mpbcrn(c     ,NLSQ)
      call mpbcrn(g     ,NLSQ)
      call mpbcrn(tau   ,NLSQ)

      call mpscin(nindex,NSPP)
      call mpscsp(sak,sakpp,NLEV)
      call mpbcrn(sigh  ,NLEV)

!     Copy some calendar variables to calmod

      call calini(n_days_per_month,n_days_per_year,n_start_step,ntspd,solar_day)
!restart value overwritten. In any event the timestep is overwritten in master as
!it is now controlled by GENIE
      nstep = n_start_step ! timestep since 01-01-0001
      call updatim(nstep)  ! set date & time array ndatim
!
!     allocate additional diagnostic arrays, if switched on
!

      if(ndiaggp2d > 0) then
       allocate(dgp2d(NHOR,ndiaggp2d))
       dgp2d(:,:)=0.
      end if
      if(ndiaggp3d > 0) then
       allocate(dgp3d(NHOR,NLEV,ndiaggp3d))
       dgp3d(:,:,:)=0.
      end if
      if(ndiagsp2d > 0) then
       allocate(dsp2d(NESP,ndiagsp2d))
       dsp2d(:,:)=0.
      end if
      if(ndiagsp3d > 0) then
       allocate(dsp3d(NESP,NLEV,ndiagsp3d))
       dsp3d(:,:,:)=0.
      end if
      if(ndiagcf > 0) then
       allocate(dclforc(NHOR,7))
       dclforc(:,:)=0.
      end if
      if(nentropy > 0) then
       allocate(dentropy(NHOR,33))
       allocate(dentrot(NHOR,NLEV))
       allocate(dentroq(NHOR,NLEV))
       allocate(dentrop(NHOR))
       dentropy(:,:)=0.
      end if
      if(nenergy > 0) then
       allocate(denergy(NHOR,28))
       denergy(:,:)=0.
      end if

      call legini

      if (nrestart > 0) then
         call read_atmos_restart
!option added to add noise to restart for initial condition ensemble from spin up.
         if(kick.gt.0) call noise_plasim
      else
         call initfd
      endif

!
!*    initialize miscellaneous additional parameterization
!     which are included in *miscmod*
!

      call miscini

!
!*    initialize surface fluxes and vertical diffusion
!

      call fluxini

!
!*    initialize radiation
!

      call radini

!
!*    initialize convective and large scale rain and clouds
!

      call rainini

!
!*    initialize surface parameterizations or models
!

      call surfini

!
!*    reset psurf according to orography
!

      if (mypid == NROOT) then
         write (*,'(" Surface pressure with no topography = ",f10.2," [hPa]")')&
            psurf * 0.01
         zmeanoro = so(1) * cv * cv / sqrt(2.0)
         zdlnp    = zmeanoro / gascon / tgr
         psurf    = EXP(LOG(psurf)-zdlnp)
         write (*,'(" Mean of topographic height          = ",f10.2," [m]")')&
            zmeanoro / ga
         write (*,'(" Mean of surface pressure            = ",f10.2," [hPa]")')&
            psurf * 0.01
      end if
      call mpbcr(psurf)

!
!*    broadcast and scatter
!

      call mpbcrn(sp,NESP)
      call mpbcrn(sd,NESP*NLEV)
      call mpbcrn(st,NESP*NLEV)
      call mpbcrn(sz,NESP*NLEV)
      call mpbcrn(sq,NESP*NLEV)

      call mpscsp(sd,sdp,NLEV)
      call mpscsp(st,stp,NLEV)
      call mpscsp(sz,szp,NLEV)
      call mpscsp(sq,sqp,NLEV)
      call mpscsp(sr,srp,NLEV)
      call mpscsp(sp,spp,1)
      call mpscsp(so,sop,1)

!
!*    close the namelist file
!

      if(mypid==NROOT) close(11)

      if (mypid == NROOT) call print_planets

!
!*    open efficiency diagnostic file
!

      if(ndheat > 1 .and. mypid == NROOT) then
       open(9,file='efficiency.dat',form='formatted')
      endif


      call outreset !reset all accumulated diagnostics on restart
                    !this leads to problems when new outputs are added
                    !as old restart files do not contain data. Aslo it
                    !is not especially useful as strating accumulation
                    !at start of simulation is almost always the
                    !appropriate things to do. This is a left over from
                    !when the model was run from continuous restarts

! plasim grid on genie coordinate system, used for manipulating coupling
! outputs

      do i=1,NLON
        do j=1,NLAT
          jhor=(j-1)*NLON+i
          genie_area(i,j)=gwd(j)
          genie_mask(i,j)=dls(jhor)
        enddo
      enddo
      genie_area(:,:)=genie_area(:,:)*4.0*PI*(plarad**2)/sum(genie_area)

!
!*    start time consuming calculations
!

      if(ntime==1) then
       call mksecond(zsec,0.)
       time0=zsec
      endif

      open(unit=9191,file=trim(outdir_name)//'/annual.out')

      return
      end


!     ========================
!     SUBROUTINE PRINT_PLANETS
!     ========================

      subroutine print_planets
      use pumamod
      implicit none

      write (*,4000)
      write (*,1000)
      write (*,1100) 'This simulatiomn is for ',yplanet
      write (*,1000)
      write (*,2000) 'Parameter','Units','Earth','Mars'
      write (*,1000)
      write (*,3000) 'Mass'             ,'[10^24 kg]' ,   5.9736,   0.6419
      write (*,3000) 'Volume'           ,'[10^10 km3]', 108.321 ,  16.318 
      write (*,3000) 'Equatorial radius','[km]'       ,6378.0   ,3393.0
      write (*,3000) 'Polar radius'     ,'[km]'       ,6356.0   ,3373.0
      write (*,3000) 'Mean radius'      ,'[km]'       ,6371.0   ,3390.0
      write (*,3000) 'Ellipticity'      ,' '        ,   0.0034,   0.0065
      write (*,3000) 'Mean density'     ,'[kg/m3]'    ,5520.0   ,3933.0   
      write (*,3000) 'Surface gravity'  ,'[m/s2]'     ,GA_EARTH ,GA_MARS
      write (*,3000) 'Bond albedo'      ,' '        ,   0.385 ,   0.16
      write (*,3000) 'Solar irradiance' ,'[W/m2]' ,SOLCON_EARTH,SOLCON_MARS
      write (*,3000) 'Black-body temperature','[K]'   , 247.3   , 216.6
      write (*,3000) 'Topographic range'     ,'[km]'  ,  20.0   ,  36.0
      write (*,3000) 'Sidereal orbit period' ,'[days]', 365.256 , 686.980
      write (*,3000) 'Sidereal rotation period','[hrs]',23.9345 , 24.6229
      write (*,3000) 'Equatorial inclination'  ,'[deg]',23.44   , 23.98  
      write (*,3000) 'Perihelion'       ,'[10^6 km]'  , 147.1   , 206.6
      write (*,3000) 'Aphelion'         ,'[10^6 km]'  , 152.1   , 249.2
      write (*,3000) 'Orbit eccentricity'    ,' '   , 0.0167  , 0.0934
      write (*,1000)
      write (*,4000)
      return

 1000 format(60('*'))
 1100 format('* ',a24,1x,a31,' *')
 2000 format('* ',a24,1x,a11,2a10,' *')
 3000 format('* ',a24,1x,a11,2f10.4,' *')
 4000 format(/)
      end

!     =================
!     SUBROUTINE INI_STEPS
!     =================

      subroutine ini_steps(tstar_ocn,tstar_ice,hght_sic,frac_sic,albd_sic)
      use pumamod
      implicit none
      integer ikits,jkits,i,j,ip
      real tstar_ocn(NLON,NLAT)
      real tstar_ice(NLON,NLAT)
      real hght_sic(NLON,NLAT)
      real frac_sic(NLON,NLAT)
      real albd_sic(NLON,NLAT)

! COUPLING INPUTS      
      do i=1,nlon
       do j=1,nlat
        ip=i+(j-1)*nlon
        genie_sst(ip)=tstar_ocn(i,nlat-j+1)+273.15
        genie_icet(ip)=tstar_ice(i,nlat-j+1)+273.15
        genie_hght_sic(ip)=hght_sic(i,nlat-j+1)
        genie_frac_sic(ip)=frac_sic(i,nlat-j+1)
        genie_alb_sic(ip)=albd_sic(i,nlat-j+1)
       enddo
      enddo

!     ***************************
!     * short initial timesteps *
!     ***************************

      if(nrestart==0) then
       ikits = nkits
       do jkits=1,ikits
         deltsec  = (solar_day / ntspd) / (2**nkits)
         deltsec2 = deltsec + deltsec
         delt     = (TWOPI     / ntspd) / (2**nkits)
         delt2    = delt + delt
         call gridpointa
         call makebm
         call spectrala
         call gridpointd
         call spectrald
         nkits = nkits - 1
       enddo
      endif

!     ****************************************************************
!     * The scaling factor "ww" is derived from the rotation "omega" *
!     * with 1 planetary rotation per siderial day (2 Pi)            *
!     ****************************************************************

      deltsec  = solar_day / ntspd   ! timestep in seconds
      deltsec2 = deltsec + deltsec   ! timestep in seconds * 2
      delt     = TWOPI     / ntspd   ! timestep scaled
      delt2    = delt + delt
      call makebm

      if (mypid == NROOT .and. nsela > 0) then
         call tracer_ini
      endif

      nstep = n_start_step ! timestep since 01-01-0001
      call updatim(nstep)  ! set date & time array ndatim


      end subroutine ini_steps

!     =================
!     SUBROUTINE MASTER
!     =================

      subroutine master(                                         &
!input from GENIE
       kstep,                                                    &
       tstar_ocn,                                                &
       tstar_ice,hght_sic,frac_sic,albd_sic,                     &
       delta_flux,                                               &
       atchem_co2,                                               &
!output to GENIE
       sfxatm_lnd_plas,                                          &
       latent_plas,latent_coeff_plas,                            &
       sensible_plas,sensible_coeff_plas,                        &
       netsolar_plas,netlong_plas,                               &
       insolar_plas,inlong_plas,netheat_plas,                    &
       sat_plas,spec_hum_plas,pressure_plas,                     &
       evap_plas,precip_plas,runoff_plas,                        &
       stressx2_plas,stressy2_plas,stressx3_plas,stressy3_plas,  &
       windspeed_plas,solfor_plas)

      use pumamod
      implicit none
! input coupling variables
      real tstar_ocn(NLON,NLAT)
      real tstar_ice(NLON,NLAT)
      real hght_sic(NLON,NLAT)
      real frac_sic(NLON,NLAT)
      real albd_sic(NLON,NLAT)
      real delta_flux(4,NLON,NLAT)
      real atchem_co2
! output coupling variables, g variables for geared coupling
      real sfxatm_lnd_plas(NLON,NLAT) !carbon flux for carbon-cycle coupling
      real latent_plas(NLON,NLAT)
      real latent_coeff_plas(NLON,NLAT)
      real sensible_plas(NLON,NLAT)
      real sensible_coeff_plas(NLON,NLAT)
      real netsolar_plas(NLON,NLAT)
      real netlong_plas(NLON,NLAT)
      real solfor_plas(NLAT)
      real insolar_plas(NLON,NLAT)
      real inlong_plas(NLON,NLAT)
      real netheat_plas(NLON,NLAT)
      real sat_plas(NLON,NLAT)
      real spec_hum_plas(NLON,NLAT)
      real pressure_plas(NLON,NLAT)
      real evap_plas(NLON,NLAT)
      real precip_plas(NLON,NLAT)
      real runoff_plas(NLON,NLAT)
      real stressx_plas(NLON,NLAT)
      real stressy_plas(NLON,NLAT)
      real stressx2_plas(NLON,NLAT)
      real stressy2_plas(NLON,NLAT)
      real stressx3_plas(NLON,NLAT)
      real stressy3_plas(NLON,NLAT)
      real windspeed_plas(NLON,NLAT)
      real divisor !for averaging gearing data
      integer i,j,ip

      integer kstep
      integer koutdiag,iyea,imon

      integer gearing_status,k

      interface
         integer function ndayofyear(kstep)
            integer kstep
         end function ndayofyear
      end interface

!PBH
!to follow the plasim convention that 1st timestep = 1 year (don't
!really understand why has been set up this way...) 
      nstep=kstep+(n_days_per_year*24*60/mpstep)-1

!********************************************************************
!********************************************************************
! COUPLING INPUTS    
      genie_co2=atchem_co2*1.0e6 
      do i=1,nlon
       do j=1,nlat
        ip=i+(j-1)*nlon
        genie_sst(ip)=tstar_ocn(i,nlat-j+1)+273.15
        genie_icet(ip)=tstar_ice(i,nlat-j+1)+273.15
        genie_hght_sic(ip)=hght_sic(i,nlat-j+1)
        genie_frac_sic(ip)=frac_sic(i,nlat-j+1)
        genie_alb_sic(ip)=albd_sic(i,nlat-j+1)
       enddo
      enddo
!********************************************************************
!********************************************************************

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
!PLASIM-GOLDSTEIN GEARING
      gearing_status=0
      if(ngear.eq.1) then
        gearing_status=mod(int(kstep/(ngear_years_plasim*n_days_per_year*24*60/mpstep)),ngear_multiple)
        k=ndayofyear(kstep)
      endif

      if(gearing_status.gt.0) then
        divisor=float(ngear_years_plasim*24*60)/float(mpstep)

        latent_coeff_plas(:,:)=g_latent_coeff_plas(:,:,k)/divisor
        sensible_coeff_plas(:,:)=g_sensible_coeff_plas(:,:,k)/divisor
        netsolar_plas(:,:)=g_netsolar_plas(:,:,k)/divisor
        evap_plas(:,:)=g_evap_plas(:,:,k)/divisor
        precip_plas(:,:)=g_precip_plas(:,:,k)/divisor
        runoff_plas(:,:)=g_runoff_plas(:,:,k)/divisor
        solfor_plas(:)=g_solfor_plas(:,k)/divisor
        insolar_plas(:,:)=g_insolar_plas(:,:,k)/divisor
        inlong_plas(:,:)=g_inlong_plas(:,:,k)/divisor
        sat_plas(:,:)=g_sat_plas(:,:,k)/divisor
        spec_hum_plas(:,:)=g_spec_hum_plas(:,:,k)/divisor
        pressure_plas(:,:)=g_pressure_plas(:,:,k)/divisor
        windspeed_plas(:,:)=g_windspeed_plas(:,:,k)/divisor
        stressx2_plas(:,:)=g_stressx2_plas(:,:,k)/divisor
        stressy2_plas(:,:)=g_stressy2_plas(:,:,k)/divisor
        stressx3_plas(:,:)=g_stressx3_plas(:,:,k)/divisor
        stressy3_plas(:,:)=g_stressy3_plas(:,:,k)/divisor

!Coupling variables that depend upon ocean temperature are re-calculated. Needed for stability.
!Do not need to worry about values over sea ice as these are calculated in goldstein sea ice.
!Only needed over ocean. If ENTS is modularised this will need to be done over land too.
!Evaporation is NOT recalculated. This is necessary for moisture conservation. 
!The alternative of scaling pptn+runoff not obviously better (and more complicated).

        where(tstar_ocn.ne.0.0) !to avoid divide by zero over land  
!take out frac sic terms here.....
!          latent_plas(:,:)=1.0e-5*(gascon*ra1/rv)*exp(ra2_liquid*         &
!               (tstar_ocn(:,:)*(1.-frac_sic(:,:))+tstar_ice(:,:)*frac_sic(:,:))/ &
!               (tstar_ocn(:,:)*(1.-frac_sic(:,:))+tstar_ice(:,:)*frac_sic(:,:)+(tmelt-ra4)))/ &
!               pressure_plas(:,:)
          latent_plas(:,:)=1.0e-5*(gascon*ra1/rv) &
                          *exp(ra2_liquid*tstar_ocn(:,:)/(tstar_ocn(:,:)+(tmelt-ra4))) &
                          /pressure_plas(:,:)
          latent_plas(:,:)=-(latent_plas(:,:)-spec_hum_plas(:,:))*latent_coeff_plas(:,:)
          latent_plas(:,:)=alv*latent_plas(:,:)*1000.
          netlong_plas(:,:)=inlong_plas(:,:)-(0.98*5.67e-8*(tstar_ocn(:,:)+273.15)**4)
          sensible_plas(:,:)=sensible_coeff_plas(:,:)*(sat_plas(:,:)-tstar_ocn(:,:))
          netheat_plas(:,:)= latent_plas(:,:)+sensible_plas(:,:)+netsolar_plas(:,:)+netlong_plas(:,:)
        endwhere
        return

      else if (mod(kstep,(ngear_years_plasim*n_days_per_year*24*60/mpstep)).eq.1) then
        print*,"resetting coupling variables"
        g_latent_coeff_plas(:,:,:)=0.0
        g_sensible_coeff_plas(:,:,:)=0.0
        g_netsolar_plas(:,:,:)=0.0
        g_evap_plas(:,:,:)=0.0
        g_precip_plas(:,:,:)=0.0
        g_runoff_plas(:,:,:)=0.0
        g_solfor_plas(:,:)=0.0
        g_insolar_plas(:,:,:)=0.0
        g_inlong_plas(:,:,:)=0.0
        g_sat_plas(:,:,:)=0.0
        g_spec_hum_plas(:,:,:)=0.0
        g_pressure_plas(:,:,:)=0.0
        g_windspeed_plas(:,:,:)=0.0
        g_stressx2_plas(:,:,:)=0.0
        g_stressy2_plas(:,:,:)=0.0
        g_stressx3_plas(:,:,:)=0.0
        g_stressy3_plas(:,:,:)=0.0

      endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!        ************************************************************
!        * calculation of non-linear quantities in grid point space *
!        ************************************************************

      call gridpointa

!        ******************************
!        * adiabatic part of timestep *
!        ******************************

      call spectrala

!        *****************************
!        * diabatic part of timestep *
!        *****************************

      call gridpointd

      if (mypid == NROOT) then
         if (mod(nstep,nafter) == 0 .and. noutput > 0) then
            call outsp
         endif
         if (mod(nstep,ndiag) == 0) then
            call diag_plasim
         endif
      endif

      call spectrald
!changed to end 31/12 for consistency with GENIE annual outputs
!also need to subtract 1 year because plasim starts at 11520
      if (mod(nstep+1-(n_days_per_year*24*60/mpstep),nafter) == 0) then
        call outaccu
        if(noutput > 0) then
          call outgp
          koutdiag=ndiaggp3d+ndiaggp2d+ndiagsp3d+ndiagsp2d+ndiagcf     &
     &            +nentropy+nenergy
          if(koutdiag > 0) call outdiag
        endif
          call outreset
          call outrest !restart file output
      else
        call outaccu
      endif

      iyea  = ndatim(1)    ! current year
      imon  = ndatim(2)    ! current month
      nstep = nstep + 1
      mstep = mstep + 1
      call updatim(nstep)  ! set date & time array ndatim
      if (imon /= ndatim(2)) then
        if (mypid == NROOT) then
          write (*,"('Completed month ',I2.2,'-',I4.4)") imon,iyea  
        endif
        mstep = 0
      endif

      call stability_check ! aborts if model tends to explode

!******************************************************************
!******************************************************************
!COUPLING OUTPUTS
      solfor_plas(:)=0.0
      do i=1,nlon
       do j=1,nlat
        ip=i+(j-1)*nlon
! for goldstein ocean
        latent_plas(i,nlat-j+1)=dlhfl(ip)
        latent_coeff_plas(i,nlat-j+1)=dlhcoeff(ip)
        sensible_plas(i,nlat-j+1)=dshfl(ip)
        sensible_coeff_plas(i,nlat-j+1)=dshcoeff(ip)
        netsolar_plas(i,nlat-j+1)=dswfl(ip,NLEP)
        netlong_plas(i,nlat-j+1)=dlwfl(ip,NLEP)
        evap_plas(i,nlat-j+1)=devap(ip)*1000.0             !m/s -> mm/s
        precip_plas(i,nlat-j+1)=(dprc(ip)+dprl(ip))*1000.0 !m/s -> mm/s
        runoff_plas(i,nlat-j+1)=drunoff(ip)*1000.0         !m/s -> mm/s
! apm adjustment added into runoff
        runoff_plas(i,nlat-j+1)=(drunoff(ip)+genie_apm(i,nlat-j+1))*1000.0         !m/s -> mm/s

! for biogem
! note this is incoming solar (constant in longitude) averaged over longitude (i.e. could be outside longitude loop)
        solfor_plas(nlat-j+1)=solfor_plas(nlat-j+1)+dswfl(ip,1)-dfu(ip,1)
 
! for goldstein sea ice
        insolar_plas(i,nlat-j+1)= dswfl(ip,NLEP)-dfu(ip,NLEP)
        inlong_plas(i,nlat-j+1)=  dlwfl(ip,NLEP)-dftu(ip,NLEP)
        netheat_plas(i,nlat-j+1)= latent_plas(i,nlat-j+1) +   &
                                  sensible_plas(i,nlat-j+1) + &
                                  netsolar_plas(i,nlat-j+1) + &
                                  netlong_plas(i,nlat-j+1)
        sat_plas(i,nlat-j+1)=dtsa(ip)-273.15               !K -> degC
        spec_hum_plas(i,nlat-j+1)=dq(ip,NLEV)
        pressure_plas(i,nlat-j+1)=dp(ip)*1.e-5
!wind stresses at cell centres (not passed out but used for cell edges)
        stressx_plas(i,nlat-j+1)=dtaux(ip)
        stressy_plas(i,nlat-j+1)=dtauy(ip)
        windspeed_plas(i,nlat-j+1)= &
         sqrt(du(ip,NLEV)*du(ip,NLEV)+dv(ip,NLEV)*dv(ip,NLEV))
       enddo
      enddo
      solfor_plas(:)=solfor_plas(:)/float(nlon)
!wind stresses at velocity points (grid cell boundaries)
      do i=1,nlon
        do j=1,nlat
!u points
          if(i.lt.nlon) then
            stressx2_plas(i,j)=0.5*(stressx_plas(i,j)+stressx_plas(i+1,j))
            stressy2_plas(i,j)=0.5*(stressy_plas(i,j)+stressy_plas(i+1,j))
          else
            stressx2_plas(i,j)=0.5*(stressx_plas(i,j)+stressx_plas(1,j))
            stressy2_plas(i,j)=0.5*(stressy_plas(i,j)+stressy_plas(1,j))
          endif
!v points
          if(j.lt.nlat) then
            stressx3_plas(i,j)=0.5*(stressx_plas(i,j)+stressx_plas(i,j+1))
            stressy3_plas(i,j)=0.5*(stressy_plas(i,j)+stressy_plas(i,j+1))
          else
            stressx3_plas(i,j)=0.0
            stressy3_plas(i,j)=0.0
          endif
        enddo

!carbon fluxes
      enddo
      if(atchem_couple) then
        sfxatm_lnd_plas(:,:)=sfxatm_lnd(:,:)
        if(ntco2e.eq.1) then
!well mixed CO2 s assumed (flux units moles/m2/s)
          sfxatm_lnd_plas(:,:)=sfxatm_lnd_plas(:,:)+                   &
           co2emissions(ndatim(1))
        endif
      else
        sfxatm_lnd_plas(:,:)=0.0
      endif

!****************************************************************************
!****************************************************************************

!accumlate daily averages if gearing is being used
      if(ngear.eq.1) then
        g_latent_coeff_plas(:,:,k)=g_latent_coeff_plas(:,:,k)+latent_coeff_plas(:,:)
        g_sensible_coeff_plas(:,:,k)=g_sensible_coeff_plas(:,:,k)+sensible_coeff_plas(:,:)
        g_netsolar_plas(:,:,k)=g_netsolar_plas(:,:,k)+netsolar_plas(:,:)
        g_evap_plas(:,:,k)=g_evap_plas(:,:,k)+evap_plas(:,:)
        g_precip_plas(:,:,k)=g_precip_plas(:,:,k)+precip_plas(:,:)
        g_runoff_plas(:,:,k)=g_runoff_plas(:,:,k)+runoff_plas(:,:)
        g_solfor_plas(:,k)=g_solfor_plas(:,k)+solfor_plas(:)
        g_insolar_plas(:,:,k)=g_insolar_plas(:,:,k)+insolar_plas(:,:)
        g_inlong_plas(:,:,k)=g_inlong_plas(:,:,k)+inlong_plas(:,:)
        g_sat_plas(:,:,k)=g_sat_plas(:,:,k)+sat_plas(:,:)
        g_spec_hum_plas(:,:,k)=g_spec_hum_plas(:,:,k)+spec_hum_plas(:,:)
        g_pressure_plas(:,:,k)=g_pressure_plas(:,:,k)+pressure_plas(:,:)
        g_windspeed_plas(:,:,k)=g_windspeed_plas(:,:,k)+windspeed_plas(:,:)
        g_stressx2_plas(:,:,k)=g_stressx2_plas(:,:,k)+stressx2_plas(:,:)
        g_stressy2_plas(:,:,k)=g_stressy2_plas(:,:,k)+stressy2_plas(:,:)
        g_stressx3_plas(:,:,k)=g_stressx3_plas(:,:,k)+stressx3_plas(:,:)
        g_stressy3_plas(:,:,k)=g_stressy3_plas(:,:,k)+stressy3_plas(:,:)
      endif

      annsat=annsat+sum(sat_plas*genie_area)/sum(genie_area)
      if(mod(nstep,n_days_per_year*ntspd)==0) then
        annsat=annsat/float(n_days_per_year*ntspd)
        write(9191,919) (float(nstep)/float(n_days_per_year*ntspd))-1.0,annsat
        annsat=0.0
 919  format(1p10e11.3)
      endif

      return
      end subroutine master


!
!     ==================
!     SUBROUTINE OUTREST
!     ==================
      subroutine outrest
      use pumamod
      implicit none
!
!     write restart file
!

      if (mypid == NROOT) then
         call restart_prepare(trim(outdir_name)//'/plasim_status')
         call put_restart_integer('nstep'   ,nstep   )
         call put_restart_integer('naccuout',naccuout)
         call put_restart_integer('nlat'    ,NLAT    )
         call put_restart_integer('nlon'    ,NLON    )
         call put_restart_integer('nlev'    ,NLEV    )
         call put_restart_integer('nrsp'    ,NRSP    )

         call put_restart_array('sz',sz,NRSP,NESP,NLEV)
         call put_restart_array('sd',sd,NRSP,NESP,NLEV)
         call put_restart_array('st',st,NRSP,NESP,NLEV)
         if (nqspec == 1) call put_restart_array('sq',sq,NRSP,NESP,NLEV)
         call put_restart_array('sr',sr,NRSP,NESP,NLEV)
         call put_restart_array('sp',sp,NRSP,NESP,   1)
         call put_restart_array('so',so,NRSP,NESP,   1)
      endif

      call mpputsp('szm',szm,NSPP,NLEV)
      call mpputsp('sdm',sdm,NSPP,NLEV)
      call mpputsp('stm',stm,NSPP,NLEV)
      if (nqspec == 1) call mpputsp('sqm',sqm,NSPP,NLEV)
      call mpputsp('spm',spm,NSPP,   1)
!
!     gridpoint restart
!
      call mpputgp('dls'    ,dls    ,NHOR,1)
      call mpputgp('drhs'   ,drhs   ,NHOR,1)
      call mpputgp('dalb'   ,dalb   ,NHOR,1)
      call mpputgp('dz0'    ,dz0    ,NHOR,1)
      call mpputgp('dicec'  ,dicec  ,NHOR,1)
      call mpputgp('diced'  ,diced  ,NHOR,1)
      call mpputgp('dwatc'  ,dwatc  ,NHOR,1)
      call mpputgp('drunoff',drunoff,NHOR,1)
      call mpputgp('dveg'   ,dveg   ,NHOR,1)
      call mpputgp('dforest',dforest,NHOR,1)
      call mpputgp('dust3'  ,dust3  ,NHOR,1)
      call mpputgp('dcc'    ,dcc    ,NHOR,NLEV)
      call mpputgp('dql'    ,dql    ,NHOR,NLEV)
      call mpputgp('dqsat'  ,dqsat  ,NHOR,NLEV)
      call mpputgp('dt'     ,dt(1,NLEP),NHOR,1)
      if (nqspec == 1) then ! spectral: save only soil humidity
         call mpputgp('dq'  ,dq(1,NLEP),NHOR,1)
      else                  ! semi-langrange: save complete humidity array
         call mpputgp('dq'  ,dq,NHOR,NLEP)
      endif

      call surf_rest

      call restart_stop

      return
      end   


!     =================
!     SUBROUTINE EPILOG
!     =================

      subroutine epilog
      use pumamod
      implicit none

      close(unit=9191)

!
!     deallocate additional diagnostic arrays, if switched on
!

      if(ndiaggp2d > 0) deallocate(dgp2d)
      if(ndiagsp2d > 0) deallocate(dsp2d)
      if(ndiaggp3d > 0) deallocate(dgp3d)
      if(ndiagsp3d > 0) deallocate(dsp3d)
      if(ndiagcf   > 0) deallocate(dclforc)
      if(ndiagcf   > 0) deallocate(dclforc)
      if(nentropy  > 0) deallocate(dentropy,dentrop,dentrot,dentroq)
      if(nenergy   > 0) deallocate(denergy)

!
!     close output file
!
      if (mypid == NROOT) close(40)

!
!     close efficiency diagnostic file
!

      if(ndheat > 1 .and. mypid == NROOT) close(9)

!
!     write restart file
!

      if (mypid == NROOT) then
         call restart_prepare(trim(outdir_name)//'/plasim_status')
         call put_restart_integer('nstep'   ,nstep   )
         call put_restart_integer('naccuout',naccuout)
         call put_restart_integer('nlat'    ,NLAT    )
         call put_restart_integer('nlon'    ,NLON    )
         call put_restart_integer('nlev'    ,NLEV    )
         call put_restart_integer('nrsp'    ,NRSP    )

         call put_restart_array('sz',sz,NRSP,NESP,NLEV)
         call put_restart_array('sd',sd,NRSP,NESP,NLEV)
         call put_restart_array('st',st,NRSP,NESP,NLEV)
         if (nqspec == 1) call put_restart_array('sq',sq,NRSP,NESP,NLEV)
         call put_restart_array('sr',sr,NRSP,NESP,NLEV)
         call put_restart_array('sp',sp,NRSP,NESP,   1)
         call put_restart_array('so',so,NRSP,NESP,   1)
      endif

      call mpputsp('szm',szm,NSPP,NLEV)
      call mpputsp('sdm',sdm,NSPP,NLEV)
      call mpputsp('stm',stm,NSPP,NLEV)
      if (nqspec == 1) call mpputsp('sqm',sqm,NSPP,NLEV)
      call mpputsp('spm',spm,NSPP,   1)
!
!     gridpoint restart
!
      call mpputgp('dls'    ,dls    ,NHOR,1)
      call mpputgp('drhs'   ,drhs   ,NHOR,1)
      call mpputgp('dalb'   ,dalb   ,NHOR,1)
      call mpputgp('dz0'    ,dz0    ,NHOR,1)
      call mpputgp('dicec'  ,dicec  ,NHOR,1)
      call mpputgp('diced'  ,diced  ,NHOR,1)
      call mpputgp('dwatc'  ,dwatc  ,NHOR,1)
      call mpputgp('drunoff',drunoff,NHOR,1)
      call mpputgp('dveg'   ,dveg   ,NHOR,1)
      call mpputgp('dforest',dforest,NHOR,1)
      call mpputgp('dust3'  ,dust3  ,NHOR,1)
      call mpputgp('dcc'    ,dcc    ,NHOR,NLEV)
      call mpputgp('dql'    ,dql    ,NHOR,NLEV)
      call mpputgp('dqsat'  ,dqsat  ,NHOR,NLEV)
      call mpputgp('dt'     ,dt(1,NLEP),NHOR,1)
      if (nqspec == 1) then ! spectral: save only soil humidity
         call mpputgp('dq'  ,dq(1,NLEP),NHOR,1)
      else                  ! semi-langrange: save complete humidity array
         call mpputgp('dq'  ,dq,NHOR,NLEP)
      endif

!
!*    finish miscellaneous additional parameterizations
!

      call miscstop

!
!*    finish surface fluxes and vertical diffusion
!

      call fluxstop

!
!*    finish radiation
!

      call radstop

!
!*    finish large scale and convective rain and clouds
!

      call rainstop

!
!*    finish surface parameterizations or models
!

      call surfstop

      if (mypid == NROOT) then
         call restart_stop
      endif

      return
      end


!     =============================
!     SUBROUTINE READ_ATMOS_RESTART
!     =============================

      subroutine read_atmos_restart
      use pumamod
      implicit none

!     read scalars and full spectral arrays

      if (mypid == NROOT) then
         call get_restart_integer('nstep'   ,nstep)
         call get_restart_integer('naccuout',naccuout)
         call get_restart_array('sz',sz,NRSP,NESP,NLEV)
         call get_restart_array('sd',sd,NRSP,NESP,NLEV)
         call get_restart_array('st',st,NRSP,NESP,NLEV)
         if (nqspec == 1) call get_restart_array('sq',sq,NRSP,NESP,NLEV)
         call get_restart_array('sr',sr,NRSP,NESP,NLEV)
         call get_restart_array('sp',sp,NRSP,NESP,   1)
         call get_restart_array('so',so,NRSP,NESP,   1)
      endif

      call mpbci(nstep)     ! broadcast current timestep
      call mpbci(naccuout)  ! broadcast accumulation timestep for diagnostics

!     read and scatter spectral arrays

      call mpgetsp('szm',szm,NSPP,NLEV)
      call mpgetsp('sdm',sdm,NSPP,NLEV)
      call mpgetsp('stm',stm,NSPP,NLEV)
      if (nqspec == 1) call mpgetsp('sqm',sqm,NSPP,NLEV)
      call mpgetsp('spm',spm,NSPP,   1)

!     read and scatter surface grids

      call mpgetgp('dls'    ,dls    ,NHOR,   1)
      call mpgetgp('drhs'   ,drhs   ,NHOR,   1)
      call mpgetgp('dalb'   ,dalb   ,NHOR,   1)
      call mpgetgp('dz0'    ,dz0    ,NHOR,   1)
      call mpgetgp('dicec'  ,dicec  ,NHOR,   1)
      call mpgetgp('diced'  ,diced  ,NHOR,   1)
      call mpgetgp('dwatc'  ,dwatc  ,NHOR,   1)
      call mpgetgp('drunoff',drunoff,NHOR,   1)
      call mpgetgp('dveg'   ,dveg   ,NHOR,   1)
      call mpgetgp('dforest',dforest,NHOR,   1)
      call mpgetgp('dust3'  ,dust3  ,NHOR,   1)
      call mpgetgp('dcc'    ,dcc    ,NHOR,NLEV)
      call mpgetgp('dql'    ,dql    ,NHOR,NLEV)
      call mpgetgp('dqsat'  ,dqsat  ,NHOR,NLEV)
      call mpgetgp('dt'     ,dt(1,NLEP),NHOR,1)
      if (nqspec == 1) then ! spectral: read only soil humidity
         call mpgetgp('dq',dq(1,NLEP),NHOR,1)
      else                  ! semi-langrange: read complete humidity array
         call mpgetgp('dq',dq,NHOR,NLEP)
      endif

      return
      end subroutine read_atmos_restart


!     ==========================
!     SUBROUTINE STABILITY_CHECK
!     ==========================

      subroutine stability_check
      use pumamod
      implicit none

!     Some operating systems ignore floating point exceptions
!     and continue to run the program after an explosion,
!     e.g. after wrong settings for timestep or other conditions.
!     This subroutine checks some variables for valid ranges
!     and aborts the program if it's obviously broken.

      if (mypid == NROOT) then
         if (gd(1,1) > 1000.0 .or. gd(1,1) < -1000.0 .or. &
             gz(1,1) > 1000.0 .or. gz(1,1) < -1000.0 .or. &
             gt(1,1) > 1000.0 .or. gt(1,1) < -1000.0) then
            open(44,file='Abort_Message')
            write(44,*) 'Planet Simulator aborted'
            write(44,*) 'gd(1,1) = ',gd(1,1)
            write(44,*) 'gz(1,1) = ',gz(1,1)
            write(44,*) 'gt(1,1) = ',gt(1,1)
            close(44)
   
            write(*,*) 'Planet Simulator aborted'
            write(*,*) 'gd(1,1) = ',gd(1,1)
            write(*,*) 'gz(1,1) = ',gz(1,1)
            write(*,*) 'gt(1,1) = ',gt(1,1)
   
            stop
         endif
      endif
      end subroutine stability_check
            

!     =================
!     SUBROUTINE INITFD
!     =================

      subroutine initfd
      use pumamod
      implicit none

      if (nkits < 1) nkits = 1
!     ============================================================
!     next subroutine call to set inital temperature.
!     model started from rest with stratification given by
!     trs calculated in setzt(ex). a perturbation is added to
!     constant log(sp) by subroutine noise, called from setzt(ex).
!     ============================================================
!
      if (mypid == NROOT) then
       call setzt_plasim
      endif
      call mpscsp(sp,spm,1)
      if (mypid == NROOT) then
          st(1,:) = sr(1,:)
         stm(1,:) = sr(1,:)
          sz(3,:) = plavor
         szm(3,:) = plavor
      endif
      return
      end

!     =================
!     SUBROUTINE READNL
!     =================

      subroutine readnl
      use pumamod
      implicit none

      namelist /inp/ ncoeff  , ndel                                     &
                   , nexp    , nexper  , nkits   , nrestart, noutput    &
                   , nstep   , ndebug  , ntspd   , neqsig  , nqspec     &
                   , nprint  , nprhor  , npacksp , npackgp              &
                   , sellon                                             &
                   , nrad    , nflux, nadv, nhordif                     &
                   , ntime   , nperpetual                               &
                   , n_start_year , n_start_month                       &
                   , nsela                                              &
                   , dtep    , dtns    , dtrop   , dttrp                &
                   , tgr                                                &
                   , psurf   , ndl     , nhdiff  , ndheat               &
                   , restim  , t0      , tfrc    , ndiaggp , ndiagsp    &
                   , ndiaggp2d, ndiaggp3d, ndiagsp2d, ndiagsp3d, ndiagcf&
                   , sigh     , nentropy , nenergy  , nsponge  , dampsp


      namelist /planet/ akap, alr, ga, gascon, plarad, pnu, ww          &
                      , solar_day, siderial_day, rotspd                 &
                      , ra1, ra2, ra4, yplanet                          

      namelist /ini_plasim_nml/                                       &
                 n_days_per_year, n_days_per_month                    &
               , nafter, ndiag                                        &
               , ngenie                                               &
               , kick                                                 &
               , nhdiff, tdissd, tdissz, tdisst, tdissq               &
               , mars                                                 &
               , runtime_root
      
      namelist /planet_plasim_nml/                                    &
                 nfixorb, eccen, obliq, mvelp

      namelist /genie_coupling_nml/                                   &
                 indir_name,outdir_name,rstdir_name,                  &
                 scale_apm,                                           &
                 ngear,ngear_years_plasim,ngear_multiple,             &
                 atchem_couple

      namelist /plasim_output_switches/                              &
                nout3D,noutseas,noutmnth

!
!     preset namelist parameter according to model set up
!
      if (NLEV==10) then
         tfrc(1)      =  20.0 * solar_day
         tfrc(2)      = 100.0 * solar_day
         tfrc(3:NLEV) =   0.0 * solar_day
      endif
!
      if(NTRU==42) then
       nhdiff=16
       ndel(:)=4
       tdissq(:)=0.1  * solar_day
       tdisst(:)=0.76 * solar_day
       tdissz(:)=0.3  * solar_day
       tdissd(:)=0.06 * solar_day
      endif

      open(unit=56,file='data_PLASIM')
      read(unit=56,NML=ini_plasim_nml)
      write(*,ini_plasim_nml)
      read(unit=56,NML=planet_plasim_nml)
      write(*,planet_plasim_nml)
      read(unit=56,NML=genie_coupling_nml)
      write(*,genie_coupling_nml)
      read(unit=56,NML=plasim_output_switches)
      write(*,plasim_output_switches)
      close(56)

!
!     read namelist
!
      open(11,file=trim(runtime_root)//'/genie-plasim/config/puma_namelist',form='formatted')
      read (11,inp)

!     shortcut for Mars settings

      if (mars == 1) then
         yplanet      = "Mars" 
         n_days_per_month = 57   ! simple calendar
         n_days_per_year  = 12 * n_days_per_month
         solar_day    = SOL_DAY_MARS
         siderial_day = SID_DAY_MARS
         akap    = AKAP_MARS
         alr     = ALR_MARS
         ga      = GA_MARS
         gascon  = GASCON_MARS
         plarad  = PLARAD_MARS
         pnu     = PNU_MARS
!WW_MARS is not defined, but ww is overwritten below anyway...
!         ww      = WW_MARS
         ra1     = RA1_ICE
         ra2     = RA2_ICE
         ra4     = RA4_ICE
         dtrop     = 40000.0
         dttrp     =  20.0
         tgr       = 214.0
         psurf     = 820.0
         dveg(:)   =   0.0       ! No vegetation on Mars
         tdisst(:) =   2.0 * solar_day
!        restim(:) =  15.0 * solar_day      ! Instable
!        t0(:)     = 120.0       ! Explodes after 5 days
         tfrc(1)   =   1.0 * solar_day
         tfrc(2)   =   5.0 * solar_day
         tfrc(3:)  =  10.0 * solar_day
      endif

      read (11,planet)

!     set planet dependent variables

      if (rotspd /= 1.0) then
         solar_day = solar_day / rotspd
         n_days_per_year = n_days_per_year * rotspd
         n_days_per_month = n_days_per_year / 12
         siderial_day =(n_days_per_year*solar_day)/(n_days_per_year+1.0)
      endif
      ww    = TWOPI / siderial_day ! Omega (scaling)
      acpd  = gascon / akap        ! Specific heat for dry air
      adv   = ACPV / acpd -1.0     ! Often used
      cv    = plarad * ww          ! cv
      ct    = cv * cv / gascon     ! ct
      pnu21 = 1.0 - 2.0 * pnu      ! Time filter 2
      rdbrv = gascon / RV          ! rd / rv

!
!     calendar and time control

!     Make sure that (mpstep * 60) * ntspd = solar_day

      if (mpstep > 0) then             ! timestep given in [min]
         ntspd = nint(solar_day) / (mpstep * 60)
      endif
      mpstep = solar_day  / (ntspd * 60)
! PBH I don't want multiple writes per day, so namelist vaiable is now nafter (no timesteps betwenne writes)
!      if (nwpd > 0 .and. nwpd <= ntspd) then
!         nafter = ntspd / nwpd
!      endif
      if (ndiag < 1) ndiag = 10 * ntspd
!
!     for column runs set horizontal diffusion coefficients to 0
!
      if (nhordif==0) then
       restim = 0.0
       tfrc   = 0.0
       tdissd = 0.0
       tdissz = 0.0
       tdisst = 0.0
       tdissq = 0.0
      endif

      write (*,'(/," *************************************")')
      write (*,'(" * Namelist INP from <puma_namelist> *")')
      write (*,'(" *************************************")')
      write(*,inp)
      write (*,'(/," ****************************************")')
      write (*,'(" * Namelist PLANET from <puma_namelist> *")')
      write (*,'(" ****************************************")')
      write(*,planet)

!     Convert start date to timesteps since 1-Jan-0000

      if (n_days_per_year == 365) then
         call cal2step(n_start_step,ntspd,n_start_year,n_start_month,   &
                       1,0,0)
      else
         n_start_step = ntspd * (n_start_year     * n_days_per_year     &
                              + (n_start_month-1) * n_days_per_month)   
     endif

!     Print some values

      write (*,'(/," **********************************")')
      write (*,'(" * Solar    day     :",f8.1," [s] *")') solar_day
      write (*,'(" * Siderial day     :",f8.1," [s] *")') siderial_day
      write (*,'(" * Omega            :",f6.2," [s-6] *")') ww * 1.0e6
      write (*,'(" * Rotation Speed   :",f8.1,"     *")') rotspd
      write (*,'(" * Days / Year      :",i6,"       *")') n_days_per_year
      if (n_days_per_month > 0) then
      write (*,'(" * Days / Month     :",i6,"       *")') n_days_per_month
      else
      write (*,'(" * Days / Month     :    variable *")')
      endif
      write (*,'(" * Timestep         :",i6," [min] *")') mpstep
      write (*,'(" * Timesteps / day  :",i6,"       *")') ntspd
      write (*,'(" **********************************")')

!     set sponge layer time scale

      if(dampsp > 0.) then
       if(dampsp < (solar_day/ntspd)) dampsp=dampsp*solar_day
       dampsp=solar_day/(TWOPI*dampsp)
      endif

!     set franks diagnostics

      if(ndiaggp==1) then
       ndiaggp3d=21+ndiaggp3d
      end if
      if(ndiagsp==1) then
       ndiagsp3d=3+ndiagsp3d
      end if

      return
      end

      subroutine dayseccheck(pf,yn)
      use pumamod
      implicit none
      real zmax
      real :: pf(NLEV)
      character (len=*) :: yn

      zmax = maxval(pf(:))
      if (zmax < (solar_day / ntspd)) then
         write (*,*) 'old maxval(',trim(yn),') = ',zmax
         write (*,*) 'assuming [days] - converting to [sec]'
         pf(:) = pf(:) * solar_day
         write (*,*) 'new maxval(',trim(yn),') = ',maxval(pf(:))
      endif   
      return
      end 
         
!     =================
!     SUBROUTINE INITPM
!     =================

      subroutine initpm
      use pumamod
      implicit none
      integer jlev,jdel,jm,jr,jn,ji
      real zsk,zsq,zrsq2

      real (kind=8) radea,zakk

!     *************************************************************
!     * carries out all initialisation of model prior to running. *
!     * major sections identified with comments.                  *
!     * this s/r sets the model parameters and all resolution     *
!     * dependent quantities.                                     *
!     *************************************************************

      radea = plarad

!     *********************
!     * set vertical grid *
!     *********************

      if(neqsig==-1) then
       sigmah(:)=sigh(:)
      elseif(neqsig==1) then
       do jlev = 1 , NLEV
        sigmah(jlev) = real(jlev) / NLEV
       enddo
      else
       do jlev=1,NLEV
        zsk=REAL(jlev)/REAL(NLEV)
        sigmah(jlev)=0.75*zsk+1.75*zsk**3-1.5*zsk**4
       enddo
      end if

      dsigma(1     ) = sigmah(1)
      dsigma(2:NLEV) = sigmah(2:NLEV) - sigmah(1:NLEV-1)

      rdsig = 0.5 / dsigma

      sigma(1     ) = 0.5 * sigmah(1)
      sigma(2:NLEV) = 0.5 * (sigmah(1:NLEV-1) + sigmah(2:NLEV))

!     dimensionless coefficient for newtonian cooling
!     friction and timestep. of course a day is 2*PI in non dimensional
!     units using omega as the unit of frquency.
!     
!     dayseccheck assumes units [days] if values < timestep
!     and converts values to [sec] (compatibilty routine)

      call dayseccheck(restim,"restim")
      call dayseccheck(tfrc  ,"tfrc"  )
      call dayseccheck(tdissd,"tdissd")
      call dayseccheck(tdissz,"tdissz")
      call dayseccheck(tdisst,"tdisst")
      call dayseccheck(tdissq,"tdissq")

      where (restim > 0.0)
         damp = solar_day / (TWOPI * restim)
      elsewhere
         damp = 0.0
      endwhere
         
      where (tfrc > 0.0)
          tfrc = solar_day / (TWOPI * tfrc)
      elsewhere
          tfrc = 0.0
      endwhere

!     compute internal diffusion parameter (LAUERSON)

      do jlev=1,NLEV
       jdel = ndel(jlev)
       if (tdissd(jlev) > 0.0) then
        tdissd(jlev) = solar_day/(TWOPI*tdissd(jlev))
       else
        tdissd(jlev)=0.
       endif
       if (tdissz(jlev) > 0.0) then
        tdissz(jlev) = solar_day/(TWOPI*tdissz(jlev))
       else
        tdissz(jlev)=0.
       endif
       if (tdisst(jlev) > 0.0) then
        tdisst(jlev) = solar_day/(TWOPI*tdisst(jlev))
       else
        tdisst(jlev) = 0.
       endif
       if (tdissq(jlev) > 0.0) then
        tdissq(jlev) = solar_day/(TWOPI*tdissq(jlev))
       else
        tdissq(jlev)=0.
       endif
       zakk=1./((NTRU-nhdiff)**jdel)
       jr=-1
       do jm=0,NTRU
         do jn=jm,NTRU
            jr=jr+2
            ji=jr+1
            zsq = (jn - nhdiff)
            if(jn >= nhdiff) then
             sak(jr,jlev) = zakk*zsq**jdel
            else
             sak(jr,jlev) = 0.
            endif
            sak(ji,jlev) = sak(jr,jlev)
         enddo
       enddo
      enddo

!     set coefficients which depend on wavenumber

      zrsq2 = 1.0 / sqrt(2.0)

      jr=-1
      do jm=0,NTRU
         do jn=jm,NTRU
            jr=jr+2
            ji=jr+1
            nindex(jr)=jn
            nindex(ji)=jn
            spnorm(jr)=zrsq2
            spnorm(ji)=zrsq2
         enddo
         zrsq2=-zrsq2
      enddo

! finally make temperatures dimensionless

      dtns  = dtns  / ct
      dtep  = dtep  / ct
      dttrp = dttrp / ct
      t0    = t0    / ct ! (NLEV)

!     print out

      zakk=tdisst(NLEV)/((NTRU-nhdiff)**ndel(NLEV))
      write(*,'(/," *************************************************")')
      if (zakk == 0.0) then
      write (*,'(" * No lateral dissipation *")')
      else
      write(*,'(" * Lateral dissipation",13x,"NDEL(",i3,") =",i2," *")')&
         NLEV,ndel(NLEV)
      write(*,'(" * Diffusion coefficient = ",g14.4," [m**",i1,"] *")')&
         zakk*ww*radea**ndel(NLEV),ndel(NLEV)
      write(*,'(" * e-folding time for smallest scale =",f5.1," days *")')&
         1.0/(TWOPI*tdisst(NLEV))
      endif
      write(*,'(" *************************************************")')
      return
      end


!     =================
!     SUBROUTINE MAKEBM
!     =================

      subroutine makebm
      use pumamod
      implicit none
      integer jlev,jlev1,jlev2,jn
      real zdeltsq,zaq

      zdeltsq = delt * delt

      do jlev1 = 1 , NLEV
         do jlev2 = 1 , NLEV
            zaq = zdeltsq * (t0(jlev1) * dsigma(jlev2)                  &
     &          + dot_product(g(:,jlev1),tau(jlev2,:)))
            bm1(jlev2,jlev1,:) = zaq
         enddo
      enddo

      do jn=1,NTRU
         do jlev = 1 , NLEV
            bm1(jlev,jlev,jn) = bm1(jlev,jlev,jn) + 1.0 / (jn*(jn+1))
         enddo
         call minvers(bm1(1,1,jn),NLEV)
      enddo
      return
      end

!     =================
!     SUBROUTINE INITSI
!     =================

      subroutine initsi
      use pumamod
      implicit none
      integer jlev,jlev2,i
      real zalp,zt01s2,zsig,zsigm,ztm,ztmm,ztau,zfctr,zh,zttm
!===========================================================
! carries out all initialisation of model prior to running.
! major sections identified with comments.
! this s/r sets the variables and arrays associated with
! the semi-implicit scheme.
!===========================================================
!
      dimension zalp(NLEV),zh(NLEV)
!
! this value, used in setting alpha(1), is irrelevant in the
! angular momentum conserving ecmwf scheme

      tkp = akap * t0
      t01s2(1:NLEM) = t0(2:NLEV) - t0(1:NLEM)
      t01s2(  NLEV) = 0.0

      zalp(2:NLEV) = log(sigmah(2:NLEV)) - log(sigmah(1:NLEM))

      g      = 0.0
      g(1,1) = 1.0
      do jlev = 2 , NLEV
         g(jlev,jlev) = 1.0 - zalp(jlev)*sigmah(jlev-1)/dsigma(jlev)
         g(jlev,1:jlev-1) = zalp(jlev)
      enddo

      do jlev = 1 , NLEV
         c(jlev,:) = g(:,jlev) * (dsigma(jlev) / dsigma(:))
      enddo

      zt01s2   = t01s2(1)
      zsig     = sigmah(1)
      tau(1,1) = 0.5 * zt01s2 * (zsig - 1.0) + tkp(1)
      tau(2:NLEV,1) = 0.5 * zt01s2 * dsigma(2:NLEV)

      do 1410 jlev=2,NLEV
        zttm=zt01s2
        zsigm=zsig
        zt01s2=t01s2(jlev)
        zsig=sigmah(jlev)
        do 1420 jlev2=1,NLEV
          ztm=0.
          ztmm=0.
          if(jlev2.le.jlev) ztm=1
          if(jlev2.lt.jlev) ztmm=1
          ztau=zttm*(zsigm-ztmm)
          if(jlev.lt.NLEV) ztau=ztau+zt01s2*(zsig-ztm)
          ztau=ztau*rdsig(jlev)*dsigma(jlev2)
          if(jlev2.le.jlev) ztau=ztau+tkp(jlev)*c(jlev2,jlev)
          tau(jlev2,jlev)=ztau
 1420   continue
 1410 continue
!
      zfctr=0.001*CV*CV/ga
      do 1500 jlev = 1 , NLEV
         zh(jlev) = dot_product(g(:,jlev),t0) * zfctr
 1500 continue
!
!     **********************************
!     * write out vertical information *
!     **********************************
!
      write(*,9001)
      write(*,9003)
      write(*,9002)
      do jlev = 1 , NLEV
        write(*,9004) jlev,sigma(jlev),t0(jlev),zh(jlev)
      enddo
      write(*,9000)

      write(*,9012)
      write(*,9013) (jlev,jlev = 1 , 5)
      write(*,9012)
      do jlev = 1 , NLEV
        write(*,9014) jlev,(c(i,jlev),i=1,5)
      enddo
      write(*,9012)
      return
 9000 format(1x,33('*'),/)
 9001 format(/,1x,33('*'))
 9002 format(1x,33('*'))
 9003 format(' * Lv *    Sigma Basic-T  Height *')
 9004 format(' *',i3,' * ',3f8.3,' *')
 9012 format(1x,69('*'))
 9013 format(' * Lv * C',i11,4i12,' *')
 9014 format(' *',i3,' * ',5f12.8,' *')
      end

!     ==================
!     SUBROUTINE MINVERS
!     ==================

      subroutine minvers(a,n)
      implicit none
      integer n,j,indx
      real a,b
      dimension a(n,n),b(n,n),indx(n)

      b = 0.0
      do j = 1 , n
         b(j,j) = 1.0
      enddo
      call ludcmp(a,n,indx)
      do j = 1 , n
         call lubksb(a,n,indx,b(1,j))
      enddo
      a = b
      return
      end

!     =================
!     SUBROUTINE LUBKSB
!     =================

      subroutine lubksb(a,n,indx,b)
      implicit none
      integer n,indx,k,i,j,l
      real a,b,sum
      dimension a(n,n),b(n),indx(n)
      k = 0
      do i = 1 , n
         l    = indx(i)
         sum  = b(l)
         b(l) = b(i)
         if (k > 0) then
            do j = k , i-1
               sum = sum - a(i,j) * b(j)
            enddo
         else if (sum /= 0.0) then
            k = i
         endif
         b(i) = sum
      enddo

      do i = n , 1 , -1
         sum = b(i)
         do j = i+1 , n
            sum = sum - a(i,j) * b(j)
         enddo
         b(i) = sum / a(i,i)
      enddo
      return
      end

!     =================
!     SUBROUTINE LUDCMP
!     =================

      subroutine ludcmp(a,n,indx)
      implicit none
      integer n,indx,i,j,imax,k
      real a,d,vv,aamax,dum
      dimension a(n,n),indx(n),vv(n)

      d = 1.0
      vv = 1.0 / maxval(abs(a),2)

      do 19 j = 1 , n
         do i = 2 , j-1
            a(i,j) = a(i,j) - dot_product(a(i,1:i-1),a(1:i-1,j))
         enddo
         aamax = 0.0
         do i = j , n
            if (j > 1)                                                  &
     &      a(i,j) = a(i,j) - dot_product(a(i,1:j-1),a(1:j-1,j))
            dum = vv(i) * abs(a(i,j))
            if (dum .ge. aamax) then
               imax = i
               aamax = dum
            endif
         enddo
         if (j .ne. imax) then
            do 17 k = 1 , n
               dum = a(imax,k)
               a(imax,k) = a(j,k)
               a(j,k) = dum
   17       continue
            d = -d
            vv(imax) = vv(j)
         endif
         indx(j) = imax
         if (a(j,j) == 0.0) a(j,j) = tiny(a(j,j))
         if (j < n) a(j+1:n,j) = a(j+1:n,j) / a(j,j)
   19 continue
      return
      end

!     =================
!     SUBROUTINE INILAT
!     =================

      subroutine inilat
      use pumamod
      implicit none
      integer jlat,ideg
      character(len=1) ch

      ch = 'N'
      do jlat = 1 , NLAT
         csq(jlat)  = 1.0 - sid(jlat) * sid(jlat)
         rcs(jlat)  = 1.0 / sqrt(csq(jlat))
      enddo
      do jlat = 1 , NLAT/2
         ideg = nint(180.0/PI * asin(sid(jlat)))
         write(chlat(jlat),'(i2,a1)') ideg,'N'
         write(chlat(NLAT+1-jlat),'(i2,a1)') ideg,'S'
      enddo
      return
      end

!     ================
!     SUBROUTINE NOISE
!     ================

      subroutine noise_plasim
      use pumamod
      implicit none
      integer itp1,jsp1,jsp,jr,jm,jn,ji
      real zeps,zscale,zrand

!     if kick is set to 1 or 2
!     adds white noise perturbation to ln(surface pressure)
!     balanced initial state at t=0.
!     for kick=2, the white noise pertubation is
!     symmetric to the equator
!     eps sets magnitude of the noise

      itp1 = NTP1 ! Suppress compiler warnings for T1
      if (itp1 <= 2 .and. kick > 2) kick = 0 ! for T1
      call random_seed()
      zeps=1.e-4
      zscale=zeps/sqrt(2.0)

      write(*,'(/," *****************************************")')
      if (kick == 1) then
         jsp1=2*NTP1+1
         do jsp=jsp1,NRSP
            call random_number(zrand)
            sp(jsp)=sp(jsp)+zscale*(zrand-0.5)
         enddo
         write(*,'(" *     White noise added (KICK = 1)      *")')
      elseif (kick == 2) then
         jr=2*NTP1-1
         do jm=1,NTRU
            do jn=jm,NTRU
               jr=jr+2
               ji=jr+1
               if (mod(jn+jm,2) == 0) then
                  call random_number(zrand)
                  sp(jr)=sp(jr)+zscale*(zrand-0.5)
                  sp(ji)=sp(ji)+zscale*(zrand-0.5)
               endif
            enddo
         enddo
         write(*,'(" * Symmetric white noise added (KICK=2) *")')
      elseif (kick == 3) then
         sp(2*itp1+3) = zscale
         sp(2*itp1+4) = zscale * 0.5
         write(*,'(" *  Mode sp(1,1) disturbed (KICK = 3)    *")')
      endif
      write(*,'(" *****************************************")')
      return
      end

!     ================
!     SUBROUTINE SETZT
!     ================
      subroutine setzt_plasim
      use pumamod
      implicit none
      integer jlev
      real zdttrp,zsigprev,ztprev,zzprev,zzp,ztp,ztpm,ztpp,zttrop,ztrs
      real ztps,zsqrt2,zsqrt04,zsqrt6,zzpp,zfac
!
      dimension ztrs(NLEV)
      dimension zfac(NLEV)
!
!*********************************************************************
!  this s/r sets up restoration temp field.
! the temperature at sigma = 1 is tgr, entered in kelvin.
! a lapse rate of ALR k/m is assumed under the tropopause and zero
! above. the actual profile tends to this away from then
! tropopause, with smooth interpolation depending on dttrp
! at the model tropopause.the height of
! the tropopause is given as dtrop m.
!*********************************************************************
!
      sr(:,:) = 0.0 ! NESP,NLEV
!
      zdttrp=dttrp*ct
!
      zsigprev=1.
      ztprev=tgr
      zzprev=0.
      do 1100 jlev=NLEV,1,-1
        zzp=zzprev+(gascon*ztprev/ga)*log(zsigprev/sigma(jlev))
        ztp=tgr-dtrop*ALR
        ztp=ztp+sqrt((.5*ALR*(zzp-dtrop))**2+zdttrp**2)
        ztp=ztp-.5*ALR*(zzp-dtrop)
        ztpm=.5*(ztprev+ztp)
        zzpp=zzprev+(gascon*ztpm/ga)*log(zsigprev/sigma(jlev))
        ztpp=tgr-dtrop*ALR
        ztpp=ztpp+sqrt((.5*ALR*(zzpp-dtrop))**2+zdttrp**2)
        ztpp=ztpp-.5*ALR*(zzpp-dtrop)
        ztrs(jlev)=ztpp
        zzprev=zzprev                                                   &
     &        +(.5*(ztpp+ztprev)*gascon/ga)*log(zsigprev/sigma(jlev))
        ztprev=ztpp
        zsigprev=sigma(jlev)
1100  continue
!
!     **********************************
!     * write out vertical information *
!     **********************************
!
      write(*,9001)
      write(*,9003)
      write(*,9002)
!
      do 1200 jlev = 1 , NLEV
         write(*,9004) jlev,sigma(jlev),ztrs(jlev)
         ztrs(jlev)=ztrs(jlev)/ct
 1200 continue
!
      write(*,9002)
!
!******************************************************************
! loop to set array zfac - this controls temperature gradients as a
! function of sigma in tres. it is a sine wave from one at
! sigma = 1 to zero at stps (sigma at the tropopause) .
!******************************************************************
! first find sigma at dtrop
!
      zttrop=tgr-dtrop*ALR
      ztps=(zttrop/tgr)**(ga/(ALR*gascon))
!
! now the latitudinal variation in tres is set up ( this being in terms
! of a deviation from t0 which is usually constant with height)
!
      zsqrt2=sqrt(2.)
      zsqrt04=sqrt(0.4)
      zsqrt6=sqrt(6.)
      do 2100 jlev = 1 , NLEV
        zfac(jlev)=sin(0.5*PI*(sigma(jlev)-ztps)/(1.-ztps))
        if (zfac(jlev).lt.0.0) zfac(jlev)=0.0
        sr(1,jlev)=zsqrt2*(ztrs(jlev)-t0(jlev))
        sr(3,jlev)=(1./zsqrt6)*dtns*zfac(jlev)
        sr(5,jlev)=-2./3.*zsqrt04*dtep*zfac(jlev)
 2100 continue
!
      call noise_plasim
!
      return
 9001 format(/,1x,26('*'))
 9002 format(1x,26('*'))
 9003 format(' * Lv *    Sigma Restor-T *')
 9004 format(' *',i3,' * ',f8.3,f9.3,' *')
      end


!     ===============
!     SUBROUTINE DIAG
!     ===============

      subroutine diag_plasim
      use pumamod
      implicit none
      if (mod(nstep,ndiag) == 0) then
         if (ncoeff .gt. 0) call prisp
         call xsect
         print*,"CO2 effective ",co2
         print*,"CO2 actual ",co2veg
      endif
      call energy_plasim
      return
      end

!     ================
!     SUBROUTINE PRISP
!     ================

      subroutine prisp
      use pumamod
      implicit none
      integer jlev
      real scale

      character(len=30) title

      scale = 100.0
      title = 'Vorticity [10-2]'
      do 100 jlev = 1 , NLEV
         if (ndl(jlev).ne.0) call wrspam(sz(1,jlev),jlev,title,scale)
  100 continue

      title = 'Divergence [10-2]'
      do 200 jlev = 1 , NLEV
         if (ndl(jlev).ne.0) call wrspam(sd(1,jlev),jlev,title,scale)
  200 continue

      scale = 1000.0
      title = 'Temperature [10-3]'
      do 300 jlev = 1 , NLEV
         if (ndl(jlev).ne.0) call wrspam(st(1,jlev),jlev,title,scale)
  300 continue

      if (nqspec == 1) then
         scale = 1000.0
         title = 'Specific Humidity [10-3]'
         do jlev = 1 , NLEV
            if (ndl(jlev).ne.0) call wrspam(sq(1,jlev),jlev,title,scale)
         enddo
      endif

      title = 'Pressure [10-3]'
      call wrspam(sp,0,title,scale)

      return
      end

!     ==============
!     FUNCTION RMSSP
!     ==============

      function rmssp(pf)
      use pumamod
      implicit none
      integer jlev
      real rmssp
      real zsum
      real pf(NESP,NLEV)

      zsum = 0.0
      do jlev = 1 , NLEV
         zsum = zsum + dsigma(jlev)                                     &
     &        * (dot_product(pf(1:NZOM,jlev),pf(1:NZOM,jlev)) * 0.5     &
     &        +  dot_product(pf(NZOM+1:NRSP,jlev),pf(NZOM+1:NRSP,jlev)))
      enddo
      rmssp = zsum
      return
      end

!     =================
!     SUBROUTINE ENERGY
!     =================

      subroutine energy_plasim
      use pumamod
      implicit none
      integer idim

      parameter (idim=6) ! Number of scalars for GUI timeseries
      real ziso(idim)

      ziso(1) = umax   ! maximum value of csu (zonal mean cross section)
      ziso(2) = t2mean - TMELT  ! mean of 2m temperature
      ziso(3) = precip * 1.0e9  ! mean precipitation
      ziso(4) = evap   * 1.0e9  ! mean evaporation
      ziso(5) = olr             ! OLR
!     ziso(6) = minval(dt(:,NLEP)) ! Minimum of surface temperature
      ziso(6) = 1000.0 * sum(dq(:,NLEV))/2048.

      return
      end

!     ==================
!     SUBROUTINE UPDATIM
!     ==================

      subroutine updatim(kstep)
      use pumamod
      implicit none
      integer kstep
       
      interface
         integer function nweekday(kday)
            integer kday
         end function nweekday
      end interface

      if (n_days_per_year == 365) then
         call step2cal(kstep,ntspd, &
            ndatim(1),ndatim(2),ndatim(3),ndatim(4),ndatim(5))
         ndatim(6) = nweekday(kstep/ntspd)
      else
         call step2cal30(kstep,ntspd, &
            ndatim(1),ndatim(2),ndatim(3),ndatim(4),ndatim(5))
      endif
      return
      end

!     =================
!     SUBROUTINE WRSPAM
!     =================

      subroutine wrspam(ps,klev,title,scale)
      use pumamod
      implicit none
      integer klev,i
      real ps,scale
!
      dimension ps(NRSP)
      character(len=30) title
      character(len=18) datch


      call ntodat(nstep,datch)
      write(*,'(1x)')
      write(*,20000)
      write(*,20030) datch,title,klev
      write(*,20000)
      write(*,20020) (i,i=0,9)
      write(*,20000)
      write(*,20100) (cab(i),i=1,10)
      write(*,20200) (cab(i),i=NTRU+2,NTRU+10)
      write(*,20300) (cab(i),i=2*NTRU+2,2*NTRU+9)
      write(*,20400) (cab(i),i=3*NTRU+1,3*NTRU+7)
      write(*,20000)
      write(*,'(1x)')

      return

20000 format(1x,78('*'))
20020 format(' * n * ',10i7,' *')
20030 format(' *   * ',a18,2x,a30,'  Level ',i2,11x,'*')
20100 format(' * 0 *',f8.2,9f7.2,' *')
20200 format(' * 1 *',8x,9f7.2,' *')
20300 format(' * 2 *',15x,8f7.2,' *')
20400 format(' * 3 *',22x,7f7.2,' *')
      contains
      function cab(i)
       implicit none
       integer i
       real cab
       cab=real(scale*sqrt(ps(i+i-1)*ps(i+i-1)+ps(i+i)*ps(i+i)))
      end function cab
      end

!     ===============
!     SUBROUTINE WRZS
!     ===============

      subroutine wrzs(zs,title,scale)
      use pumamod
      implicit none
      integer ip,ia,ib,ic,id,i,j,jlev
      real zs,scale
!
      dimension zs(NLAT,NLEV)
      character(len=30) title
      character(len=18) datch

      ip = NLAT / 16
      ia = ip/2
      ib = ia + 7 * ip
      id = NLAT + 1 - ia
      ic = id - 7 * ip

      call ntodat(nstep,datch)
      write(*,'(1x)')
      write(*,20000)
      write(*,20030) datch,title
      write(*,20000)
      write(*,20020) (chlat(i),i=ia,ib,ip),(chlat(j),j=ic,id,ip)
      write(*,20000)
      do 200 jlev = 1 , NLEV
         write(*,20100) jlev,((int(zs(i,jlev)*scale)),i=ia,ib,ip),      &
     &                       ((int(zs(j,jlev)*scale)),j=ic,id,ip),jlev
  200 continue
      write(*,20000)
      write(*,'(1x)')

20000 format(1x,78('*'))
20020 format(' * Lv * ',16(1x,a3),' * Lv *')
20030 format(' *    * ',a18,2x,a30,20x,'*')
20100 format(' * ',i2,' * ',16i4,' * ',i2,' *')
      end

!     ================
!     SUBROUTINE XSECT
!     ================

      subroutine xsect
      use pumamod
      implicit none
      real scale
      character(len=30) title

      scale = 10.0
      title = 'Zonal Wind [0.1 m/s]'
      call wrzs(csu,title,scale)
      title = 'Meridional Wind [0.1 m/s]'
      call wrzs(csv,title,scale)
      scale = 1.0
      title = 'Temperature [C]'
      call wrzs(cst,title,scale)
      scale = 10000.0
      title = 'specific humidity [0.1g/Kg]'
      call wrzs(csm,title,scale)
      scale = 100.0
      title = 'cloud cover [%]'
      call wrzs(ccc,title,scale)
      return
      end


!     =====================
!     SUBROUTINE GRIDPOINTA
!     =====================

      subroutine gridpointa
      use pumamod
      implicit none
      integer jlev,jlat,jlon,j1,j2
      real sec,zp00
!
!*    Adiabatic Gridpoint Calculations
!
      real gtn(NHOR,NLEV)
      real gut(NHOR,NLEV)
      real gvt(NHOR,NLEV)
      real gqn(NHOR,NLEV)
      real guq(NHOR,NLEV)
      real gvq(NHOR,NLEV)
      real guz(NHOR,NLEV)
      real gvz(NHOR,NLEV)
      real gke(NHOR,NLEV)
      real gphi(NHOR,NLEV)
      real gvpp(NHOR)
      real gpmt(NLON,NLPP)
      real sdf(NESP,NLEV)
      real stf(NESP,NLEV)
      real sqf(NESP,NLEV)
      real szf(NESP,NLEV)
      real spf(NESP)

      real zgp(NLON,NLAT)

!
!     inverse Legendre transformation (spectral to fourier domain)
!     st -> gt  sq -> gq  sd -> gd  sz -> gz
!     (sd,sz) -> (gu,gv)
!     sp -> gp  sp -> gpj (dlnps/dphi)
!

      call invlega

      if (mod(nstep,ndiag) == 0) then
        do jlev = 1 , NLEV
          do jlat = 1 , NLPP
            sec = CV / sqrt(csq(jlat))
            csu(jlat,jlev) =  gu(1+(jlat-1)*NLON,jlev) * sec
            csv(jlat,jlev) =  gv(1+(jlat-1)*NLON,jlev) * sec
            cst(jlat,jlev) =(gt(1+(jlat-1)*NLON,jlev) + t0(jlev))*ct-TMELT
            j1=(jlat-1)*NLON+1
            j2=jlat*NLON
            ccc(jlat,jlev) = SUM(dcc(j1:j2,jlev))/real(NLON)
            if (nqspec == 1) then
               csm(jlat,jlev) = (gq(1+(jlat-1)*NLON,jlev))
            else
               csm(jlat,jlev) = sum(dq(j1:j2,jlev))/real(NLON)
            endif
          enddo
        enddo
        umax = maxval(csu)
      endif

      do jlat = 1 , NLPP
         do jlon = 1 , NLON , 2
           gpmt(jlon  ,jlat) = -gp(jlon+1+(jlat-1)*NLON) * ((jlon-1)/2)
           gpmt(jlon+1,jlat) =  gp(jlon  +(jlat-1)*NLON) * ((jlon-1)/2)
         enddo
      enddo

      call fc2gp(gu  ,NLON,NLPP*NLEV)
      call fc2gp(gv  ,NLON,NLPP*NLEV)
      call fc2gp(gt  ,NLON,NLPP*NLEV)
      call fc2gp(gd  ,NLON,NLPP*NLEV)
      call fc2gp(gz  ,NLON,NLPP*NLEV)
      call fc2gp(gpj ,NLON,NLPP)
      call fc2gp(gpmt,NLON,NLPP)
      call fc2gp(gp  ,NLON,NLPP)
      if (nqspec == 1) call fc2gp(gq  ,NLON,NLPP*NLEV)
      gp = exp(gp)

      call calcgp(gtn,gqn,guz,gvz,gpmt,gvpp,gphi)

      gut = gu * gt
      gvt = gv * gt
      gke = gu * gu + gv * gv
      if (nqspec == 1) then
         guq = gu * gq
         gvq = gv * gq
      endif
!
!     add non linear geopotential terms
!
      gke(:,:) = gke(:,:) + gphi(:,:)

!
!     fft
!

      call gp2fc(gtn ,NLON,NLPP*NLEV)
      call gp2fc(gqn ,NLON,NLPP*NLEV)

      call gp2fc(gut ,NLON,NLPP*NLEV)
      call gp2fc(gvt ,NLON,NLPP*NLEV)
      call gp2fc(guz ,NLON,NLPP*NLEV)
      call gp2fc(gvz ,NLON,NLPP*NLEV)
      call gp2fc(gke ,NLON,NLPP*NLEV)
      call gp2fc(gvpp,NLON,NLPP     )
      if (nqspec == 1) then
         call gp2fc(guq ,NLON,NLPP*NLEV)
         call gp2fc(gvq ,NLON,NLPP*NLEV)
      endif
!
!     direct Legendre transformation (fourier domain to spectral domain)
!
      call fc2sp(gvpp,spf)
      call mktend(sdf,stf,szf,gtn,gvz,guz,gke,gut,gvt)
      call mpsumsc(spf,spt,1)
      call mpsumsc(stf,stt,NLEV)
      call mpsumsc(sdf,sdt,NLEV)
      call mpsumsc(szf,szt,NLEV)
      if (nqspec == 1) then
         call qtend(sqf,gqn,guq,gvq)
         call mpsumsc(sqf,sqt,NLEV)
      endif
!
!     compute entropy
!
      if(nentropy > 0) then
       zp00=100000.
       dentrop(:)=psurf*gp(:)
       dentropy(:,1)=0.
       do jlev=1,NLEV
        dentrot(:,jlev)=ct*(gt(:,jlev)+t0(jlev))
        dentroq(:,jlev)=gq(:,jlev)*psurf/dentrop(:)
        dentropy(:,1)=dentropy(:,1)                                     &
     &       +acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)  &
     &       *log(dentrot(:,jlev)*(zp00/(dentrop(:)*sigma(jlev)))**akap)
       enddo
       if(nentropy==2) dentrot(:,:)=1.
      endif
!
!     save u, v, and ps (at time t) for tracer transport
!
       dp0(:)=psurf*gp(:)
       do jlev=1,NLEV
        du0(:,jlev)=cv*gu(:,jlev)*SQRT(rcsq(:))
        dv0(:,jlev)=cv*gv(:,jlev)*SQRT(rcsq(:))
       enddo
!
!     save u v t q and ps (at time t) for further diagnostics
!
      if(nenergy > 0) then
       dp(:)  =dp0(:)
       du(:,:)=du0(:,:)
       dv(:,:)=dv0(:,:)
       do jlev=1,NLEV
        dt(:,jlev)=ct*(gt(:,jlev)+t0(jlev))
        if (nqspec == 1) dq(:,jlev)=gq(:,jlev)*psurf/dp(:)
       enddo
      endif

!     Diagnostic output and GUI calls

      if (mod(nstep,ndiag)==0 .or. mod(nstep,nafter)==0) then
         call mpgagp(zgp,gp,1);
         call mpgagp(zgp,dtsa,1)
         if (mypid == NROOT) t2mean = sum(zgp) / (NLON * NLAT) ! Mean of 2m temperature
         call mpgagp(zgp,dprc,1)        ! Convective precip
         if (mypid == NROOT) precip = sum(zgp)
         call mpgagp(zgp,dprl,1)        ! Large scale precip
         if (mypid == NROOT) precip = (precip + sum(zgp)) / (NLON * NLAT)
         call mpgagp(zgp,devap,1)       ! Evaporation
         if (mypid == NROOT) evap = sum(zgp) / (NLON * NLAT)
         call mpgagp(zgp,dftu,1)        ! OLR = dftu level 1
         if (mypid == NROOT) olr = -sum(zgp) / (NLON * NLAT)! make positive upwards
         gp(:) = gp(:) - 1.0
         call gp2fc(gp,NLON,NLPP)
         call fc2sp(gp,span)
         call mpsum(span,1)
 
         call mpgacs(csu)
         call mpgacs(csv)
         call mpgacs(cst)
         call mpgacs(csm)
         call mpgacs(ccc)
 
      endif
      if (nqspec == 0) then
         do jlev = 1 , NLEV
           ! dq(:,jlev) = dq(:,jlev) + gqn(:,jlev) * psurf / dp(:)
         enddo
      endif
      return
      end

!     =================
!     SUBROUTINE CALCGP
!     =================

      subroutine calcgp(gtn,gqn,guz,gvz,gpm,gvp,gphi)

!     *****************************************************
!     * computes nonlinear tendencies in grid point space *
!     *****************************************************

      use pumamod
      implicit none
      integer jlev,jlej,jhor

      real gtn(NHOR,NLEV)
      real gqn(NHOR,NLEV)                                                       !NEU
      real gphi(NHOR,NLEV)
      real guz(NHOR,NLEV), gvz(NHOR,NLEV)
      real gpm(NHOR)     , gvp(NHOR)
      real zsdotp(NHOR,NLEM),zsumd(NHOR)
      real ztpta(NHOR),ztptb(NHOR)
      real zvgpg(NHOR,NLEV)
      real gtd(NHOR,NLEM)
      real gqm(NHOR,NLEM)                                                       !NEU

      real gud(NHOR,NLEM)
      real gvd(NHOR,NLEM)

      real ztv1(NHOR,NLEV),ztv2(NHOR,NLEV),zq(NHOR)

      do jlev = 1 , NLEV
         zvgpg(:,jlev) = rcsq  * (gu(:,jlev) * gpm + gv(:,jlev) * gpj)
!
!     set pseudo temperatures to use virtual temperatures
!     (note: gq=ps*q)
!
         if (nqspec == 1) then
            zq(:)=AMAX1(gq(:,jlev)/gp(:),0.)
         else
            zq(:)=max(dq(:,jlev),0.0)
         endif
         ztv1(:,jlev)=(gt(:,jlev)+t0(jlev))*(1.+(1./rdbrv-1.)*zq(:))    &
     &               -t0(jlev)
         ztv2(:,jlev)=(gt(:,jlev)+t0(jlev))*(1.+(1./rdbrv-1.)*zq(:))    &
     &               /(1.+ADV*zq(:))                                    &
     &               -t0(jlev)

      enddo

!     *******
!     * gvp *
!     *******

      zsumd = dsigma(1) * gd(:,1)
      gvp   = dsigma(1) * zvgpg(:,1)
      zsdotp(:,1) = zsumd + gvp

      do jlev = 2 , NLEV-1
         zsumd = zsumd + dsigma(jlev) * gd(:,jlev)
         gvp   = gvp   + dsigma(jlev) * zvgpg(:,jlev)
         zsdotp(:,jlev) = zsumd + gvp
      enddo

      zsumd = zsumd + dsigma(NLEV) * gd(:,NLEV)
      gvp   = gvp   + dsigma(NLEV) * zvgpg(:,NLEV)

!     **************
!     * loop  400: *
!     **************

      do jlev = 1 , NLEM
         zsdotp(:,jlev) = (sigmah(jlev) * (zsumd+gvp) - zsdotp(:,jlev))
         gtd(:,jlev) = zsdotp(:,jlev) * (gt(:,jlev+1) - gt(:,jlev))
         gqm(:,jlev) = zsdotp(:,jlev) * (gq(:,jlev+1) + gq(:,jlev))
         gud(:,jlev) = zsdotp(:,jlev) * (gu(:,jlev+1) - gu(:,jlev))
         gvd(:,jlev) = zsdotp(:,jlev) * (gv(:,jlev+1) - gv(:,jlev))
      enddo

!     *************
!     * top level *
!     *************

      zsumd = zvgpg(:,1) * dsigma(1)

      gtn(:,1) = gt(:,1) * gd(:,1) - akap * ztv2(:,1) * gd(:,1)         &
     &   - rdsig(1)*(gtd(:,1) + t01s2(1) * (sigmah(1)*gvp-zsumd))

      gqn(:,1) = - rdsig(1) * gqm(:,1)

      guz(:,1) =-gu(:,1) * gz(:,1) - gpj * ztv1(:,1) - rdsig(1)*gvd(:,1)
      gvz(:,1) = gv(:,1) * gz(:,1) - gpm * ztv1(:,1) - rdsig(1)*gud(:,1)

      dw(:,1)=gd(:,1)*gp(:)*psurf*ww

!     ****************
!     * inner levels *
!     ****************

      do jlev = 2 , NLEV-1
         ztpta = c(1,jlev) *  zvgpg(:,1)
         ztptb = c(1,jlev) * (zvgpg(:,1) + gd(:,1))

         do jlej = 2 , jlev
            ztpta = ztpta + c(jlej,jlev) *  zvgpg(:,jlej)
            ztptb = ztptb + c(jlej,jlev) * (zvgpg(:,jlej) + gd(:,jlej))
         enddo

         zsumd = zsumd + zvgpg(:,jlev) * dsigma(jlev)

         gtn(:,jlev) = gt(:,jlev) * gd(:,jlev)                          &
     &       + akap * ztv2(:,jlev) * (zvgpg(:,jlev) - ztptb)            &
     &       + tkp(jlev) * (zvgpg(:,jlev) - ztpta)                      &
     &       - rdsig(jlev) * (gtd(:,jlev) + gtd(:,jlev-1)               &
     &                       +gvp*(t01s2(jlev)*sigmah(jlev)             &
     &                            +t01s2(jlev-1)*sigmah(jlev-1))        &
     &                       -zsumd*(t01s2(jlev-1)+t01s2(jlev))         &
     &                       +zvgpg(:,jlev)*dsigma(jlev)*t01s2(jlev-1))

         gqn(:,jlev) = - rdsig(jlev)*(gqm(:,jlev) - gqm(:,jlev-1))

         guz(:,jlev) = - gu(:,jlev) * gz(:,jlev) - gpj * ztv1(:,jlev)   &
     &       - rdsig(jlev)*(gvd(:,jlev) + gvd(:,jlev-1))

         gvz(:,jlev) =   gv(:,jlev) * gz(:,jlev) - gpm * ztv1(:,jlev)   &
     &       - rdsig(jlev)*(gud(:,jlev) + gud(:,jlev-1))

         dw(:,jlev)=(zvgpg(:,jlev)-ztptb)*gp(:)*psurf*ww

      enddo

!     ****************
!     * bottom level *
!     ****************

      ztpta = c(1,NLEV) *  zvgpg(:,1)
      ztptb = c(1,NLEV) * (zvgpg(:,1) + gd(:,1))

      do jlej = 2 , NLEV
         ztpta = ztpta + c(jlej,NLEV) *  zvgpg(:,jlej)
         ztptb = ztptb + c(jlej,NLEV) * (zvgpg(:,jlej) + gd(:,jlej))
      enddo

      gtn(:,NLEV) = gt(:,NLEV) * gd(:,NLEV)                             &
     &   + akap*ztv2(:,NLEV)*(zvgpg(:,NLEV)-ztptb)                      &
     &   + tkp(NLEV)*(zvgpg(:,NLEV)-ztpta)                              &
     &   - rdsig(NLEV)*(gtd(:,NLEM)                                     &
     &                 +t01s2(NLEV-1)*(sigmah(NLEV-1)*gvp-zsumd))

      gqn(:,NLEV) = rdsig(NLEV) * gqm(:,NLEM)

      guz(:,NLEV) = -gu(:,NLEV)*gz(:,NLEV) - gpj*ztv1(:,NLEV)           &
     &    - rdsig(NLEV) * gvd(:,NLEM)
      gvz(:,NLEV) =  gv(:,NLEV)*gz(:,NLEV) - gpm*ztv1(:,NLEV)           &
     &    - rdsig(NLEV) * gud(:,NLEM)

      dw(:,NLEV)=(zvgpg(:,NLEV)-ztptb)*gp(:)*psurf*ww

!
!     compute non linear geopotential terms
!

      ztv1(:,:)=ztv1(:,:)-gt(:,:)
      do jlev=1,NLEV
      do jhor=1,NHOR
       gphi(jhor,jlev)=dot_product(g(:,jlev),ztv1(jhor,:))              &
     &                *2./rcsq(jhor)
      enddo
      enddo

      return
      end

!     ====================
!     SUBROUTINE SPECTRALA
!     ====================

      subroutine spectrala
      use pumamod
      implicit none
      integer jlev,jsp,jn,jlev2
      real zsum,zq,zt,zm,za,zb,zz,z0
!
!*    Add adiabatic and diabatic tendencies
!
!     the adiabatic tendencies are added using the semi implicit scheme
!     described in
!     Hoskins and Simmons 1975 (Q.J.R.Meteorol.Soc.,101,637-655) (HS75)
!     To compare the code directly with HS75 the following notes might be
!     helpful (in addition to the comments below):
!
!     - *x* means model variable x
!     - eq.x referres to equation x of HS75
!     - the script T of HS75 (e.g. 1st term rhs of eq.9) is stored in *stt*
!     - the script D of HS75 (e.g. 1st term rhs of eq.8) is stored in *sdt*
!     - the script P of HS75 (e.g. 1st term rhs of eq.10) is stored in -*spt*
!     - the surface geopotential is stored in *so*
!
!     - the vertical scheme has being changed to the ECMWF scheme
!       (see e.g. Simmons and Burridge 1981, Mon.Wea.Rev.,109,758-766).
!       in this scheme,  matrix g differs from that in HS75.
!
!     - in addition to the dry HS75 version also the tendencys of specific
!       humidity are processed
!
      real azm(NSPP,NLEV)
      real atm(NSPP,NLEV)
      real aqm(NSPP,NLEV)
      real adt(NSPP,NLEV)
      real adm(NSPP,NLEV)
      real zgt(NSPP,NLEV)
      real zgm(NSPP,NLEV)
      real apm(NSPP)
!
!     franks diagnostics
!
      real, allocatable :: ztt(:,:)
      real, allocatable :: zttgp(:,:)
      real, allocatable :: zsd(:,:),zsz(:,:),zsq(:,:),zsp(:)
      real, allocatable :: zugp(:,:),zvgp(:,:),zekin(:,:)
      real, allocatable :: zqgp(:,:),zpgp(:)
!
!*    0. save prognostic variables at (t-dt)
!        and the non-linear divergence tendency terms
!
      apm(:)   = spm(:)   ! log surface pressure
      adm(:,:) = sdm(:,:) ! divergence
      azm(:,:) = szm(:,:) ! (absolut) vorticity
      atm(:,:) = stm(:,:) ! temperature
      adt(:,:) = sdt(:,:) ! divergence tendency
      if (nqspec == 1) aqm(:,:) = sqm(:,:) ! spec.  humidity
!
!*    do the advective time step
!
      if(nadv > 0) then
!
!*    1. calculate divergence on timelevel t (solving eq.17),
!        which will replace the divergence tendency sdt
!
!     1.a precompute (g*script T; *zgt*) and (phi-phi*=g*T, eq.11; *zgm*)
!         needed for the rhs of eq.17.
!         (note that phi is needed in eq.17 and, therefor,
!         the surface geopotential phi* is added later (loop 1.b))
!
       do jlev = 1 , NLEV
         do jsp=1,NSPP
            zgt(jsp,jlev) = dot_product(g(:,jlev),stt(jsp,:))
            zgm(jsp,jlev) = dot_product(g(:,jlev),atm(jsp,:))
         enddo
       enddo
!
!     1.b compute divergence at time t (i.e. solve eq.17)
!         and overwrite *sdt* with the result
!         for comparison with HS75 note:
!
!         - *spt* contains -(!)script P
!         - *spm* contains log(ps)(t-dt)
!         - surface geopotential (*so*) needs to be added (see 1.a)
!         - bm1 is the invers of matrix (1/cn I+B dt**2) (lhs eq.17)
!         - zz is set to the rhs of eq.17
!         - zsum holds the result of the dot product of rhs(eq17) and bm1
!           (therefor jlev2-loop)
!
       do jlev = 1 , NLEV
        do jsp = 1 , NSPP
          jn = nindex(jsp)
          zsum = 0.0
          if (jn > 0) then
            zq = 1.0 / (jn * (jn+1))
            do jlev2 = 1 , NLEV
              z0 = t0(jlev2)
              zt = zgt(jsp,jlev2) - z0 * spt(jsp)
              zm = zgm(jsp,jlev2) + z0 * spm(jsp)
              za = adt(jsp,jlev2) * zq + sop(jsp)
              zb = adm(jsp,jlev2) * zq
              zz = zb + delt * (zm + za + delt * zt)
              zsum = zsum + zz * bm1(jlev2,jlev,jn)
            enddo
          endif
          sdt(jsp,jlev) = zsum
        enddo
       enddo
       if (mypid == NROOT) then
        sdt(1:2,:) = 0.0 ! first mode should be zero (errors due to numerics)
       endif
!
!*    2. calculate (-log) surface pressure tendency from eq.15 (pi=*dsigma*)
!
       do jlev = 1 , NLEV
         spt = spt + dsigma(jlev) * sdt(:,jlev)
       enddo
!
!*    3. calculate temperature tendency from eq.14
!
       do jlev = 1 , NLEV
        do jsp = 1 , NSPP
         stt(jsp,jlev)=stt(jsp,jlev)-dot_product(tau(:,jlev),sdt(jsp,:))
        enddo
       enddo
!
      endif
!
!*    4. time stepping and time filtering (1st part)
!        the time filtering is splitted into two parts:
!
!        1st part: xf(prel)=x+eps*(xf(-)-2x)
!        2nd part: xf=xf(prel)+eps*x(+)
!
!        together the complete filter is xf=x+eps(xf(-)-2x+x(+))
!
!        with: x        = the prognostic variable to be filtered
!              xf       = the filtered variable (current time step)
!              xf(-)    = the filtered variable (old time step)
!              xf(prel) = a preliminary filterd variable
!              x(+)     = x at time (t+2dt)
!              eps      = filter constant = *pnu*
!
!     4.a 1st part of time filter
!
      if (nkits == 0) then
         spm = pnu21 * spp + pnu * apm
         sdm = pnu21 * sdp + pnu * adm
         szm = pnu21 * szp + pnu * azm
         stm = pnu21 * stp + pnu * atm
         if (nqspec == 1) sqm = pnu21 * sqp + pnu * aqm
      endif
!
!     4.b add tendencies
!
      if(nadv > 0 ) then
       spp = apm - delt2 * spt   ! log surface pressure (negative tendencies)
       sdp =   2.0 * sdt - adm   ! note that sdt=adm+delt*tendencies (see 1.)
       stp = delt2 * stt + atm   ! temperature
       szp = delt2 * szt + azm   ! vorticity
       if (nqspec == 1) sqp = delt2 * sqt + aqm   ! spec. humidity
      else
       spp = apm   ! log surface pressure (negative tendencies)
       sdp = adm   ! note that sdt=adm+delt*tendencies (see 1.)
       stp = atm   ! temperature
       szp = azm   ! vorticity
       if (nqspec == 1) sqp = aqm   ! spec. humidity
      endif
!
!     conserve p
!
      if (mypid == NROOT) then
       spp(1) = 0.0
       spp(2) = 0.0
      endif

!*    now the adiabatic time step is finished beside the time filtering.
!     2nd part of time filtering is done in subroutine spectrald
!
!     finaly the partial arrays are gathered from all processors (mpi)
!
      call mpgallsp(sd,sdp,NLEV)
      call mpgallsp(sz,szp,NLEV)
      call mpgallsp(st,stp,NLEV)
      call mpgallsp(sp,spp,   1)
      if (nqspec == 1) call mpgallsp(sq,sqp,NLEV)
!
!     franks diagnostic
!
      if(ndiagsp==1) then
       allocate(ztt(NESP,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       dsp3d(:,1:NLEV,3)=ztt(:,1:NLEV)*ct*ww
       deallocate(ztt)
      endif
!
!     entropy/energy diagnostics
!
      if(nentropy > 0) then
       allocate(ztt(NESP,NLEV))
       allocate(zttgp(NHOR,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       ztt(:,:)=ztt(:,:)*ct*ww
       call sp2fl(ztt,zttgp,NLEV)
       call fc2gp(zttgp,NLON,NLPP*NLEV)
       deallocate(ztt)
       dentropy(:,2)=0.
       do jlev=1,NLEV
        dentropy(:,2)=dentropy(:,2)                                     &
     &         +zttgp(:,jlev)/dentrot(:,jlev)                           &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
       enddo
       deallocate(zttgp)
      endif
      if(nenergy > 0) then
       allocate(ztt(NESP,NLEV))
       allocate(zsd(NESP,NLEV))
       allocate(zsz(NESP,NLEV))
       if (nqspec == 1) allocate(zsq(NESP,NLEV))
       allocate(zsp(NESP))
       allocate(zugp(NHOR,NLEV))
       allocate(zvgp(NHOR,NLEV))
       allocate(zqgp(NHOR,NLEV))
       allocate(zpgp(NHOR))
       allocate(zekin(NHOR,NLEV))
       allocate(zttgp(NHOR,NLEV))
       call mpgallsp(zsd,adm,NLEV)
       call mpgallsp(zsz,azm,NLEV)
       if (nqspec == 1) call mpgallsp(zsq,aqm,NLEV)
       call mpgallsp(ztt,stt,NLEV)
       call mpgallsp(zsp,apm,1)
       call dv2uv(zsd,zsz,zugp,zvgp)
       if (nqspec == 1) call sp2fl(zsq,zqgp,NLEV)
       call sp2fl(ztt,zttgp,NLEV)
       call sp2fl(zsp,zpgp,1)
       call fc2gp(zugp,NLON,NLPP*NLEV)
       call fc2gp(zvgp,NLON,NLPP*NLEV)
       if (nqspec == 1) then
          call fc2gp(zqgp,NLON,NLPP*NLEV)
       else
          zqgp(:,:) = 0.0
       endif
       call fc2gp(zttgp,NLON,NLPP*NLEV)
       call fc2gp(zpgp,NLON,NLPP)
       zpgp(:)=psurf*exp(zpgp(:))
       zttgp(:,:)=ct*ww*zttgp(:,:)
       do jlev=1,NLEV
        zekin(:,jlev)=0.5*(zugp(:,jlev)*zugp(:,jlev)                    &
     &                    +zvgp(:,jlev)*zvgp(:,jlev))*cv*cv*rcsq(:)
       enddo
       call dv2uv(sd,sz,zugp,zvgp)
       call fc2gp(zugp,NLON,NLPP*NLEV)
       call fc2gp(zvgp,NLON,NLPP*NLEV)
       denergy(:,28)=0.
       denergy(:,2)=0.
       do jlev=1,NLEV
        if (nqspec == 1) zqgp(:,jlev)=zqgp(:,jlev)*psurf/zpgp(:)
        zekin(:,jlev)=0.5*(zugp(:,jlev)*zugp(:,jlev)                    &
     &                    +zvgp(:,jlev)*zvgp(:,jlev))*cv*cv*rcsq(:)     &
     &               -zekin(:,jlev)
        denergy(:,28)=denergy(:,28)                                     &
     &               -zekin(:,jlev)/deltsec2*zpgp(:)/ga*dsigma(jlev)
        denergy(:,2)=denergy(:,2)                                       &
     &              +zttgp(:,jlev)                                      &
     &              *acpd*(1.+adv*zqgp(:,jlev))*zpgp(:)/ga*dsigma(jlev)

       enddo
       deallocate(ztt)
       deallocate(zsd)
       deallocate(zsz)
       if (nqspec == 1) deallocate(zsq)
       deallocate(zsp)
       deallocate(zugp)
       deallocate(zvgp)
       deallocate(zqgp)
       deallocate(zpgp)
       deallocate(zekin)
       deallocate(zttgp)
      endif
!
      return
      end

!-Diabatic-subroutines  (Frank (Larry) 26-Nov-99)

!     =====================
!     SUBROUTINE GRIDPOINTD
!     =====================

      subroutine gridpointd
      use pumamod
      implicit none
      integer jlev,jlat

      real zfac
!
      real sdf(NESP,NLEV)
      real stf(NESP,NLEV)
      real sqf(NESP,NLEV)
      real szf(NESP,NLEV)

      real zqout(NHOR,NLEV)
      real zgq(NLON,NLAT,NLEV)
!
!*    Diabatic Gridpoint Calculations
!
      gudt(:,:)=0.
      gvdt(:,:)=0.
      gtdt(:,:)=0.
      gqdt(:,:)=0.
      dudt(:,:)=0.
      dvdt(:,:)=0.
      dtdt(:,:)=0.
      dqdt(:,:)=0.

!
!     transform to gridpoint domain
!

      call invlegd

      call fc2gp(gu  ,NLON,NLPP*NLEV)
      call fc2gp(gv  ,NLON,NLPP*NLEV)
      call fc2gp(gt  ,NLON,NLPP*NLEV)
      call fc2gp(gp  ,NLON,NLPP)
      if (nqspec == 1) then
         call fc2gp(gq  ,NLON,NLPP*NLEV)
      endif
!
!     dimensionalize
!

      dp(:)=psurf*exp(gp(:))
      do jlev = 1 , NLEV
       du(:,jlev)=cv*gu(:,jlev)*SQRT(rcsq(:))
       dv(:,jlev)=cv*gv(:,jlev)*SQRT(rcsq(:))
       dt(:,jlev)=ct*(gt(:,jlev)+t0(jlev))
       if (nqspec == 1) dq(:,jlev)=gq(:,jlev)*psurf/dp(:)
      enddo
!
!     tracer transport
!
      if (nsela > 0 .and. nkits == 0) then
       if (nqspec == 0) then
        dqt(:,:) = dq(:,:) ! Save old value of q
        call mpgagp(zgq,dq,NLEV)
        if (mypid == NROOT) then
         do jlat = 1 , NLAT
          dtrace(:,NLAT+1-jlat,:,1) = zgq(:,jlat,:)
         enddo ! jlat
        endif ! mypid
       endif ! nqspec
       call tracer_main
       if (nqspec == 0) then
        if (mypid == NROOT) then
         do jlat = 1 , NLAT
          zgq(:,jlat,:) = dtrace(:,NLAT+1-jlat,:,1)
         enddo
        endif ! mypid
        call mpscgp(zgq,dq,NLEV)
        dqt(:,:) = (dq(:,:) - dqt(:,:)) / deltsec !  q advection term
       endif ! nqspec
      endif ! nkits

!
!     compute output specific humidity
!

      if (mod(nstep,nafter)==0 .and. nqspec == 1) then
       do jlev=1,NLEV
        zqout(:,jlev)=gq(:,jlev)/exp(gp(:))
        call gp2fc(zqout(:,jlev),NLON,NLPP)
        call fc2sp(zqout(:,jlev),sqout(:,jlev))
       enddo
       call mpsum(sqout,NLEV)
      endif

!
!     dbug print out
!

      if(nprint > 0) then
       if(mypid==NROOT) then
        print*,'-------------- SUBROUTINE GRIDPOINTD ---------------'
       endif
       call prdbug1
      endif

!
!     PARAMETERIZATION ROUTINES TO BE INCLUDED BELOW
!
!     a) miscellaneous parameterizations (like q-fixer)
!

      call miscstep

!
!     e) surface parmeterizations or models
!     was previously called after rainstep, but in the coupling we need
!     surface updated before diabatic processes to update immediately
!     after a call to goldstein

      call surfstep

!
!     dbug print out
!

      if(nprint > 0) then
       if(mypid==NROOT) then
        print*,'After MISC:'
       endif
       call prdbug2
      endif

!
!     b) surface fluxes and vertical diffusion
!

      call fluxstep

!
!     dbug print out
!

      if(nprint > 0) then
       if(mypid==NROOT) then
        print*,'After FLUX:'
       endif
       call prdbug2
      endif

!
!     c) radiation
!

      if(nrad > 0) call radstep

!
!     dbug print out
!

      if(nprint > 0) then
       if(mypid==NROOT) then
        print*,'After RAD:'
       endif
       call prdbug2
      endif

!
!     d) convective and large scale rain and clouds
!

      call rainstep


!
!     dbug print out
!

      if(nprint > 0) then
       if(mypid==NROOT) then
        print*,'After RAIN:'
       endif
       call prdbug2
      endif


!
!     END OF PARAMETERISATION ROUTINES
!
!     dbug print out
!

      if(nprint > 0) then
       if(mypid==NROOT) then
        print*,'FINAL:'
       endif
       call prdbug2
      endif

!
!     franks diagnostic
!

      if(ndiaggp==1) then
       dgp3d(:,1:NLEV,1)=dtdt(:,1:NLEV)
       do jlev=1,NLEV
        dgp3d(:,jlev,11)=dqdt(:,jlev)*dp(:)*dsigma(jlev)/ga/1000
       enddo
      endif
!
!     entropy/energy diagnostics
!
      if(nentropy > 0) then
       dentropy(:,4)=0.
       dentropy(:,33)=0.
       do jlev=1,NLEV
        if(nentropy > 2) then
        dentropy(:,4)=dentropy(:,4)                                     &
     &         +dtdt(:,jlev)/dt(:,jlev)                                 &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        zfac=-1.*ww*tfrc(jlev)/(1.+tfrc(jlev)*delt2)
        dentropy(:,33)=dentropy(:,33)                                   &
     &                +((du(:,jlev)+dudt(:,jlev)*deltsec2)**2           &
     &                 +(dv(:,jlev)+dvdt(:,jlev)*deltsec2)**2)          &
     &                *zfac*dp(:)/ga*dsigma(jlev)/dt(:,jlev)
        else
        dentropy(:,4)=dentropy(:,4)                                     &
     &         +dtdt(:,jlev)/dentrot(:,jlev)                            &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        zfac=-1.*ww*tfrc(jlev)/(1.+tfrc(jlev)*delt2)
        dentropy(:,33)=dentropy(:,33)                                   &
     &                +((du(:,jlev)+dudt(:,jlev)*deltsec2)**2           &
     &                 +(dv(:,jlev)+dvdt(:,jlev)*deltsec2)**2)          &
     &                *zfac*dentrop(:)/ga*dsigma(jlev)/dentrot(:,jlev)
        endif
       enddo
      endif
      if(nenergy > 0) then
       denergy(:,4)=0.
       denergy(:,23)=0.
       do jlev=1,NLEV
        denergy(:,4)=denergy(:,4)                                       &
     &              +dtdt(:,jlev)                                       &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        zfac=-1.*ww*tfrc(jlev)/(1.+tfrc(jlev)*delt2)
        denergy(:,23)=denergy(:,23)                                     &
     &               +((du(:,jlev)+dudt(:,jlev)*deltsec2)**2            &
     &                +(dv(:,jlev)+dvdt(:,jlev)*deltsec2)**2)           &
     &               *zfac*dp(:)/ga*dsigma(jlev)
       enddo
      endif
!
!     de-dimensionalize
!

      do jlev = 1 , NLEV
       gudt(:,jlev)=dudt(:,jlev)/SQRT(rcsq(:))/cv/ww                    &
     &             +gudt(:,jlev)
       gvdt(:,jlev)=dvdt(:,jlev)/SQRT(rcsq(:))/cv/ww                    &
     &             +gvdt(:,jlev)
       gtdt(:,jlev)=dtdt(:,jlev)/ct/ww                                  &
     &             +gtdt(:,jlev)
       gqdt(:,jlev)=dqdt(:,jlev)*dp(:)/ww/psurf                         &
     &             +gqdt(:,jlev)
      enddo

!
!     transform to spectral space
!

      call gp2fc(gtdt,NLON,NLPP*NLEV)
      call gp2fc(gudt,NLON,NLPP*NLEV)
      call gp2fc(gvdt,NLON,NLPP*NLEV)
      if (nqspec == 1) call gp2fc(gqdt,NLON,NLPP*NLEV)

!
!     direct Legendre transformation (fourier domain to spectral domain)
!
      do jlev = 1 , NLEV
         call fc2sp(gtdt(1,jlev),stf(1,jlev))
         if (nqspec == 1) call fc2sp(gqdt(1,jlev),sqf(1,jlev))
      enddo

      call uv2dv(gudt,gvdt,sdf,szf)

      call mpsumsc(stf,stt,NLEV)
      call mpsumsc(sdf,sdt,NLEV)
      call mpsumsc(szf,szt,NLEV)
      if (nqspec == 1) call mpsumsc(sqf,sqt,NLEV)
      if (nqspec == 0) dq(:,:) = dq(:,:) + dqdt(:,:) * deltsec

      return
      end

!     ==================
!     SUBROUTINE PRDBUG1
!     ==================

      subroutine prdbug1
      use pumamod
      implicit none
      integer jlev
      real zmaxv,zminv

      integer kmaxl(1),kminl(1)

      real zfpr1(NLON*NLAT,NLEV)
      real zfpr2(NLON*NLAT,NLEV)
      real zfpr3(NLON*NLAT,NLEV)
      real zfpr4(NLON*NLAT,NLEV)
      real zfpr5(NLON*NLAT,NLEV)
      real zfpr6(NLON*NLAT,NLEV)

      call mpgagp(zfpr1,dt,NLEV)
      call mpgagp(zfpr2,dq,NLEV)
      call mpgagp(zfpr3,du,NLEV)
      call mpgagp(zfpr4,dv,NLEV)
      call mpgagp(zfpr5,dcc,NLEV)
      call mpgagp(zfpr6,dql,NLEV)

      if(mypid==NROOT) then
       if(nprint==1) then
        print*,'Global diagnostics: '
        do jlev=1,NLEV
         zmaxv=MAXVAL(zfpr1(:,jlev))
         kmaxl=MAXLOC(zfpr1(:,jlev))
         zminv=MINVAL(zfpr1(:,jlev))
         kminl=MINLOC(zfpr1(:,jlev))
         print*,'L= ',jlev,' MAX T= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN T= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr2(:,jlev))
         kmaxl=MAXLOC(zfpr2(:,jlev))
         zminv=MINVAL(zfpr2(:,jlev))
         kminl=MINLOC(zfpr2(:,jlev))
         print*,'L= ',jlev,' MAX Q= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN Q= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr3(:,jlev))
         kmaxl=MAXLOC(zfpr3(:,jlev))
         zminv=MINVAL(zfpr3(:,jlev))
         kminl=MINLOC(zfpr3(:,jlev))
         print*,'L= ',jlev,' MAX U= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN U= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr4(:,jlev))
         kmaxl=MAXLOC(zfpr4(:,jlev))
         zminv=MINVAL(zfpr4(:,jlev))
         kminl=MINLOC(zfpr4(:,jlev))
         print*,'L= ',jlev,' MAX V= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN V= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr5(:,jlev))
         kmaxl=MAXLOC(zfpr5(:,jlev))
         zminv=MINVAL(zfpr5(:,jlev))
         kminl=MINLOC(zfpr5(:,jlev))
         print*,'L= ',jlev,' MAX C= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN C= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr6(:,jlev))
         kmaxl=MAXLOC(zfpr6(:,jlev))
         zminv=MINVAL(zfpr6(:,jlev))
         kminl=MINLOC(zfpr6(:,jlev))
         print*,'L= ',jlev,' MAX L= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN L= ',zminv,' NHOR= ',kminl(1)
        enddo
       elseif(nprint==2) then
        print*,'Local diagnostics at nhor= ',nprhor,': '
        do jlev=1,NLEV
         print*,'L= ',jlev,' T= ',zfpr1(nprhor,jlev)                    &
     &                    ,' Q= ',zfpr2(nprhor,jlev)
         print*,'L= ',jlev,' U= ',zfpr3(nprhor,jlev)                    &
     &                    ,' V= ',zfpr4(nprhor,jlev)
         print*,'L= ',jlev,' C= ',zfpr5(nprhor,jlev)                    &
     &                    ,' L= ',zfpr6(nprhor,jlev)
        enddo
       endif
      endif

      call mpgagp(zfpr1(1,1),dls,1)
      call mpgagp(zfpr2(1,1),dp,1)
      call mpgagp(zfpr3(1,1),drhs,1)
      call mpgagp(zfpr4(1,1),dalb,1)
      call mpgagp(zfpr5(1,1),dt(1,NLEP),1)
      call mpgagp(zfpr6(1,1),dq(1,NLEP),1)

      if(mypid==NROOT) then
       if(nprint==1) then
        print*,'Global diagnostic:'
        zmaxv=MAXVAL(zfpr5(:,1))
        kmaxl=MAXLOC(zfpr5(:,1))
        zminv=MINVAL(zfpr5(:,1))
        kminl=MINLOC(zfpr5(:,1))
        print*,'SURF MAX T= ',zmaxv,' NHOR= ',kmaxl(1)
        print*,'SURF MIN T= ',zminv,' NHOR= ',kminl(1)
        zmaxv=MAXVAL(zfpr6(:,1))
        kmaxl=MAXLOC(zfpr6(:,1))
        zminv=MINVAL(zfpr6(:,1))
        kminl=MINLOC(zfpr6(:,1))
        print*,'SURF MAX Q= ',zmaxv,' NHOR= ',kmaxl(1)
        print*,'SURF MIN Q= ',zminv,' NHOR= ',kminl(1)
        zmaxv=MAXVAL(zfpr2(:,1))
        kmaxl=MAXLOC(zfpr2(:,1))
        zminv=MINVAL(zfpr2(:,1))
        kminl=MINLOC(zfpr2(:,1))
        print*,'SURF MAX P= ',zmaxv,' NHOR= ',kmaxl(1)
        print*,'SURF MIN P= ',zminv,' NHOR= ',kminl(1)
        zmaxv=MAXVAL(zfpr3(:,1))
        kmaxl=MAXLOC(zfpr3(:,1))
        zminv=MINVAL(zfpr3(:,1))
        kminl=MINLOC(zfpr3(:,1))
        print*,'SURF MAX RHS= ',zmaxv,' NHOR= ',kmaxl(1)
        print*,'SURF MIN RHS= ',zminv,' NHOR= ',kminl(1)
        zmaxv=MAXVAL(zfpr4(:,1))
        kmaxl=MAXLOC(zfpr4(:,1))
        zminv=MINVAL(zfpr4(:,1))
        kminl=MINLOC(zfpr4(:,1))
        print*,'SURF MAX ALB= ',zmaxv,' NHOR= ',kmaxl(1)
        print*,'SURF MIN ALB= ',zminv,' NHOR= ',kminl(1)
       elseif(nprint==2) then
        print*,'Local diagnostics at nhor= ',nprhor,': '
        print*,'SURF: LS= ',zfpr1(nprhor,1)                             &
     &             ,' PS= ',zfpr2(nprhor,1)
        print*,'SURF: RHS= ',zfpr3(nprhor,1)                            &
     &             ,' ALB= ',zfpr4(nprhor,1)
        print*,'SURF: TS= ',zfpr5(nprhor,1)                             &
     &             ,' QS= ',zfpr6(nprhor,1)
       endif
      endif

      if(nprint==2) then
       call mpgagp(zfpr1(1,1),dicec,1)

       if(mypid==NROOT) then
        print*,'SURF: ICEC= ',zfpr1(nprhor,1)
       endif
      endif

      return
      end subroutine prdbug1

!     ==================
!     SUBROUTINE PRDBUG2
!     ==================

      subroutine prdbug2
      use pumamod
      implicit none
      integer jlev
      real zmaxv,zminv

      integer kmaxl(1),kminl(1)

      real zfpr1(NLON*NLAT,NLEV)
      real zfpr2(NLON*NLAT,NLEV)
      real zfpr3(NLON*NLAT,NLEV)
      real zfpr4(NLON*NLAT,NLEV)

      call mpgagp(zfpr1,dtdt,NLEV)
      call mpgagp(zfpr2,dqdt,NLEV)
      call mpgagp(zfpr3,dudt,NLEV)
      call mpgagp(zfpr4,dvdt,NLEV)

      if(mypid==NROOT) then
       if(nprint==1) then
        print*,'Global diagnostics: '
        do jlev=1,NLEV
         zmaxv=MAXVAL(zfpr1(:,jlev))
         kmaxl=MAXLOC(zfpr1(:,jlev))
         zminv=MINVAL(zfpr1(:,jlev))
         kminl=MINLOC(zfpr1(:,jlev))
         print*,'L= ',jlev,' MAX DT= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN DT= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr2(:,jlev))
         kmaxl=MAXLOC(zfpr2(:,jlev))
         zminv=MINVAL(zfpr2(:,jlev))
         kminl=MINLOC(zfpr2(:,jlev))
         print*,'L= ',jlev,' MAX DQ= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN DQ= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr3(:,jlev))
         kmaxl=MAXLOC(zfpr3(:,jlev))
         zminv=MINVAL(zfpr3(:,jlev))
         kminl=MINLOC(zfpr3(:,jlev))
         print*,'L= ',jlev,' MAX DU= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN DU= ',zminv,' NHOR= ',kminl(1)
         zmaxv=MAXVAL(zfpr4(:,jlev))
         kmaxl=MAXLOC(zfpr4(:,jlev))
         zminv=MINVAL(zfpr4(:,jlev))
         kminl=MINLOC(zfpr4(:,jlev))
         print*,'L= ',jlev,' MAX DV= ',zmaxv,' NHOR= ',kmaxl(1)
         print*,'L= ',jlev,' MIN DV= ',zminv,' NHOR= ',kminl(1)
        enddo
       elseif(nprint==2) then
        print*,'Local diagnostics at nhor= ',nprhor,': '
        do jlev=1,NLEV
         print*,'L= ',jlev,' DT= ',zfpr1(nprhor,jlev)                   &
     &                    ,' DQ= ',zfpr2(nprhor,jlev)
         print*,'L= ',jlev,' DU= ',zfpr3(nprhor,jlev)                   &
     &                    ,' DV= ',zfpr4(nprhor,jlev)
        enddo
       endif
      endif

      return
      end subroutine prdbug2

!     ====================
!     SUBROUTINE SPECTRALD
!     ====================

      subroutine spectrald
      use pumamod
      implicit none
      integer jlon,jlev,jhor,jlat
      real ztp,ztm,zsum3
!
      real :: zsdt1(NSPP,NLEV),zsdt2(NSPP,NLEV)
      real :: zszt1(NSPP,NLEV),zszt2(NSPP,NLEV)
!
!     franks diagnostics
!
      real, allocatable :: ztt(:,:)
      real, allocatable :: zttgp(:,:)
      real, allocatable :: zst(:,:),zsq(:,:),zstt(:,:),zstt2(:,:)
      real, allocatable :: ztgp(:,:),zdtgp(:,:),zqgp(:,:)
      real, allocatable :: zgw(:),zsum1(:)
!
!     prepare diagnostics of efficiency
!

      if(ndheat > 1) then
       allocate(zst(NESP,NLEV))      
       if (nqspec == 1) allocate(zsq(NESP,NLEV))      
       allocate(zstt(NESP,NLEV)) 
       allocate(zstt2(NESP,NLEV)) 
       allocate(ztgp(NHOR,NLEV)) 
       allocate(zqgp(NHOR,NLEV)) 
       allocate(zdtgp(NHOR,NLEV)) 
       allocate(zsum1(4))
       allocate(zgw(NHOR))
       call mpgallsp(zst,stp,NLEV)
       call mpgallsp(zstt,stt,NLEV)
       if (nqspec == 1) call mpgallsp(zsq,sqp,NLEV)
       jhor=0
       do jlat=1,NLPP
        do jlon=1,NLON
         jhor=jhor+1
         zgw(jhor)=gwd(jlat)
        enddo
       enddo
      endif

!
!     add tendencies from diabatic parameterizations
!

      szp = szp + delt2 * szt
      stp = stp + delt2 * stt
      sdp = sdp + delt2 * sdt
      if (nqspec == 1) sqp = sqp + delt2 * sqt

!
!     franks diagnostic
!

      if(ndiagsp==1) then
       allocate(ztt(NESP,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       dsp3d(:,1:NLEV,1)=ztt(:,1:NLEV)*ct*ww
       deallocate(ztt)
      endif
      if(ndiaggp==1) then
       call mkdqtgp
       do jlev=1,NLEV
        dgp3d(:,jlev,20)=dqt(:,jlev)*dp(:)*dsigma(jlev)/ga/1000.
       enddo
      endif
!
!     entropy/energy diagnostics
!
      if(nentropy > 0) then
       allocate(ztt(NESP,NLEV))
       allocate(zttgp(NHOR,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       ztt(:,:)=ztt(:,:)*ct*ww
       call sp2fl(ztt,zttgp,NLEV)
       call fc2gp(zttgp,NLON,NLPP*NLEV)
       deallocate(ztt)
       dentropy(:,3)=0.
       do jlev=1,NLEV
        if(nentropy > 2) then
        dentropy(:,3)=dentropy(:,3)                                     &
     &         +zttgp(:,jlev)/dt(:,jlev)                                &
     &         *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
        dentropy(:,3)=dentropy(:,3)                                     &
     &         +zttgp(:,jlev)/dentrot(:,jlev)                           &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       deallocate(zttgp)
      endif
      if(nenergy > 0) then
       allocate(ztt(NESP,NLEV))
       allocate(zttgp(NHOR,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       ztt(:,:)=ztt(:,:)*ct*ww
       call sp2fl(ztt,zttgp,NLEV)
       call fc2gp(zttgp,NLON,NLPP*NLEV)
       deallocate(ztt)
       denergy(:,3)=0.
       do jlev=1,NLEV
        denergy(:,3)=denergy(:,3)                                       &
     &              +zttgp(:,jlev)                                      &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       deallocate(zttgp)
      endif

!     calculates spectral tendencies from restoration (if included)
!     and biharmonic diffusion (both implicitely).
!     add newtonian cooling and drag.

      do jlev = 1 , NLEV
       stt(:,jlev)=((-damp(jlev)-tdisst(jlev)*sakpp(1:NSPP,jlev))       &
     &              *stp(:,jlev)+damp(jlev)*srp(:,jlev))                &
     &          /(1.+delt2*(damp(jlev)+tdisst(jlev)*sakpp(1:NSPP,jlev)))
       if (nqspec == 1)                                                 &
     & sqt(:,jlev)=-tdissq(jlev)*sakpp(1:NSPP,jlev)*sqp(:,jlev)         &
     &            /(1.+delt2*tdissq(jlev)*sakpp(1:NSPP,jlev))
       zsdt1(:,jlev)=-tfrc(jlev)*sdp(:,jlev)                            &
     &              /(1.+delt2*tfrc(jlev))
       zsdt2(:,jlev)=-tdissd(jlev)*sakpp(1:NSPP,jlev)*sdp(:,jlev)       &
     &              /(1.+delt2*tdissd(jlev)*sakpp(1:NSPP,jlev))
       sdt(:,jlev)=zsdt1(:,jlev)+zsdt2(:,jlev)
       zszt1(:,jlev)=-tfrc(jlev)*szp(:,jlev)                            &
     &              /(1.+delt2*tfrc(jlev))
       zszt2(:,jlev)=-tdissz(jlev)*sakpp(1:NSPP,jlev)*szp(:,jlev)       &
     &              /(1.+delt2*tdissz(jlev)*sakpp(1:NSPP,jlev))
       szt(:,jlev)=zszt1(:,jlev)+zszt2(:,jlev)
      enddo

!     no friction on planetary vorticity
!     no diffusion on plavor (planetary vorticity)

      if (mypid == NROOT) then
         spp(1) = 0.0
         spp(2) = 0.0
         zszt1(3,:)=zszt1(3,:)+plavor*tfrc(:)/(1.+delt2*tfrc(:))
         zszt2(3,:)=zszt2(3,:)+plavor*tdissz(:)*sakpp(3,:)              &
     &                               /(1.+delt2*tdissz(:)*sakpp(3,:))
         szt(3,:)=szt(3,:)+plavor*(tfrc(:)/(1.+delt2*tfrc(:))           &
     &                            +tdissz(:)*sakpp(3,:)                 &
     &                            /(1.+delt2*tdissz(:)*sakpp(3,:)))
      endif

!     add temp-tendencies due to momentum diffusion/dissipation
!     (if switched on)

      if(ndheat > 0) call mkdheat(zszt1,zszt2,zsdt1,zsdt2)
!
!     diagnostics of efficiency
!
      if(ndheat > 1) then
       call mpgallsp(zstt2,stt,NLEV)
       zstt(:,:)=zstt(:,:)+zstt2(:,:)
       if (nqspec == 1) call sp2fl(zsq,zqgp,NLEV)
       call sp2fl(zst,ztgp,NLEV)
       call sp2fl(zstt,zdtgp,NLEV)
       if (nqspec == 1) then
          call fc2gp(zqgp,NLON,NLPP*NLEV)
       else
          zqgp(:,:) = 0.0
       endif
       call fc2gp(ztgp,NLON,NLPP*NLEV)
       call fc2gp(zdtgp,NLON,NLPP*NLEV)
       zsum1(:)=0.
       do jlev=1,NLEV
        if (nqspec == 1) zqgp(:,jlev)=zqgp(:,jlev)*psurf/dp(:)
        ztgp(:,jlev)=ct*(ztgp(:,jlev)+t0(jlev))
        zdtgp(:,jlev)=ct*ww*zdtgp(:,jlev)
        zsum1(1)=zsum1(1)+SUM(zdtgp(:,jlev)*zgw(:)                      &
     &                *acpd*(1.+adv*zqgp(:,jlev))*dp(:)/ga*dsigma(jlev) &
     &                ,mask=(zdtgp(:,jlev) >= 0.))
        zsum1(2)=zsum1(2)+SUM(zdtgp(:,jlev)*zgw(:)                      &
     &                *acpd*(1.+adv*zqgp(:,jlev))*dp(:)/ga*dsigma(jlev) &
     &               ,mask=(zdtgp(:,jlev) < 0.))
        zsum1(3)=zsum1(3)+SUM(zdtgp(:,jlev)/ztgp(:,jlev)*zgw(:)         &
     &                *acpd*(1.+adv*zqgp(:,jlev))*dp(:)/ga*dsigma(jlev) &
     &               ,mask=(zdtgp(:,jlev) >= 0.))
        zsum1(4)=zsum1(4)+SUM(zdtgp(:,jlev)/ztgp(:,jlev)*zgw(:)         & 
     &                *acpd*(1.+adv*zqgp(:,jlev))*dp(:)/ga*dsigma(jlev) &
     &               ,mask=(zdtgp(:,jlev) < 0.))
       enddo
       zsum3=SUM(zgw(:))
       call mpsumbcr(zsum1,4)
! PBH new dummy function as compiler does not like this call with a
! scalar aurgument 
       call mpsumbcr1(zsum3)
       zsum1(:)=zsum1(:)/zsum3
       if(mypid == NROOT) then
        ztp=zsum1(1)/zsum1(3)
        ztm=zsum1(2)/zsum1(4)
        write(9,*) zsum1(:),zsum1(1)/zsum1(3),zsum1(2)/zsum1(4)         &
     &            ,(ztp-ztm)/ztp
       endif
       deallocate(zst)
       if (nqspec == 1) deallocate(zsq)
       deallocate(zstt)
       deallocate(zstt2)
       deallocate(ztgp)
       deallocate(zqgp)
       deallocate(zdtgp)
       deallocate(zsum1)
       deallocate(zgw)
      endif
!
!     sponge layer ad the top
!     (relax t(1) to t(2))
!
      if(nsponge > 0) then
       stt(:,1)=dampsp*(stp(:,2)-stp(:,1))
      endif

      szp = szp + delt2 * szt
      stp = stp + delt2 * stt
      sdp = sdp + delt2 * sdt
      if (nqspec == 1) sqp = sqp + delt2 * sqt

      if (nkits == 0) then
         szm = szm + pnu * szp
         stm = stm + pnu * stp
         sdm = sdm + pnu * sdp
         spm = spm + pnu * spp
         if (nqspec == 1) sqm = sqm + pnu * sqp
      endif

      call mpgallsp(sd,sdp,NLEV)
      call mpgallsp(sz,szp,NLEV)
      call mpgallsp(st,stp,NLEV)
      call mpgallsp(sp,spp,   1)
      if (nqspec == 1) call mpgallsp(sq,sqp,NLEV)

!
!     franks diagnostic
!

      if(ndiagsp==1) then
       allocate(ztt(NESP,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       dsp3d(:,1:NLEV,2)=ztt(:,1:NLEV)*ct*ww
       deallocate(ztt)
      endif
      if(ndiaggp==1) then
       call mkdqtgp
       do jlev=1,NLEV
        dgp3d(:,jlev,21)=dqt(:,jlev)*dp(:)*dsigma(jlev)/ga/1000.
       enddo
      endif
!
!     entropy/energy diagnostics
!
      if(nentropy > 0) then
       allocate(ztt(NESP,NLEV))
       allocate(zttgp(NHOR,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       ztt(:,:)=ztt(:,:)*ct*ww
       call sp2fl(ztt,zttgp,NLEV)
       call fc2gp(zttgp,NLON,NLPP*NLEV)
       deallocate(ztt)
       dentropy(:,5)=0.
       do jlev=1,NLEV
        if(nentropy > 2) then
         dentropy(:,5)=dentropy(:,5)                                    &
     &                +zttgp(:,jlev)/dt(:,jlev)                         &
     &                *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
        else
         dentropy(:,5)=dentropy(:,5)                                    &
     &                +zttgp(:,jlev)/dentrot(:,jlev)                    &
     &         *acpd*(1.+adv*dentroq(:,jlev))*dentrop(:)/ga*dsigma(jlev)
        endif
       enddo
       deallocate(zttgp)
      endif
      if(nenergy > 0) then
       allocate(ztt(NESP,NLEV))
       allocate(zttgp(NHOR,NLEV))
       call mpgallsp(ztt,stt,NLEV)
       ztt(:,:)=ztt(:,:)*ct*ww
       call sp2fl(ztt,zttgp,NLEV)
       call fc2gp(zttgp,NLON,NLPP*NLEV)
       deallocate(ztt)
       denergy(:,5)=0.
       do jlev=1,NLEV
        denergy(:,5)=denergy(:,5)                                       &
     &              +zttgp(:,jlev)                                      &
     &              *acpd*(1.+adv*dq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       deallocate(zttgp)
      endif

!
      return
      end subroutine spectrald

!     ===============
!     SUBROUTINE MKDQ
!     ===============

!      subroutine mkdq(psq,pgq,KLEV)
!      use pumamod
!      implicit none
!
!      real psq(NESP,klev),pgq(NHOR,klev)
!
!      real zps(NHOR)
!
!      call sp2fl(psq,pgq,KLEV)
!      call fc2gp(pgq,NLON,NLPP*KLEV)
!      call sp2fl(sp,zps,1)
!      call fc2gp(zps,NLON,NLPP)
!
!      zps(:)=psurf*EXP(zps(:))
!      do jlev=1,KLEV
!       pgq(:,jlev)=pgq(:,jlev)*psurf/zps(:)
!      enddo
!
!      return
!      end subroutine mkdq


!     ===================
!     SUBROUTINE MKSECOND
!     ===================

      subroutine mksecond(ps,poff)
      implicit none
      integer zdays
      real zhour,zmins,zsecs
!
      real ps,poff
      integer ivalues(8)
!
      call date_and_time(values=ivalues)
!
      zdays = real(ivalues(3) - 1)
      zhour = 24.0 * zdays + real(ivalues(5))
      zmins = 60.0 * zhour + real(ivalues(6))
      zsecs = 60.0 * zmins + real(ivalues(7)) + 0.001 * real(ivalues(8))
      ps = zsecs - poff
!
      return
      end

!     ==================
!     SUBROUTINE MKDHEAT
!     ==================

      subroutine mkdheat(zszt1,zszt2,zsdt1,zsdt2)
      use pumamod
      implicit none
      integer jlev
!
      real zszt1(NSPP,NLEV),zszt2(NSPP,NLEV)
      real zsdt1(NSPP,NLEV),zsdt2(NSPP,NLEV)
!
      real zsd(NESP,NLEV),zsz(NESP,NLEV),zsq(NESP,NLEV)
      real zsdp(NSPP,NLEV),zszp(NSPP,NLEV),zsqp(NSPP,NLEV)
      real zu(NHOR,NLEV),zun(NHOR,NLEV)
      real zv(NHOR,NLEV),zvn(NHOR,NLEV)
      real zq(NHOR,NLEV)
!
      real zdtdt(NHOR,NLEV),zdekin(NHOR,NLEV)
      real zsde(NSPP,NLEV),zsdef(NESP,NLEV)
      real zstt1(NSPP,NLEV),zstf1(NESP,NLEV)
      real zstt2(NSPP,NLEV),zstf2(NESP,NLEV)
!
      zsdp(:,:)=sdp(:,:)
      zszp(:,:)=szp(:,:)
      if (nqspec == 1) zsqp(:,:)=sqp(:,:)
      call mpgallsp(zsd,zsdp,NLEV)
      call mpgallsp(zsz,zszp,NLEV)
      if (nqspec == 1) call mpgallsp(zsq,zsqp,NLEV)
      call dv2uv(zsd,zsz,zu,zv)
      if (nqspec == 1) call sp2fl(zsq,zq,NLEV)
      call fc2gp(zu,NLON,NLPP*NLEV)
      call fc2gp(zv,NLON,NLPP*NLEV)
      call fc2gp(zq,NLON,NLPP*NLEV)
!
      zsdp(:,:)=sdp(:,:)+zsdt1(:,:)*delt2
      zszp(:,:)=szp(:,:)+zszt1(:,:)*delt2
      call mpgallsp(zsd,zsdp,NLEV)
      call mpgallsp(zsz,zszp,NLEV)
      call dv2uv(zsd,zsz,zun,zvn)
      call fc2gp(zun,NLON,NLPP*NLEV)
      call fc2gp(zvn,NLON,NLPP*NLEV)
!
      do jlev = 1 , NLEV
       zu(:,jlev)=cv*zu(:,jlev)*SQRT(rcsq(:))
       zv(:,jlev)=cv*zv(:,jlev)*SQRT(rcsq(:))
       zun(:,jlev)=cv*zun(:,jlev)*SQRT(rcsq(:))
       zvn(:,jlev)=cv*zvn(:,jlev)*SQRT(rcsq(:))
       zq(:,jlev)=zq(:,jlev)*psurf/dp(:)

       zdtdt(:,jlev)=-(zun(:,jlev)*zun(:,jlev)                          &
     &                -zu(:,jlev)*zu(:,jlev)                            &
     &                +zvn(:,jlev)*zvn(:,jlev)                          &
     &                -zv(:,jlev)*zv(:,jlev))/deltsec2                  &
     &               *0.5/acpd/(1.+adv*zq(:,jlev))
      enddo
!
      zdtdt(:,:)=zdtdt(:,:)/ct/ww
      call gp2fc(zdtdt,NLON,NLPP*NLEV)
      do jlev=1,NLEV
       call fc2sp(zdtdt(:,jlev),zstf1(:,jlev))
      enddo
      call mpsumsc(zstf1,zstt1,NLEV)
!
      zsdp(:,:)=sdp(:,:)+zsdt2(:,:)*delt2
      zszp(:,:)=szp(:,:)+zszt2(:,:)*delt2
      call mpgallsp(zsd,zsdp,NLEV)
      call mpgallsp(zsz,zszp,NLEV)
      call dv2uv(zsd,zsz,zun,zvn)
      call fc2gp(zun,NLON,NLPP*NLEV)
      call fc2gp(zvn,NLON,NLPP*NLEV)
!
      do jlev = 1 , NLEV
       zun(:,jlev)=cv*zun(:,jlev)*SQRT(rcsq(:))
       zvn(:,jlev)=cv*zvn(:,jlev)*SQRT(rcsq(:))
       zdekin(:,jlev)=(zun(:,jlev)*zun(:,jlev)                          &
     &                -zu(:,jlev)*zu(:,jlev)                            &
     &                +zvn(:,jlev)*zvn(:,jlev)                          &
     &                -zv(:,jlev)*zv(:,jlev))/deltsec2                  &
     &               *dp(:)/ga*dsigma(jlev)   
      enddo
      call gp2fc(zdekin,NLON,NLPP*NLEV)
      do jlev=1,NLEV
       call fc2sp(zdekin(:,jlev),zsdef(:,jlev))
      enddo
      call mpsumsc(zsdef,zsde,NLEV)
      call mpgallsp(zsdef,zsde,NLEV)
      zsdef(2:NESP,:)=0.
      call sp2fl(zsdef,zdekin,NLEV)
      call fc2gp(zdekin,NLON,NLPP*NLEV)
      do jlev=1,NLEV
       zdtdt(:,jlev)=-zdekin(:,jlev)                                    &
     &           *0.5/acpd/(1.+adv*zq(:,jlev))/dp(:)*ga/dsigma(jlev)
      enddo
      zdtdt(:,:)=zdtdt(:,:)/ct/ww
      call gp2fc(zdtdt,NLON,NLPP*NLEV)
      do jlev=1,NLEV
       call fc2sp(zdtdt(:,jlev),zstf2(:,jlev))
      enddo
      call mpsumsc(zstf2,zstt2,NLEV)
!
!     energy diagnostics
!
      if(nenergy > 0) then
       call mpgallsp(zstf1,zstt1,NLEV)       
       zstf1(:,:)=zstf1(:,:)*ct*ww
       call sp2fl(zstf1,zdtdt,NLEV)
       call fc2gp(zdtdt,NLON,NLPP*NLEV)
       denergy(:,27)=0.
       do jlev=1,NLEV
        denergy(:,27)=denergy(:,27)                                     &
     &               +zdtdt(:,jlev)                                     &
     &               *acpd*(1.+adv*zq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo
       call mpgallsp(zstf2,zstt2,NLEV)
       zstf2(:,:)=zstf2(:,:)*ct*ww
       call sp2fl(zstf2,zdtdt,NLEV)
       call fc2gp(zdtdt,NLON,NLPP*NLEV)
       denergy(:,26)=0.
       do jlev=1,NLEV
        denergy(:,26)=denergy(:,26)                                     &
     &               +zdtdt(:,jlev)                                     &
     &               *acpd*(1.+adv*zq(:,jlev))*dp(:)/ga*dsigma(jlev)
       enddo

      endif
!
      stt(:,:)=stt(:,:)+zstt1(:,:)+zstt2(:,:)
!
      return
      end subroutine mkdheat

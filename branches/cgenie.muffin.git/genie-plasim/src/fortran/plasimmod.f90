!
!     **********************************************************
!     * PUMA 2.0 * Portable University Model of the Atmosphere *
!     **********************************************************

!     *****************
!     * Frank Lunkeit *       University of Hamburg
!     * Edilbert Kirk *      Meteorological Institute
!     *****************   Dept.: Theoretical Meteorology
!                             Head: Klaus Fraedrich

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

      module pumamod

!     ****************************************************************
!     * The module resmod (used to) define all resolution params    *
!     * NPRO_ATM, NLAT_ATM, NLEV_ATM                                 *
!     * If MPI (Message Passing Interface) is to be used it will     *
!     * also include a 'use mpi' statement                           *
!     * This is not subsumed into geniemod which deals with global   *
!     * parameters in the coupled model.
!     ****************************************************************

      use geniemod
      implicit none

!     ****************************************************************
!     * The number of processes for processing on parallel machines  *
!     * It must be a power of 2 e.g. (1, 2, 4, 8, ... NLAT)          *
!     ****************************************************************

      integer NPRO
      parameter(NPRO = NPRO_ATM)          ! Number of processes (CPUs)

!     ****************************************************************
!     * Set the horizontal resolution by specifying the number of    *
!     * latitudes. Use only values, listed below (FFT restrictions)  *
!     ****************************************************************

      integer NLAT
      parameter(NLAT = NLAT_ATM)                !  Number of latitudes

!     currently allowed values:  2,  4,  32,  48,  64, 128,  192,  256
!     corresponding truncation: T1, T2, T21, T31, T42, T85, T127, T170

!     ****************************************************************
!     * Set the vertical resolution, a minimum of 5 is recommended   *
!     ****************************************************************

      integer NLEV
      parameter(NLEV = NLEV_ATM)                    ! Number of levels

!     ****************************************************************
!     * Don't touch the following parameter definitions !            *
!     ****************************************************************

!     ********************
!     * Global Constants *
!     ********************
!
      integer :: NTRACE,NLON,NTRU,NLPP,NHOR,NUGP,NPGP,NLEM,NLEP,NLSQ
      integer :: NTP1,NRSP,NCSP,NSPP,NESP,NVCT,NZOM,NROOT
      parameter(NTRACE = 1)                ! # of tracers 1st. reserved for q
      parameter(NLON = NLAT + NLAT)        ! Number of longitudes
      parameter(NTRU = (NLON-1) / 3)       ! Triangular truncation
      parameter(NLPP = NLAT / NPRO)        ! Latitudes per process
      parameter(NHOR = NLON * NLPP)        ! Horizontal part
      parameter(NUGP = NLON * NLAT)        ! Number of gridpoints
      parameter(NPGP = NLON * NLAT / 2)    ! Dimension of packed fields
      parameter(NLEM = NLEV - 1)           ! Levels - 1
      parameter(NLEP = NLEV + 1)           ! Levels + 1
      parameter(NLSQ = NLEV * NLEV)        ! Levels squared
      parameter(NTP1 = NTRU + 1)           ! Truncation + 1
      parameter(NRSP =(NTRU+1)*(NTRU+2))   ! No of real global    modes
      parameter(NCSP = NRSP / 2)           ! No of complex global modes
      parameter(NSPP = (NRSP+NPRO-1)/NPRO) ! Modes per process
      parameter(NESP = NSPP * NPRO)        ! Dim of spectral fields
      parameter(NVCT = 2 * (NLEV+1))       ! Dim of Vert. Coord. Tab
      parameter(NZOM  = 2 * NTP1)          ! Dim for zonal mean diagnostics
      parameter(NROOT = 0)                 ! Master node

      real*8 EZ,PI,TWOPI
      real RV,ALV,ALS,ACPV,TMELT,TMELT_CO2
      parameter(EZ     = 1.63299310207)  ! ez = 1 / sqrt(3/8)
      parameter(PI     = 3.14159265359)  ! Pi
      parameter(TWOPI  = PI + PI)          ! 2 Pi
      parameter(RV     = 461.51)           ! Gas constant for water vapour
      parameter(ALV    = 2.5008E6)         ! Latent heat of vaporization
      parameter(ALS    = 2.8345E6)         ! Latent heat of sublimation
      parameter(ACPV   = 1870.)            ! Specific heat for water vapour
      parameter(TMELT  = 273.16)           ! Melting point (H2O)
      parameter(TMELT_CO2 = 148.0)         ! Melting point (CO2) - for Mars

!     ***************
!     * Date & Time *
!     ***************

      integer :: nstep           =       0 ! current timestep
      integer :: nstep1          =       0 ! start timestep for this run
      integer :: mstep           =       0 ! timestep # in current month
      integer :: n_start_year    =    0001 ! start year
      integer :: n_start_month   =      01 ! start month
      integer :: n_start_step    =       0 ! start step since 1-Jan-0000
      integer :: n_days_per_month=      30 ! set to   0 for real calendar
      integer :: n_days_per_year =     360 ! set to 365 for real calendar
      integer :: mpstep          =       0 ! minutes/timestep = 1day/ntspd
      integer :: ntspd           =      32 ! number of timesteps per day
      integer :: nwpd            =       1 ! number of writes per day
      integer :: ndatim(6)       =      -1 ! date & time array
      real    :: tmstart         =     0.0 ! start of run

      

!     **************************
!     * Global Integer Scalars *
!     **************************

      integer :: ngenie    = 1  ! pass genie variables to plasim (1) or one way couple (0)
      integer :: kick     =  1  ! add noise for kick > 0
      integer :: noutput  =  1  ! master switch for output: 0=no output
      integer :: nafter   =  0  ! write data interval: 0 = once per day
      integer :: ncoeff   =  0  ! number of modes to print
      integer :: ndiag    =  0  ! write diagnostics interval 0 = every 10th. day
      integer :: ndebug   =  0  ! activates additional printouts
      integer :: nexp     =  0  ! experiment number
      integer :: nexper   =  0  ! 1: dipole experiment
      integer :: sellon   =  1  ! index of longitude for column mode
      integer :: nkits    =  3  ! number of initial timesteps
      integer :: nrestart =  0  ! 1 for true, 0 for false
      integer :: nrad     =  1  ! switches radiation off/on  1/0
      integer :: nflux    =  1  ! vertical diffusion 1/0
      integer :: nadv     =  1  ! advection 1/0=(y/n)
      integer :: nhordif  =  1  ! horizontal diffusion 1/0=(y/n)
      integer :: neqsig   =  0  ! equidistant sigma levels (1/0)=(y/n)
      integer :: nprint   =  0  ! comprehensive print out (only for checks!)
      integer :: nprhor   =  0  ! grid point for print out (only for checks!)
      integer :: npacksp  =  0  ! pack spectral fields on output
      integer :: npackgp  =  0  ! pack gridpoint fields on output
      integer :: naccuout =  0  ! accumulation counter for diagnistics
      integer :: ndiaggp  =  0  ! switch for frank's gp-diagnostic arrays
      integer :: ndiagsp  =  0  ! switch for frank's sp-diagnostic arrays
      integer :: ndiagcf  =  0  ! switch for cloud forcing diagnostic
      integer :: ndiaggp2d=  0  ! number of additional 2-d gp-diagnostic arrays
      integer :: ndiaggp3d=  0  ! number of additional 3-d gp-diagnostic arrays
      integer :: ndiagsp2d=  0  ! number of additional 2-d sp-diagnostic arrays
      integer :: ndiagsp3d=  0  ! number of additional 3-d sp-diagnostic arrays
      integer :: nhdiff   = 15  ! critical wavenumber for horizontal diffusion
      integer :: ntime    =  0  ! switch for time use diagnostics
      integer :: nperpetual = 0 ! radiation day for perpetual integration
      integer :: n_sea_points=0 ! number of sea points on grid
      integer :: nentropy = 0   ! switch for entropy diagnostics
      integer :: nenergy  = 0   ! switch for energy diagnostics
      integer :: ndheat   = 1   ! switch for heating due to momentum dissipation
      integer :: nsela    = 0   ! enable (1) or disable (0) Semi Lagrangian Advection
      integer :: nsponge  = 0   ! switch for top sponge layer
      integer :: nqspec   = 1   ! 1: spectral q   0: gridpoint q (semi-Langrangian)

      integer :: nout3D    = 1   ! 3D netcdf outputs, 1=yes (namelist parameter)
      integer :: noutseas  = 1   ! seasonal netcdf outputs, 1=yes (namelist parameter)
      integer :: noutmnth  = 1   ! monthly netcdf outputs, 1=yes (namelist parameter)

!     ***********************
!     * Global Real Scalars *
!     ***********************

      real :: plavor=     EZ  ! planetary vorticity
      real :: dawn  =     0.0 ! angle threshold for solar radiation
      real :: deltsec  =  0.0 ! timestep [sec]
      real :: deltsec2 =  0.0 ! timestep [sec] * 2
      real :: delt            ! deltsec * Omega (ww)
      real :: delt2           ! 2 * delt
      real :: dtep  =     0.0
      real :: dtns  =     0.0
      real :: dtrop = 12000.0
      real :: dttrp =     2.0
      real :: tgr   =   288.0  ! Temperature ground in mean profile
      real :: psurf =101100.0  ! global mean surface pressure
      real :: time0 =     0.0  ! start time (for performance estimates)
      real :: co2   =   280.0  ! atm. co2 concentration for radiation (ppmv) i.e. CO2eff
      real :: co2veg  = 280.0  ! atm. co2 concentration for vegetationc(ppmv)
      real :: umax  =     0.0  ! diagnostic U max
      real :: t2mean=     0.0  ! diagnostic T2m mean
      real :: precip=     0.0  ! diagnostic precipitation mean
      real :: evap  =     0.0  ! diagnostic evaporation
      real :: olr   =     0.0  ! Outgoing longwave radiation
      real :: dampsp=     0.0  ! damping time (days) for sponge layer

!     **************************
!     * Global Spectral Arrays *
!     **************************

      real ::  sd(NESP,NLEV) = 0.0 ! Spectral Divergence
      real ::  st(NESP,NLEV) = 0.0 ! Spectral Temperature
      real ::  sz(NESP,NLEV) = 0.0 ! Spectral Vorticity
      real ::  sq(NESP,NLEV) = 0.0 ! Spectral Specific Humidity
      real ::  sp(NESP)      = 0.0 ! Spectral Pressure (ln Ps)
      real ::  so(NESP)      = 0.0 ! Spectral Orography
      real ::  sr(NESP,NLEV) = 0.0 ! Spectral Restoration Temperature

      real :: sdp(NSPP,NLEV) = 0.0 ! Spectral Divergence  Partial
      real :: stp(NSPP,NLEV) = 0.0 ! Spectral Temperature Partial
      real :: szp(NSPP,NLEV) = 0.0 ! Spectral Vorticity   Partial
      real :: sqp(NSPP,NLEV) = 0.0 ! Spectral S.Humidity  Partial
      real :: spp(NSPP)      = 0.0 ! Spectral Pressure    Partial
      real :: sop(NSPP)      = 0.0 ! Spectral Orography   Partial
      real :: srp(NSPP,NLEV) = 0.0 ! Spectral Restoration Partial

      real :: sdt(NSPP,NLEV) = 0.0 ! Spectral Divergence  Tendency
      real :: stt(NSPP,NLEV) = 0.0 ! Spectral Temperature Tendency
      real :: szt(NSPP,NLEV) = 0.0 ! Spectral Vorticity   Tendency
      real :: sqt(NSPP,NLEV) = 0.0 ! Spectral S.Humidity  Tendency
      real :: spt(NSPP)      = 0.0 ! Spectral Pressure    Tendency

      real :: sdm(NSPP,NLEV) = 0.0 ! Spectral Divergence  Minus
      real :: stm(NSPP,NLEV) = 0.0 ! Spectral Temperature Minus
      real :: szm(NSPP,NLEV) = 0.0 ! Spectral Vorticity   Minus
      real :: sqm(NSPP,NLEV) = 0.0 ! Spectral S.Humidity  Minus
      real :: spm(NSPP)      = 0.0 ! Spectral Pressure    Minus

      real :: sak(NESP,NLEV)   = 0.0 ! horizontal diffusion
      real :: sakpp(NSPP,NLEV) = 0.0 ! horizontal diffusion partial
      real :: sqout(NESP,NLEV) = 0.0 ! specific humidity for output
      real :: spnorm(NESP)     = 0.0 ! Factors for output normalization

      integer :: nindex(NESP) = NTRU ! Holds wavenumber
      integer :: nscatsp(NPRO)= NSPP ! Used for reduce_scatter op
      integer :: ndel(NLEV)   =    2 ! ndel for horizontal diffusion

!     ************************************************
!     * Global Gridpoint Arrays (un-dimensionalized) *
!     ************************************************

      real :: gd(NHOR,NLEV)   = 0. ! divergence
      real :: gt(NHOR,NLEV)   = 0. ! temperature (-t0)
      real :: gz(NHOR,NLEV)   = 0. ! absolut vorticity
      real :: gq(NHOR,NLEV)   = 0. ! spec. humidity
      real :: gu(NHOR,NLEV)   = 0. ! zonal wind (*cos(phi))
      real :: gv(NHOR,NLEV)   = 0. ! meridional wind (*cos(phi))
      real :: gtdt(NHOR,NLEV) = 0. ! t-tendency
      real :: gqdt(NHOR,NLEV) = 0. ! q-tendency
      real :: gudt(NHOR,NLEV) = 0. ! u-tendency
      real :: gvdt(NHOR,NLEV) = 0. ! v-tendency
      real :: gp(NHOR)        = 0. ! surface pressure or ln(ps)
      real :: gpj(NHOR)       = 0. ! dln(ps)/dphi

      real :: rcsq(NHOR)      = 0. ! 1/cos(phi)**2

!     *********************************************
!     * Global Gridpoint Arrays (dimensionalized) *
!     *********************************************

      real :: dt(NHOR,NLEP)   = 0.     ! temperature 
      real :: dq(NHOR,NLEP)   = 0.     ! spec. humidity
      real :: du(NHOR,NLEP)   = 0.     ! zonal wind (*cos(phi))
      real :: dv(NHOR,NLEP)   = 0.     ! meridional wind (*cos(phi))
      real :: dp(NHOR)        = 0.     ! surface pressure
      real :: dqsat(NHOR,NLEP)= 0.     ! saturation humidity
      real :: dqt(NHOR,NLEP)  = 0.     ! adiabatic q-tendencies (for eg kuo)
      real :: dcc(NHOR,NLEP)  = 0.     ! cloud cover
      real :: dql(NHOR,NLEP)  = 0.     ! Liquid water content
      real :: dw(NHOR,NLEV)   = 0.     ! vertical velocity (dp/dt)
      real :: dtdt(NHOR,NLEP) = 0.     ! t-tendency
      real :: dqdt(NHOR,NLEP) = 0.     ! q-tendency
      real :: dudt(NHOR,NLEP) = 0.     ! u-tendency
      real :: dvdt(NHOR,NLEP) = 0.     ! v-tendency
      real :: dp0(NHOR)       = 0.     ! surface pressure at time t
      real :: du0(NHOR,NLEP)  = 0.     ! zonal wind at time t 
      real :: dv0(NHOR,NLEP)  = 0.     ! meridional wind at time t 
      real :: dtrace(NLON,NLAT,NLEV,NTRACE) = 1.0 ! Trace array
      real :: ddummy(NHOR)    = 0.     ! dummy gridpoint array for debugging (output)

!     *************
!     * Radiation *
!     *************

      real :: dalb(NHOR)               ! albedo
      real :: dswfl(NHOR,NLEP)         ! net solar radiation
      real :: dlwfl(NHOR,NLEP)         ! net thermal radiation
      real :: dflux(NHOR,NLEP)         ! net radiation (SW + LW)
      real :: dfu(NHOR,NLEP)           ! solar radiation upward
      real :: dfd(NHOR,NLEP)           ! solar radiation downward
      real :: dftu(NHOR,NLEP)          ! thermal radiation upward
      real :: dftd(NHOR,NLEP)          ! thermal radiation downward

!     ***********
!     * SURFACE *
!     ***********

      real :: drhs(NHOR)  = 0.  ! surface wetness
      real :: dls(NHOR)   = 1.  ! land(1)/sea(0) mask
      real :: dz0(NHOR)   = 0.  ! rougthness length
      real :: diced(NHOR) = 0.  ! ice thickness
      real :: dicec(NHOR) = 0.  ! ice cover
      real :: dtaux(NHOR) = 0.  ! x-surface wind stress
      real :: dtauy(NHOR) = 0.  ! y-surface wind stress
      real :: dust3(NHOR) = 0.  ! u-star**3 (needed eg. for coupling)
      real :: dshfl(NHOR) = 0.  ! surface sensible heat flx
      real :: dlhfl(NHOR) = 0.  ! surface latent heat flx
      real :: devap(NHOR) = 0.  ! surface evaporation
      real :: dtsa(NHOR)  = 273.15  ! surface air temperature
      real :: dmld(NHOR)  = 0.  ! mixed-layer depth (output from ocean)
!
      real dshdt(NHOR),dlhdt(NHOR)
!coefficient for genie coupling
      real dlhcoeff(NHOR)
      real dshcoeff(NHOR)

!     *********
!     * WATER *
!     *********

      real dprc(NHOR)       ! Convective Precip   (m/s)
      real dprl(NHOR)       ! Large Scale Precip  (m/s)
      real dprs(NHOR)       ! Snow Fall           (m/s)
      real dqvi(NHOR)       ! vertical integrated specific humidity (kg/m**2)

!     *********
!     * BIOME *
!     *********

      real :: dcsoil(NHOR)   =0.0  ! organic carbon stored in soil
      real :: dcveg(NHOR)    =0.0  ! organic carbon stored in biomass
      real :: dforest(NHOR) = 0.5  ! forest cover (fract.)
      real :: dgpp(NHOR)    = 0.0  ! gross primary production (kg C /m2 /s)
      real :: dgppl(NHOR)   = 0.0  ! light limited gpp (kg C /m2 /s)
      real :: dgppw(NHOR)   = 0.0  ! water limited gpp (kg C /m2 /s)
      real :: dlai(NHOR)    = 0.0  ! leaf area index (integer value)
      real :: dlitter(NHOR) = 0.0  ! litterfall (kg C /m2 /s)
      real :: dnogrow(NHOR) = 0.0  ! no growth allocation (kg C /m2 /s)
      real :: dnpp(NHOR)    = 0.0  ! net primary production (kg C /m2 /s)
      real :: dres(NHOR)    = 0.0  ! heterotrophic respiration (kg C /m2 /s)
      real :: dveg(NHOR)    = 0.5  ! vegetation cover (fract.)
      real :: dwloss(NHOR)   =0.0  ! water loss
      real :: dwmax(NHOR)   = 0.0  ! field capacity (m)

!     ********
!     * SOIL *
!     ********

      real :: dwatc(NHOR)   = 0.  ! soil wetness (m)
      real :: drunoff(NHOR) = 0.  ! surface runoff (m/s)
      real :: dsnow(NHOR)   = 0.  ! snow depth (m)
      real :: dsmelt(NHOR)  = 0.  ! snow melt (m/s water eq.)
      real :: dsndch(NHOR)  = 0.  ! snow depth change (m/s water eq.)
      real :: dtsoil(NHOR)  = 0.  ! soil temperature uppermost level (K)
      real :: dtd2(NHOR)    = 0.  ! soil temperature level 2 (K)
      real :: dtd3(NHOR)    = 0.  ! soil temperature level 3 (K)
      real :: dtd4(NHOR)    = 0.  ! soil temperature level 4 (K)
      real :: dtd5(NHOR)    = 0.  ! soil temperature lowermost level (K)
      real :: dglac(NHOR)   = 0.  ! glacier mask (0.,1.)

!     *********************
!     * Diagnostic Arrays *
!     *********************

      integer :: ndl(NLEV) = 0

      real :: csu(NLAT,NLEV),csv(NLAT,NLEV),cst(NLAT,NLEV),csm(NLAT,NLEV)
      real :: ccc(NLAT,NLEV)
      real :: span(NESP)

      real, allocatable :: dgp2d(:,:),dsp2d(:,:)     ! 2-d diagnostics
      real, allocatable :: dgp3d(:,:,:),dsp3d(:,:,:) ! 3-d diagnostics
      real, allocatable :: dclforc(:,:)  ! cloud forcing diagnostics
      real, allocatable :: dentropy(:,:) ! entropy diagnostics
      real, allocatable :: denergy(:,:)  ! energy diagnostics
      real, allocatable :: dentrop(:)    ! ps for entropy diagnostics
      real, allocatable :: dentrot(:,:)  ! t for entropy diagnostics
      real, allocatable :: dentroq(:,:)  ! q for entropy diagnostics

!
!     accumulated output
!

      real :: aevap(NHOR) = 0. ! acculumated evaporation
      real :: aprl(NHOR)  = 0. ! acculumated lage scale precip.
      real :: aprc(NHOR)  = 0. ! acculumated convective precip.
      real :: aprs(NHOR)  = 0. ! acculumated snow fall
      real :: ashfl(NHOR) = 0. ! acculumated sensible heat flux
      real :: alhfl(NHOR) = 0. ! acculumated latent heat flux
      real :: aroff(NHOR) = 0. ! acculumated surface runoff
      real :: asmelt(NHOR)= 0. ! acculumated snow melt
      real :: asndch(NHOR)= 0. ! acculumated snow depth change
      real :: acc(NHOR)   = 0. ! acculumated total cloud cover
      real :: assol(NHOR) = 0. ! acculumated surface solar radiation
      real :: asthr(NHOR) = 0. ! acculumated surface thermal radiation
      real :: atsol(NHOR) = 0. ! acculumated top solar radiation
      real :: atthr(NHOR) = 0. ! acculumated top thermal radiation
      real :: assolu(NHOR)= 0. ! acculumated surface solar radiation upward
      real :: asthru(NHOR)= 0. ! acculumated surface thermal radiation upward
      real :: atsolu(NHOR)= 0. ! acculumated top solar radiation upward
      real :: ataux(NHOR) = 0. ! acculumated zonal wind stress
      real :: atauy(NHOR) = 0. ! acculumated meridional wind stress
      real :: aqvi(NHOR)  = 0. ! acculumated vertical integrated q
      real :: atsa(NHOR)  = 0. ! accumulated surface air temperature
      real :: ats0(NHOR)  = 0. ! accumulated surface temperature

      real :: anpp(NHOR)    = 0.0
      real :: agpp(NHOR)    = 0.0
      real :: agppl(NHOR)   = 0.0
      real :: agppw(NHOR)   = 0.0
      real :: anogrow(NHOR) = 0.0
      real :: aresh(NHOR)   = 0.0
      real :: alitter(NHOR) = 0.0
      real :: awloss(NHOR)  = 0.0

! PBH 
      real :: atsoil(NHOR)    = 0.0
      real :: awatc(NHOR)     = 0.0
      real :: aveg(NHOR)      = 0.0
      real :: acveg(NHOR)     = 0.0
      real :: acsoil(NHOR)    = 0.0
      real :: aforest(NHOR)   = 0.0
      real :: awmax(NHOR)     = 0.0
      real :: alai(NHOR)      = 0.0
      real :: az0(NHOR)       = 0.0
      real :: aalb(NHOR)      = 0.0
      real :: aicec(NHOR)     = 0.0
      real :: aiced(NHOR)     = 0.0
      real :: asnow(NHOR)     = 0.0
      real :: ap(NHOR)        = 0.0
      real :: ahfls(NHOR)     = 0.0
! seasonal output
      real :: apr_s(NHOR,4)   = 0.0
      real :: apr2_s(NHOR,4)  = 0.0
      real :: atsa_s(NHOR,4)  = 0.0
      real :: atsa2_s(NHOR,4) = 0.0
      real :: acc_s(NHOR,4)   = 0.0
      real :: aevap_s(NHOR,4)   = 0.0
      real :: asst_s(NHOR,4)   = 0.0
      real :: asia_s(NHOR,4)   = 0.0
      real :: assol_s(NHOR,4)   = 0.0  !incoming solar
      real :: asthr_s(NHOR,4)   = 0.0  !incoming thermal
      real :: asu_s(NHOR,4)   = 0.0    !surface u wind speed
      real :: asv_s(NHOR,4)   = 0.0    !surfave v wind speed
      real :: ataux_s(NHOR,4)   = 0.0    !surface u wind stress
      real :: atauy_s(NHOR,4)   = 0.0    !surfave v wind stress
!monthly output
      real :: ataux_m(NHOR,12)   = 0.0    !surface u wind stress
      real :: atauy_m(NHOR,12)   = 0.0    !surfave v wind stress
! seasonal 3D output
      real :: a3u_s(NHOR,NLEV,4) !uspeed
      real :: a3v_s(NHOR,NLEV,4) !vspeed

      real :: fixcveg(NHOR)
      real :: fixcsoil(NHOR)

!annual scalar lutput
      real :: annsat = 0.0 !global annual average SAT


!debug (for averaging ddummy)
      real :: adummy(NHOR)

!     *******************
!     * Latitude Arrays *
!     *******************

      character(len=3) chlat(NLAT)
      real (kind=8) :: sid(NLAT)     ! sin(phi)
      real (kind=8) :: gwd(NLAT)     ! Gaussian weights
      real :: csq(NLAT)              ! cos(phi)**2
      real :: cola(NLAT)             ! cos(phi)
      real :: rcs(NLAT)              ! 1 / cos(phi)
      real :: deglat(NLPP)           ! latitude in degrees

!     ****************
!     * Level Arrays *
!     ****************

      real :: tdissd(NLEV)  =  0.20 ! diffusion time scale for divergence [days]
      real :: tdissz(NLEV)  =  1.10 ! diffusion time scale for vorticity [days]
      real :: tdisst(NLEV)  =  5.60 ! diffusion time scale for temperature [days]
      real :: tdissq(NLEV)  =  0.1  ! diffusion time scale for sp. humidity [days]
      real :: restim(NLEV)  =   0.0
      real :: t0(NLEV)      = 250.0
      real :: tfrc(NLEV)    =   0.0
      real :: sigh(NLEV)    =   0.0
      real damp(NLEV)
      real dsigma(NLEV)
      real rdsig(NLEV)
      real sigma(NLEV)
      real sigmah(NLEV)
      real t01s2(NLEV)
      real tkp(NLEV)
      real c(NLEV,NLEV)
      real g(NLEV,NLEV)
      real tau(NLEV,NLEV)
      real bm1(NLEV,NLEV,NTRU)

!     ******************
!     * Parallel Stuff *
!     ******************

      integer :: mpinfo  = 0
      integer :: mypid   = 0
      integer :: myworld = 0
      integer :: nproc   = NPRO
      character (len=40) :: ympname(NPRO)

!     **********************************************
!     * version identifier (to be set in puma.f90) *
!     * will be printed in puma sub *prolog*       *
!     **********************************************

      character(len=80) :: pumaversion = 'Version 16 Revision 4'

!     ******************************
!     * Planet dependent variables *
!     ******************************

      real :: AKAP_EARTH,AKAP_MARS,ALR_EARTH,ALR_MARS,GA_EARTH,GA_MARS
      real :: GASCON_EARTH,GASCON_MARS,PLARAD_EARTH,PLARAD_MARS
      real :: PNU_EARTH,PNU_MARS,SID_DAY_EARTH,SID_DAY_MARS
      real :: SOL_DAY_EARTH,SOL_DAY_MARS,RA1_LIQUID,RA2_LIQUID,RA4_LIQUID
      real :: RA1_ICE,RA2_ICE,RA4_ICE, SOLCON_EARTH,SOLCON_MARS
      real :: ECCEN_EARTH,ECCEN_MARS,OBLIQ_EARTH,OBLIQ_MARS
      real :: LOPER_EARTH,LOPER_MARS
      parameter(AKAP_EARTH   = 0.286 )      ! Kappa Earth
      parameter(AKAP_MARS    = 0.2273)      ! Kappa Mars
      parameter(ALR_EARTH    = 0.0065)      ! Lapse rate Earth
      parameter(ALR_MARS     = 0.0025)      ! Lapse rate Mars
      parameter(GA_EARTH     = 9.81)        ! Gravity Earth
      parameter(GA_MARS      = 3.74)        ! Gravity Mars
      parameter(GASCON_EARTH = 287.0)       ! Gas constant for dry air on Earth
      parameter(GASCON_MARS  = 188.9)       ! Gas constant for dry air on Mars 
      parameter(PLARAD_EARTH = 6370000.0)   ! Earth radius !matched to GENIE
      parameter(PLARAD_MARS  = 3397000.0)   ! Mars radius
      parameter(PNU_EARTH    = 0.10)        ! Time filter Earth
      parameter(PNU_MARS     = 0.15)        ! Time filter Mars
      parameter(SID_DAY_EARTH= 86164.)      ! Siderial day Earth 23h 56m 04s
      parameter(SID_DAY_MARS = 88642.)      ! Siderial day Mars  24h 37m 22s
      parameter(SOL_DAY_EARTH= 86400.)      ! Solar day Earth    24h 00m 00s
      parameter(SOL_DAY_MARS = 88775.)      ! Solar day Mars     24h 39m 35s
      parameter(RA1_LIQUID   = 610.78)      ! Parameter for Magnus-Teten-Formula
      parameter(RA2_LIQUID   =  17.2693882) ! for saturation vapor pressure
      parameter(RA4_LIQUID   =  35.86)      ! over liquid water (used on Earth)
      parameter(RA1_ICE      = 610.66)      ! Parameter for Magnus-Teten-Formula
      parameter(RA2_ICE      =  21.875)     ! for saturation vapor pressure
      parameter(RA4_ICE      =   7.65)      ! over ice (used on Mars)
      parameter(SOLCON_EARTH = 1365.0)      ! Solar constant [W/m2]
      parameter(SOLCON_MARS  =  595.0)      ! Solar constant [W/m2]
      parameter(ECCEN_EARTH  = 0.016715)    ! Eccentricity (AMIP-II value)
      parameter(ECCEN_MARS   = 0.09341233)  ! Eccentricity Mars
      parameter(OBLIQ_EARTH  = 23.441)      ! Obliquity [deg] (AMIP-II)
      parameter(OBLIQ_MARS   = 25.19)       ! Obliquity [deg] Mars
      parameter(LOPER_EARTH  = 102.7)       ! Longitude of perihelion Earth
      parameter(LOPER_MARS   = 336.04084)   ! Longitude of perihelion Mars

      character(len=32) :: yplanet = "Earth"

      integer :: mars = 0              ! Global switch for planet Mars
      integer :: nfixorb = 0           ! Global switch to fix orbit

      real :: akap   = AKAP_EARTH      ! Kappa
      real :: alr    = ALR_EARTH       ! Lapse rate
      real :: ga     = GA_EARTH        ! Gravity
      real :: gascon = GASCON_EARTH    ! Gas constant for dry air
      real :: plarad = PLARAD_EARTH    ! Planet radius
      real :: pnu    = PNU_EARTH       ! Time filter
      real :: siderial_day = SID_DAY_EARTH ! Length of siderial day [sec]
      real :: solar_day    = SOL_DAY_EARTH ! Length of solar day [sec]
      real :: ww     = 0.0             ! Omega used for scaling
      real :: ra1    = RA1_LIQUID      !
      real :: ra2    = RA2_LIQUID      !
      real :: ra4    = RA4_LIQUID      !
      real :: acpd   = 0.0             ! acpd = gascon / akap ! Specific heat for dry air
      real :: adv    = 0.0             ! acpv / acpd - 1.0
      real :: cv     = 0.0             ! cv = plarad * ww
      real :: ct     = 0.0             ! ct = CV * CV / gascon
      real :: pnu21  = 0.0             ! pnu21 = 1.0 - 2.0 * pnu ! Time filter 2
      real :: rdbrv  = 0.0             ! rdbrv = gascon / RV  ! Rd/Rv
      real :: rotspd = 1.0             ! rotation speed (factor)
      real :: eccen  = ECCEN_EARTH     ! Eccentricity of Orbit
      real :: obliq  = OBLIQ_EARTH     ! Obliquity of Orbit
      real :: mvelp  = LOPER_EARTH     ! Longitude of moving vernal equinox


      end module pumamod

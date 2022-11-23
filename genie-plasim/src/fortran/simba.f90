! =======================
! VEGETATION MODULE SIMBA              ! by Axel Kleidon & Pablo Paiewonsky
! =======================

module vegmod
use landmod
implicit none

integer :: jhor

! parameter for soil component

real, parameter :: vws_crit  = 0.25             ! critical soil wetness

! parameter for vegetation component
! all values are in m-k-s units

real, parameter :: q10       = 2.0              ! q10 value for resp

!	ENTS namelist parameters

real	::	k7=0.461
real	::	k8=0.45
real	::	k9=0.15
real	::	k10=0.02
real	::	k11a=309.15
real	::	k11b=283.15
real	::	k12=268.25
real	::	k13=29.0
real	::	k14=145.0
real	::	k16=11.5
real	::	k17=0.35
real	::	k18=1.3985e-7	!converted from yr-1 to s-1
real	::	k20=54830.0
real	::	k24=6.9123e-9	!converted from yr-1 to s-1
real	::	k26=4.2117e-9	!converted from yr-1 to s-1
real	::	k29=4.0188e-9	!converted from yr-1 to s-1
real	::	k31=308.56
real	::	k32=227.13
real	::	kz=0.0452
real    ::      tadj=0.0    !shift temperature response curve
real    ::      qthresh=0.1 !moisture response threshold (default ENTS value 0.5)

!     * SIMBA parameters for land surface clean up redundancy!!!

real, parameter :: zlaimax   =  9.0               ! max LAI
real, parameter :: zlaimin   =  0.1               ! min LAI for bare soil
real, parameter :: valb_min  =  0.12              ! min albedo for veg
real, parameter :: valb_max  =  0.30              ! max albedo for bare soil
real, parameter :: vsalb_min =  0.20              ! snow albedo for veg
real, parameter :: vwmax_max =  0.5               ! max SWHC for veg
real, parameter :: vwmax_min =  0.05              ! SWHC for bare soil
real, parameter :: vz0_max   =  2.0               ! max roughness for veg 
! not used! Uses pz0_max from landmod instead, which is a namelist parameter (PP)
real, parameter :: vz0_min   =  0.05              ! min roughness for bare soil
real, parameter :: cveg_a    =  2.9               ! forest cover conversion
real, parameter :: cveg_b    =  1.0               ! forest cover conversion
real, parameter :: cveg_c    =  1.570796326794897 ! forest cover conversion
real, parameter :: cveg_d    =  -1.9              ! new forest cover parameter
real, parameter :: cveg_f    =  3.0              ! new soil cover parameter
real, parameter :: cveg_k    =  0.5               ! conversion LAI
real, parameter :: cveg_l    =  0.25              ! LAI-biomass relationship
real, parameter :: ct_crit   =  5.0               ! for temp limit function

!* beta factor defined as GPP  = (1 + beta ln(C/C0))
!  beta  = 0.3-0.6, C0 = 360ppm - Harvey 1989

real, parameter :: co2_ref   = 360.     ! co2 reference concentration in ppm
real, parameter :: co2_comp  = 0.       ! light compensation point in ppm
real, parameter :: co2_sens  = 0.3      ! beta factor

real            :: cveg_e               ! forest cover conversion
real            :: cveg_g               ! soil cover conversion

!     * dynamically calculated land surface parameters

real,dimension(NHOR) :: zforest=   0.0         ! forest cover = dforest(:)
real,dimension(NHOR) :: zwmax  =   0.0         ! bucket depth = dwmax(:)
real,dimension(NHOR) :: dvz0   =   vz0_min     ! roughness length
real,dimension(NHOR) :: dvalb  =   valb_max    ! albedo
real,dimension(NHOR) :: dvrhs  =   0.          ! surface conductance
real,dimension(NHOR) :: dvsoil =   0.          ! soil cover
real :: zalbsn =   0.          ! snow albedo

!     * output of cumulative fluxes

integer :: ibiomass
real    :: zlaim                 ! maximum lai
real    :: zvegm                 ! maximum veg cover
real    :: zft
real    :: zsvp                  ! saturation vapor pressure
real    :: zvpd                  ! vapor pressure deficit
real    :: zbeta
real    :: zglacfree             ! glacier free fraction

end module vegmod


!========!
! VEGINI !
!========!

subroutine vegini
use vegmod
implicit none

      namelist/entspar/k7,k8,k9,k10,k11a,k11b,k12,k13 &
     &		,k20,k24,k29,k31,kz,tadj

      namelist/entsplasim/ qthresh,k14,k16,k17,k18,k26,k32

if(mypid == NROOT) then

 open(unit=56,file='data_PLASIM')
 read(unit=56,NML=ENTSPLASIM)
 close(56)
 write(*,entsplasim)

 open(12,file=trim(runtime_root)//'/genie-plasim/config/veg_namelist',form='formatted')
 read(12,entspar)
 write (*,'(/," *****************************************")')
 write (*,'(" * VEGMOD ",a29," *")') trim(version)
 write (*,'(" *****************************************")')
 write (*,'(" * Namelist ENTSPAR from <land_namelist> *")')
 write (*,'(" *****************************************")')
 write(*,entspar)
 close(12)
endif

if (nrestart == 0) then
   where (dls(:) == 0) dforest(:) = 0.0
endif

end subroutine vegini


!=========!
! VEGSTOP !
!=========!

subroutine vegstop
use vegmod
implicit none

return
end subroutine vegstop

!=========!
! VEGREST !
!=========!

subroutine veg_rest
use vegmod
implicit none

return
end subroutine veg_rest


!=========!
! VEGSTEP !
!=========!

subroutine vegstep
use vegmod
implicit none

!some variables for ents THESE SHOULD BE INTTRODUCED AS NAMELIST/MODULE PARAMETERS!!!!!!!!!!!
real f1		!co2 limitation on photosynthesis
real f2 	!water stress limitation on photosynthesis
real f3a,f3b 	!temperature limitation on photosynthesis
real f4 	!vegetation respiration
real epsilon 	!self shading
real rk19
real k25
real k30
real alpha_soil
! ents parameters
real alpha_peat
real alpha_sand
real alpha_veg
real alpha_snow
real alpha_veg_snow
real tref
real tzero
real tair,tland

integer i,j

! assign values to ents parameters
alpha_peat=0.11
alpha_sand=0.3
alpha_veg=0.1
alpha_snow=0.8
alpha_veg_snow=0.3
tref=298.15
tzero=273.15

rk19=(278.0-k13+k14)/(278.0-k13)
k25=exp(-k20/(8.314*tref))
k30=exp(-k31/(tref-k32))

co2veg=max(co2veg,0.1) !prevent blowups

! Initialization and copy of puma fields needed
cveg_e = atan(cveg_d) ! forest cover
cveg_g = atan(-cveg_f)   ! soil cover
zbeta  = max(0.,1.+co2_sens*log((co2veg-co2_comp)/(co2_ref-co2_comp)))

! Following plasim arrays are used but not modified
! -------------------------------------------------
! dwatc(:) = soil wetness [m]
! dgppl(:) = GPP light limited
! dswfl(:) = short wave radiation [W/m2]
! devap(:) = surface evaporation (negative)

! Make local copies of some plasim arrays
! They are copied back if NBIOME is set to 1 (interactive)

zforest(:) = dforest(:) ! Forest cover (0.0 - 1.0)
zwmax(:)   = dwmax(:)   ! Bucket depth

! Initialize arrays (declared in plasimmod)

dgpp(:)   = 0.0  ! Gross primary production [kg C m-2 s-1]
dgppl(:)  = 0.0  ! GPP light limited        [kg C m-2 s-1] !this is now the temperature limited ENTS formulation
dgppw(:)  = 0.0  ! GPP water limited        [kg C m-2 s-1]
dnpp(:)   = 0.0  ! Net primary production   [kg C m-2 s-1]
dveg(:)   = 0.0  ! Vegetation cover         [0..1]

sfxatm_lnd=0.0

do jhor = 1 , NHOR
  if (dls(jhor) > 0.0 .and. dglac(jhor) < 0.90) then ! land cell with < 90 % glacier
    zglacfree = 1.0 - dglac(jhor) ! glacier free fraction

    tair=dtsa(jhor)
    tland=dtsoil(jhor)
    dveg(jhor)=1.0-exp(-k17*dcveg(jhor))                        !fractional vegetation
! gross primary production
    f1 = rk19*(co2veg-k13)/(co2veg-k13+k14)                           !co2 limitation
    f2 = (1.0/(0.75-qthresh))*((dwatc(jhor)/zwmax(jhor))-qthresh)
    f2 = max(min(f2,1.0),0.0)                                   !moisture limitation
!switch to turn off moisure limitation (e.g. with nbiome=0 (so no effect of vegetation on climate), can be used to look at effect of irrigation
    if(nmoist==0) f2=1.0
    dgppw(jhor)=f2
    f3a = 2.0**(0.1*(tair-tref+tadj))                           !temperature limitation tropical
    f3a = f3a/(1.0+exp(0.3*(tair-k11a+tadj)))                   !tadj shifts temp optima
    f3a = f3a/(1.0+exp(-0.3*(tair-k12+tadj)))
    f3b = 2.0**(0.1*(tair-tref+tadj))                           !temperature limitation boreal
    f3b = f3b/(1.0+exp(0.6*(tair-k11b+tadj))) 
    f3b = f3b/(1.0+exp(-0.3*(tair-k12+tadj))) 
    dgppl(jhor) = f3a+f3b                                       !temperature limited GPP
    dgpp(jhor)=k18*f1*f2*(f3a+f3b)*dveg(jhor)                   !GPP

! vegetation respiration
    f4 = (k24/k25)*exp(-k20/(8.314*tair))*dcveg(jhor)

! net primary prodcution
    dnpp(jhor) = dgpp(jhor)-f4
    dnpp(jhor) = pgrow(jhor)*dnpp(jhor)*zglacfree		!retain puma limitation of growth under ice (or with pgrow mask)
    dnogrow(jhor)  = (1.0-pgrow(jhor))*dnpp(jhor)		!limitation imposed by pgrow mask

! leaf litter
    epsilon = 1.0/(1.0+exp(k16-dcveg(jhor)))			!self-shading
    dlitter(jhor)=k26*dcveg(jhor)+epsilon*dnpp(jhor)

! soil respiration
    if(tland.ge.273.15) then
      dres(jhor)=(k29/k30)*exp(-k31/(tland-k32))*dcsoil(jhor)
    else
      dres(jhor)=(k29/k30)*exp(-k31/(tzero-k32))*dcsoil(jhor)
      dres(jhor)=dres(jhor)*(exp(10.0*k31/((tzero-k32)**2)))**(0.1*(tland-tzero))
    endif

!update vegetation carbon
    dcveg(jhor) = dcveg(jhor)+(dnpp(jhor)-dlitter(jhor))*deltsec*ncveg  !! FL
    dcveg(jhor) = max(dcveg(jhor),0.001) !minimum to ensure presence of seed
!update soil carbon
    dcsoil(jhor) = dcsoil(jhor)+(dlitter(jhor)-dres(jhor))*deltsec*ncveg !! FL

!carbon flux for genie
    i=mod(jhor,nlon)
    j=int(jhor/nlon)+1
    sfxatm_lnd(i,j)=(-dnpp(jhor)+dres(jhor))*ncveg/0.012 !kgC to mol
    ddummy(jhor)=sfxatm_lnd(i,j)
                 
! derivation of land surface parameters
! field capacity
    zwmax(jhor)=min(k8,k9+k10*dcsoil(jhor))
! surface albedo
    alpha_soil = max(alpha_peat,((alpha_peat-alpha_sand)*k10*dcsoil(jhor)/(k8-k9))+alpha_sand)
    dvalb(jhor)=dveg(jhor)*alpha_veg+(1.0-dveg(jhor))*alpha_soil
!roughness length
    dvz0(jhor)  = pz0_max(jhor) * zforest(jhor)+vz0_min*(1.0-zforest(jhor))
!    dvrhs(jhor) = pgs(jhor) * min(1.0,max(0.0,dwatc(jhor)/(zwmax(jhor)*vws_crit)))
! use ents formulation for soil saturation (beta)
    dvrhs(jhor) =  pgs(jhor)*min((dwatc(jhor)/zwmax(jhor))**4,1.0)
    if (dsnow(jhor) > 1.0e-3) then				!! PBH only if snow depth > 1 mm
      dvalb(jhor)=(alpha_snow-alpha_veg_snow)*exp(-k7*dcveg(jhor))+alpha_veg_snow
      dvrhs(jhor)=1.0
    endif

!some land surface variables not used by ents
!leaf area index
    dlai(jhor)  = -log(1.0 - dveg(jhor))/cveg_k
! forest cover
    zforest(jhor) = (atan(dcveg(jhor) - cveg_a) - cveg_e) / (cveg_c - cveg_e)
    zforest(jhor) = min(1.0,max(0.0,zforest(jhor)))
! soil cover
    dvsoil(jhor) = (atan(dcveg(jhor) - cveg_f) - cveg_g) / (cveg_c - cveg_g)
    dvsoil(jhor) = min(1.0,max(0.0,dvsoil(jhor)))

! discretization of vegetation state
    if (rnbiocats >= 2.0) then
      ibiomass      = zforest(jhor) * rnbiocats
      zforest(jhor) = min(1.0, real(ibiomass)/(rnbiocats-1.0))
      ibiomass      = dvsoil(jhor) * rnbiocats
      dvsoil(jhor)  = min(1.0, real(ibiomass)/(rnbiocats-1.0))
    endif

! interactive coupling

    if (nbiome == 1) then
      dz0(jhor)     = sqrt(dvz0(jhor)*dvz0(jhor)+dz0climo(jhor)*dz0climo(jhor))
      dwmax(jhor)   = zwmax(jhor)
      drhs(jhor)    = dvrhs(jhor)
      dalb(jhor)    = dvalb(jhor)
      dforest(jhor) = zforest(jhor)
    endif
!climate sees fixed initialised vegetation
    if (nbiome == 2) then
      zforest(jhor) = (atan(fixcveg(jhor) - cveg_a) - cveg_e) / (cveg_c - cveg_e)
      zforest(jhor) = min(1.0,max(0.0,zforest(jhor)))
      dvz0(jhor)  = pz0_max(jhor) * zforest(jhor)+vz0_min*(1.0-zforest(jhor))
      dz0(jhor)     = sqrt(dvz0(jhor)*dvz0(jhor)+dz0climo(jhor)*dz0climo(jhor))

      zwmax(jhor)=min(k8,k9+k10*fixcsoil(jhor))
      dwmax(jhor)   = zwmax(jhor)

      dvrhs(jhor) =  pgs(jhor)*min((dwatc(jhor)/zwmax(jhor))**4,1.0)
      if (dsnow(jhor) > 1.0e-3) then
        dvrhs(jhor)=1.0
      endif
      drhs(jhor)    = dvrhs(jhor)

      alpha_soil = max(alpha_peat,((alpha_peat-alpha_sand)*k10*fixcsoil(jhor)/(k8-k9))+alpha_sand)
      dvalb(jhor)=(1.0-exp(-k17*fixcveg(jhor)))*alpha_veg+ &
                       exp(-k17*fixcveg(jhor))*alpha_soil
      if (dsnow(jhor) > 1.0e-3) then
        dvalb(jhor)=(alpha_snow-alpha_veg_snow)*exp(-k7*fixcveg(jhor))+alpha_veg_snow
      endif
      dalb(jhor)    = dvalb(jhor)

      dforest(jhor) = zforest(jhor)
    endif


  endif ! (dls(jhor) > 0.0 .and. dglac(jhor) < 0.9)
enddo ! jhor

return
end subroutine vegstep

* AY (23/06/04) : surflux routine for models using only c-GOLDSTEIN's
*                 ocean and sea-ice modules
*
* AY (12/07/05) : removed surplus arguments to function

      subroutine surf_ocn_sic(istep,
     :     otemp, osaln, oalbd,
     :     atemp, ashum, apres, ahght,
     :     sich, sica, tice, albice,
     :     windspdxu_atm, windspdyu_atm,
     :     windspdxv_atm, windspdyv_atm,
     :     net_sw, net_lw,
     :     alb_net,
     :     rough_net,
     :     stressxu_ocn, stressyu_ocn,
     :     stressxv_ocn, stressyv_ocn,
     :     fxlho, fxsho, fxswo, fxlwo, evap_net,
     :     fxlha, fxsha, evap_atm,
     :     dthsic, dtareasic,
     :     test_energy_seaice,
     :     weight_ocn)

      use genie_util, ONLY: check_unit, check_iostat
      
#include "ocean.cmn"
      include 'netcdf_grid.cmn'
 
c AY (02/06/04)
c This surflux module was written to allow coupling between GOLDSTEIN 
c ocean and sea-ice modules, and the IGCM3.  It is a cut-down version
c of the c-GOLDSTEIN surflux routine, and is additionally modified to
c receive short- and long-wave radiation fields from the atmosphere.
c
c At present, it returns heat fluxes (latent, sensible, short- and
c long-wave) to the ocean and atmosphere, evaporation (ocean and
c atmosphere), change in sea-ice height and area, and albedo.

c ======================================================================
c Inputs/Outputs (in order)
c ======================================================================

c Inputs
c     iteration number (i.e. istep_ocn)
c     ocean surface temperature
c     ocean surface salinity
c     ocean albedo
c     lowest level atmospheric temperature
c     lowest level atmospheric specific humidity
c     lowest level atmospheric pressure
c     lowest level atmospheric height
c     sea-ice height
c     sea-ice area
c     sea-ice temperature
c     sea-ice albedo (both an input and output)
c     lowest level wind speed-x (at u point)
c     lowest level wind speed-y (at u point)
c     lowest level wind speed-x (at v point)
c     lowest level wind speed-y (at v point)
c     incoming (downwelling) short-wave solar radiation (net)
c     incoming (downwelling) long-wave radiation (net)
c     average ocean grid cell temperature (not strictly needed)
c     average ocean grid cell albedo      (not strictly needed)
c Outputs
c     sea-ice albedo (both an input and output)
c     ocean surface roughness
c     wind stress-x (at u point)
c     wind stress-y (at u point)
c     wind stress-x (at v point)
c     wind stress-y (at v point)
c     latent heat flux to ocean          (+ve downwards)
c     sensible heat flux to ocean        (+ve downwards)
c     short-wave heat flux to ocean      (+ve downwards)
c     long-wave heat flux to ocean       (+ve downwards)
c     evaporation flux to ocean          (+ve downwards)
c     latent heat flux to atmosphere     (+ve downwards - used to calc. evap)
c     sensible heat flux to atmosphere   (+ve downwards)
c     evaporation flux to atmosphere     (+ve downwards - not used by IGCM3)
c     change in sea-ice height
c     change in sea-ice area

c Notes :
c Output albedo does not include the effects of zenith angle - incoming
c short-wave is net, so has already been corrected for this factor.  The 
c ocean has a constant albedo everywhere.
c
c In c-GOLDSTEIN surface wind speed is calculated from the wind 
c stress fields supplied by the EMBM.  The code below takes wind speed
c from the lowest level of the atmosphere, calculates surface wind
c stress from this, then uses the "normal" c-GOLDSTEIN calculations to
c determine surface wind speed from wind stress.
c
c Sea-ice albedo is an input and an output of this routine.  It is
c needed as an input to deconvolute the IGCM3's net SW heat flux,
c but is recalculated during the routine and used also as an output.
c Just in case you were wondering.

c ======================================================================
c Declarations
c ======================================================================

c Declare variables passed into this subroutine
      integer istep

c Input variables
      real
     :     otemp(imax,jmax),osaln(imax,jmax),
     :     oalbd(imax,jmax),
     :     atemp(imax,jmax),ashum(imax,jmax),apres(imax,jmax),
     :     ahght(imax,jmax),
     :     sich(imax,jmax),sica(imax,jmax),tice(imax,jmax),
     :     albice(imax,jmax),
     :     windspdxu_atm(imax,jmax),windspdyu_atm(imax,jmax),
     :     windspdxv_atm(imax,jmax),windspdyv_atm(imax,jmax),
     :     net_sw(imax,jmax),net_lw(imax,jmax),
     :     alb_net(imax,jmax)

c Output variables
      real
     :     rough_net(imax,jmax),
     :     stressxu_ocn(imax,jmax),stressyu_ocn(imax,jmax),
     :     stressxv_ocn(imax,jmax),stressyv_ocn(imax,jmax),
     :     fxlho(imax,jmax),fxsho(imax,jmax),
     :     fxswo(imax,jmax),fxlwo(imax,jmax),
     :     evap_net(imax,jmax),
     :     fxlha(imax,jmax), fxsha(imax,jmax),
     :     evap_atm(imax,jmax),
     :     dthsic(imax,jmax),dtareasic(imax,jmax)

c Local variables
      real usurf(imax, jmax), tsfreez(imax, jmax), qb(imax, jmax)
      real evapsic(imax, jmax), fx0sic(imax, jmax), fxsen(imax, jmax)
      real qsato(imax, jmax), evap(imax, jmax), fx0o(imax, jmax)
      real fxswocn
c      real olw, siclw
      real ce,ch,cesic,chsic,tv,tv2,tv3,tol
      real albsic, fxswsic , fxsensic 
c      real fx0oa, fxlwsic, fx0sica
      real qsatsic, ticold, cfxsensic, salt, dho, dhsic
c      real alw, dhadj
      real tieqn, dtieq

      real atm_sensible
      real atm_sensiblei
c      real atm_netsoli, atm_netlong, atm_latent, atm_latenti
c      real atm_netsol, atm_netlongi

c Ocean and sea-ice long- and short-wave fluxes
      real netsw_ocn(imax, jmax), netsw_sic(imax, jmax)
      real netlw_ocn(imax, jmax), netlw_sic(imax, jmax)

c Lowest level wind speed --> surface wind stress parameters
      real z1, rhogrnd, karman, z0, u0, ugrnd

      real tmp1, tmp2, tmp3, tmp4
      real tice0

      integer i, j, iter, itice

      parameter(itice = 101 , tol=1e-10)

c For file error checks
      integer ios

#ifdef clock
c AY (09/03/04) : "time" variables for Dan
      integer it1, it2, it3, it4
      real    rt1, rt2
#endif

      character ext*3, conv_surfocnsic*3

c+DJL
c     For energy checking......
      real test_energy_seaice
      real diff_netsolar
      real diff_netlong
      real diff_latent
      real diff_sensible
      real weight_ocn(imax,jmax)
c-DJL

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      diff_latent = 0.0
      diff_sensible = 0.0
c ------------------------------------------------------------ c

c ======================================================================
c Input field modifications
c ======================================================================

c AY (07/07/04) : Following discussion with Dan, this next section
c	          now needs to calculate :
c		    - surface wind stress from lowest level wind speed
c	            - surface wind speed from surface wind stress
c	            - using previous t step info to decompose net fluxes
c                     to fluxes to ocean and sea-ice (if possible!)

c	Calculating heat flux components from net heat fluxes
c	-----------------------------------------------------
c
c+DJL First of all, update seaice energy from the previous timestep...
      if (mod(istep,conserv_per).eq.0) then
      diff_latent=0.0
      diff_sensible=0.0
      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax) then
              diff_latent=diff_latent+(fxsho(i,j)-fxsha(i,j))*
     :           weight_ocn(i,j)
              diff_sensible=diff_sensible+(fxlho(i,j)-fxlha(i,j))*
     :           weight_ocn(i,j)
            endif
         enddo
      enddo
      diff_latent=diff_latent*3600.0*24.0*yearlen*rsc*rsc*4.0*pi/
     :               real(nyear)
      diff_sensible=diff_sensible*3600.0*24.0*yearlen*rsc*rsc*4.0*pi/
     :               real(nyear)
      endif
c-DJL


c       The IGCM3 provides GOLDSTEIN with net fluxes of short- and
c       long-wave radiation.  These net fluxes obscure the components
c       that go to the ocean and the sea-ice (i.e. in the case of
c       short-wave, sea-ice should receive far less than open ocean
c       because of its high albedo).  This section of code reverse
c       engineers ocean and sea-ice fluxes from the net fluxes.  It
c       isn't ideal, but such is life ...

      do j=1,jmax
         do i=1,imax
c           Is this an ocean cell?
            if(k1(i,j).le.kmax) then
c              Does this ocean cell have sea-ice?
               if(sica(i,j).gt.0.0)then
c              Only cells with sea-ice are affected
c
c                 Short-wave
                  tmp1 = net_sw(i,j) / (1 - alb_net(i,j))
                  netsw_ocn(i,j) = tmp1 * (1 - oalbd(i,j))
                  netsw_sic(i,j) = tmp1 * (1 - albice(i,j))
c
c                 Long-wave
                  tmp1 = emo*((otemp(i,j)+zeroc)**4)
                  tmp2 = emo*(( tice(i,j)+zeroc)**4)
                  tmp3 = ((1 - sica(i,j))*tmp1) + (sica(i,j) * tmp2)
                  tmp4 = net_lw(i,j) + tmp3
                  netlw_ocn(i,j) = tmp4 - tmp1
                  netlw_sic(i,j) = tmp4 - tmp2
               else
c              Ice-free ocean cells get the full whack
c
c                 Short-wave
                  netsw_ocn(i,j) = net_sw(i,j)
                  netsw_sic(i,j) = 0.
c                 Long-wave
                  netlw_ocn(i,j) = net_lw(i,j)
                  netlw_sic(i,j) = 0.
               endif
            endif
         enddo
      enddo
                 
c AY (11/12/03) : This next section executes routines that calculate
c                 usurf from wind stresses.  If the winds are fixed,
c                 as they are with the EMBM, this only needs be run
c                 just prior to the first iteration.  If the winds
c                 are time-variant, as they are with the IGCM, this
c                 will need to be run every time-step.

c AY (10/06/04) : usurf is just scalar wind speed - the following
c	          includes the original c-GOLDSTEIN code for
c	          translating wind stresses into wind speed as well
c	          as simply calculating usurf from the first
c	          components of the stress*_ocn fields

c AY (08/07/04) : following Dan's advice, this routine now receives
c                 wind speed at the lowest level of the atmosphere,
c                 converts this into wind stress at the ocean's
c                 surface (the fields of which are then passed to
c                 the ocean to control wind-driven circulation), and
c                 then uses this wind stress to calculate surface
c                 wind speed for use (as per usual) in calculating
c                 the surface fluxes (talk about tortuous!)

c     Calculating surface wind stress from lowest level wind speed
c     ------------------------------------------------------------

c AY (12/07/04) : note in the lines following, I am reverting to using
c                 some of GOLDSTEIN's own values for some of these
c                 parameters
c AY (03/09/04) : still need height at which lowest level atmospheric
c	          properties are valid
c AY (16/03/05) : Dan has made lowest level atmospheric height available
c                 at the genie.F level, so now included - does now mean
c                 that the calculation of the bulk aerodynamic constant
c                 sits in the surface wind stress loop

c     Lowest atmospheric level air density (needs calculating really)
      rhogrnd = rhoair
c     Von Karman constant (Dan = 0.41; Phil = 0.40)
      karman = 0.40
c     Roughness length (from Dan)
      z0 = 0.001
c     Tunable parameter (sometimes called "gustiness")
c     u0 = 3.
      u0 = gust

      do j=1,jmax
         do i=1,imax
c
c AY (16/03/05) : calculation of bulk aerodynamic constant shifted in
c                 here because lowest level height is now fed in from
c                 the IGCM3
c
c     Lowest atmospheric level height (Phil's code says 10 m; my value
c     of Cd suggests something closer to 100 m)
c           z1 = 100.
            z1 = real(MAX(1.0, ahght(i,j)))
c     Bulk aerodynamic constant
c           Cd = cd
            Cd = (karman / (log(z1 / z0)))**2
c
c           u point
            ugrnd = u0 + sqrt(windspdxu_atm(i,j)**2 +
     +           windspdyu_atm(i,j)**2)
            stressxu_ocn(i,j) = real(((Cd * rhogrnd * ugrnd) 
     +           * windspdxu_atm(i,j)))
            stressyu_ocn(i,j) = real(((Cd * rhogrnd * ugrnd) 
     +           * windspdyu_atm(i,j)))
c
c           v point
            ugrnd = u0 + sqrt(windspdxv_atm(i,j)**2 +
     +           windspdyv_atm(i,j)**2)
            stressxv_ocn(i,j) = real(((Cd * rhogrnd * ugrnd) 
     +           * windspdxv_atm(i,j)))
            stressyv_ocn(i,j) = real(((Cd * rhogrnd * ugrnd) 
     +           * windspdyv_atm(i,j)))
         enddo
      enddo

c     Calculating surface wind speed from surface wind stresses
c     ---------------------------------------------------------
c
c     The stressx and stressy input arguments are assumed to be
c     the wind stresses at the u and v points of the GOLDSTEIN
c     grid (i.e. two x and y components)
      
      do j=1,jmax
         do i=1,imax
            dztau(1,i,j) = scf*stressxu_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            dztau(2,i,j) = scf*stressyu_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            dztav(1,i,j) = scf*stressxv_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            dztav(2,i,j) = scf*stressyv_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            tau(1,i,j) = dztau(1,i,j)*dzz
            tau(2,i,j) = dztav(2,i,j)*dzz
         enddo
      enddo

c AY (11/12/03) : code below taken from gseta.F
      
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
            usurf(i,j) = (sqrt((sqrt(tv**2 + tv2**2))
     1           *rh0sc*dsc*usc*fsc/(rhoair*cd*scf)))
            tv3 = tv3 + usurf(i,j)
         enddo
         do i=1,imax
            if(j.le.2.or.j.ge.jmax-1)usurf(i,j) = tv3/imax
         enddo
      enddo
      
c AY (08/07/04) : derelict code      
c        Calculating usurf from wind speeds
c        ----------------------------------
c
c        The first components of the stressx and stressy input
c        arguments are assumed to be the x and y components of
c        wind speed at the GOLDSTEIN tracer point (i.e. single
c        set of x and y components)
c
c        do j=1,jmax
c           do i=1,imax
c              usurf(i,j) = sqrt((stressx_ocn(1,i,j)**2) +
c    +              (stressx_ocn(1,i,j)**2))
c           enddo
c        enddo

c ======================================================================
c Clear output fields
c ======================================================================

      do j=1,jmax
         do i=1,imax
            albice(i,j) = 0.
            fxlho(i,j) = 0.
            fxsho(i,j) = 0.
            fxswo(i,j) = 0.
            fxlwo(i,j) = 0.
            evap_net(i,j) = 0.
            fxlha(i,j) = 0.
            fxsha(i,j) = 0.
            evap_atm(i,j) = 0.
            dthsic(i,j) = 0.
            dtareasic(i,j) = 0.
c           A few checks on incoming fields (and why not?)
            if(atemp(i,j).lt.-100.0.or.atemp(i,j).gt.100.0) then
               print*,'ATEMP ERROR at ',i,j,'=',atemp(i,j)
            endif
            if(ashum(i,j).lt.0.0)then
               print*,'ASHUM ERROR at ',i,j,'=',ashum(i,j)
            endif
         enddo
      enddo

c ======================================================================
c Return current model time (if required)
c ======================================================================

c AY (09/03/04) : Write out current time (days, years, etc.) if required
#ifdef clock
 120  format(a26,i8,a6,i6,a5,f9.4)

      it1 = istep - 1
      it2 = mod(it1, nyear)
      it3 = it1 - it2
      it4 = it3 / nyear
      rt1 = yearlen / nyear
      rt2 = real(it2)*rt1
      write(*,120) 'G-SURFLUX time : iteration',istep,', year',it4,
     +     ', day',rt2
      
#endif

c ======================================================================
c SURFLUX model timestep
c ======================================================================

c-----------------------------------------------------------------------
c     main i,j loop to compute surface flux terms
c-----------------------------------------------------------------------
      do j=1,jmax
         do i=1,imax

c-----------------------------------------------------------------------
c           calculate terms over ocean or ice
c-----------------------------------------------------------------------
            if(k1(i,j).le.kmax) then

c reset some parameters
               albsic = 0.

c AY (13/07/04) : hacked because of net longwave
c downwelling longwave radiation
c              alw = down_lw(i,j)

c surface salinity-dependent freezing point:
               salt = saln0+osaln(i,j)
               tsfreez(i,j) = salt*(-0.0575 + 0.0017*sqrt(salt)
     1              - 0.0002*salt)

c maximum amount of heat available in first layer
c nre rsictscsf must be changed if dt>17.5 days, see gseta
               qb(i,j) = rsictscsf*(tsfreez(i,j)-otemp(i,j))

c-----------------------------------------------------------------------
c              calculate terms over ice
c-----------------------------------------------------------------------

               if(sica(i,j).gt.0.0)then
c              * Sea-ice present *                 

c let albedo over sea ice vary as a function of tair (Holland et al. 1993)
                  albsic = max(0.20,min(0.7,0.40 - 0.04*atemp(i,j)))

c AY : incoming shortwave
c AY (28/06/04) : pre-albedo-ed
                  fxswsic = netsw_sic(i,j)

                  tice0 = tice(i,j)
c first need to calculate T_ice
                  do iter=1,itice
                     ticold = tice(i,j)
c Dalton number
                     cesic = ene_tune*
     &                    1.0e-3*(1.0022 - 0.0822*(atemp(i,j)
     1                    - ticold) + 0.0266*usurf(i,j))
                     cesic = max(6.0e-5,min(2.19e-3,cesic))
                     
                     chsic = 0.94*cesic

c sensible heat flux 
                     cfxsensic = rhoair*chsic*cpa*usurf(i,j)
                     
                     qsatsic = const1*exp(const2*ticold         
     1                    /(ticold + const3))
                     
                     evapsic(i,j) = max(0.0,(qsatsic - ashum(i,j))
     1                    *rhoao*cesic*usurf(i,j))

c AY (08/07/04) : hacked to insert net longwave flux
c                    tieqn = sich(i,j)*(fxswsic + alw 
c    1                     - emo*(ticold+zeroc)**4  - cfxsensic*(ticold 
c    2                     - atemp(i,j)) - rho0*hls*evapsic(i,j))
c    3                     + consic*(tsfreez(i,j)-ticold)
                     tieqn = sich(i,j)*(fxswsic + netlw_sic(i,j) 
     1                    - cfxsensic*(ticold 
     2                    - atemp(i,j)) - rho0*hls*evapsic(i,j))
     3                    + consic*(tsfreez(i,j)-ticold)

                     dtieq = sich(i,j)*( 
     1                    - 4.0*emo*(ticold+zeroc)**3 - cfxsensic
     1                    - hls*rhoair*cesic*usurf(i,j)*qsatsic*const2
     2                    *const3/((ticold + const3)**2)
     4                    *0.5*(1.0 + sign(1.0,qsatsic - ashum(i,j))) )
     5                    - consic

                     tice(i,j) = real(ticold-tieqn/dtieq)
                     
                     if(abs(tice(i,j) - ticold).lt.tol .or.     
     1                    ticold.gt.tfreez.and.tieqn.gt.0.0)goto 10
                  enddo

                  print*,'warning sea-ice iteration failed at',istep,i,j
                  print*,'old temperature =',tice0,'new temperature =',
     +                 tice(i,j)
                  if (.not.debug_loop) stop

 10               tice(i,j) = min(real(tfreez),tice(i,j))

c-----------------------------------------------------------------------
c                 recalc sea-ice terms in case tice's value is reset
c-----------------------------------------------------------------------

c AY (08/07/04) : hacked because of net longwave flux
c AY (01/06/04) : outgoing sea-ice longwave
c                 siclw = emo*(tice(i,j)+zeroc )**4

c Dalton number
                  cesic = ene_tune*
     &                 1.0e-3*(1.0022 - 0.0822*(atemp(i,j)
     1                 - tice(i,j)) + 0.0266*usurf(i,j))
                  cesic = max(6.0e-5,min(2.19e-3,cesic))
                  
                  chsic = 0.94*cesic
                  
                  cfxsensic = rhoair*chsic*cpa*usurf(i,j)

                  fxsensic = cfxsensic*(tice(i,j) - atemp(i,j))

                  qsatsic = const1*exp(const2*tice(i,j)      
     1                 /(tice(i,j) + const3))
                  
                  evapsic(i,j) = max(0.0,(qsatsic - ashum(i,j))
     1                 *rhoao*cesic*usurf(i,j))

c AY (08/07/04) : hacked to insert net longwave flux
c                 fx0sic(i,j) = fxswsic - fxsensic
c    1                        - siclw + alw - rho0*hls*evapsic(i,j)
                  fx0sic(i,j) = fxswsic - fxsensic
     1                 + netlw_sic(i,j) - rho0*hls*evapsic(i,j)

c AY (01/06/04) : set up sea-ice components of atmosphere fluxes
c                  atm_latenti   = 0.
                  atm_sensiblei = + fxsensic
c AY (08/07/04) : hacked because of net shortwave flux
c                 atm_netsoli   = + down_sw(i,j)*albsic
c                  atm_netsoli   = 0.
c AY (08/07/04) : hacked because of net longwave flux
c                 atm_netlongi  = + siclw
c                  atm_netlongi  = 0.

c AY (01/06/04) : change in sea-ice height
                  dhsic = rrholf*(qb(i,j) - fx0sic(i,j)) 
     1                 - rhooi*evapsic(i,j)

               else
c                 * Sea-ice absent *
                  albsic        = 0.
                  dhsic         = 0.
                  evapsic(i,j)  = 0.
                  tice(i,j)     = 0.
c                  atm_latenti   = 0.
                  atm_sensiblei = 0.
c                  atm_netsoli   = 0.
c                  atm_netlongi  = 0.
               endif

c-----------------------------------------------------------------------
c              calculate terms over open ocean
c-----------------------------------------------------------------------

c AY (08/07/04) : hacked because of net shortwave flux
c AY (01/06/04) : incoming short-wave
c              fxswocn = down_sw(i,j)*(1. - albocn)
               fxswocn = netsw_ocn(i,j)

c AY (08/07/04) : hacked because of net shortwave flux
c AY (01/06/04) : outgoing long-wave
c              olw = emo*(otemp(i,j)+zeroc)**4

c Dalton number
               ce = ene_tune*
     &              1.0e-3*(1.0022 - 0.0822*(atemp(i,j)-
     1              otemp(i,j)) + 0.0266*usurf(i,j))
               ce = max(6.0e-5,min(2.19e-3,ce))

               ch = 0.94*ce

c sensible heat flux from ocean to atmosphere
               fxsen(i,j) = rhoair*ch*cpa*usurf(i,j)*
     1              (otemp(i,j)-atemp(i,j))

c evaporation/sublimation rate 
               qsato(i,j) = const1*exp(const4*otemp(i,j)
     1              /(otemp(i,j)+const5))

               evap(i,j) = max(0.0,(qsato(i,j) - ashum(i,j))
     1              *rhoao*ce*usurf(i,j))

c AY (10/12/03) : set up ocean components of atmosphere fluxes
c              atm_latent   = evap(i,j)*rho0*hlv
c              atm_latent   = ( - rho0*hlv*evap(i,j)
c    &              + max(0.0,qb(i,j) - fx0o(i,j)))
               atm_sensible = + fxsen(i,j)
c AY (08/07/04) : hacked because of net shortwave flux
c              atm_netsol   = + down_sw(i,j)*albocn
c               atm_netsol   = 0.
c AY (08/07/04) : hacked because of net shortwave flux
c              atm_netlong  = + olw
c               atm_netlong  = 0.

c AY (08/07/04) : hacked because of net longwave flux
c heat flux from atmosphere into open ocean 
c              fx0o(i,j) = + fxswocn - fxsen(i,j)
c     1              - olw + alw - rho0*hlv*evap(i,j)
               fx0o(i,j) = + fxswocn - fxsen(i,j)
     1              + netlw_ocn(i,j) - rho0*hlv*evap(i,j)

c-----------------------------------------------------------------------
c              set up fluxes --> atmosphere
c-----------------------------------------------------------------------
c+DJL 16/11/2004
c     The next 2 lines no longer used ... overwritten just below...
c               fxlha(i,j)   = + ((sica(i,j)*atm_latenti)
c     1              + ((1-sica(i,j))*atm_latent))
c-DJL 16/11/2004
               fxsha(i,j) = + real(((sica(i,j)*atm_sensiblei)
     1              + ((1-sica(i,j))*atm_sensible)))
               fxsha(i,j) = real(-1. * fxsha(i,j))
c AY (08/07/04) : hacked because of net heat fluxes
c              fxswa(i,j) = (sica(i,j)*atm_netsoli)
c    1              + ((1-sica(i,j))*atm_netsol)
c              fxlwa(i,j)  = (sica(i,j)*atm_netlongi)
c    1              + ((1-sica(i,j))*atm_netlong)
c              evaporation flux (to atmosphere)
               evap_atm(i,j) = real(evap(i,j)*(1-sica(i,j))
     1              + evapsic(i,j)*sica(i,j))
c AY (04/08/04) : use this evaporation flux to calculate latent heat flux
               fxlha(i,j) = real(- evap_atm(i,j)*rho0*hlv)
               
c AY (23/07/04) : convert evaporation from m/s to mm/s for Dan
               evap_atm(i,j) = real(evap_atm(i,j) * m2mm)

c-----------------------------------------------------------------------
c              set up fluxes -> ocean
c-----------------------------------------------------------------------
               fxlho(i,j) = real((1-sica(i,j))*( - rho0*hlv*evap(i,j)
     &              + max(0.0,qb(i,j) - fx0o(i,j)))
     &              + sica(i,j)*qb(i,j))
               fxsho(i,j) = - real((1-sica(i,j))*fxsen(i,j))
               fxswo(i,j) = real((1-sica(i,j))*fxswocn)
c AY (08/07/04) : hacked because of net longwave flux
c              fxlwo(i,j) = - (1-sica(i,j))*(olw - alw)
               fxlwo(i,j) = real((1-sica(i,j))*netlw_ocn(i,j))

c              evaporation flux (to ocean)
               evap_net(i,j) = -evap_atm(i,j)

c AY (05/11/04) : temporary fix
               evap_atm(i,j) = -evap_atm(i,j)

c-----------------------------------------------------------------------
c              set up fluxes -> sea-ice
c-----------------------------------------------------------------------
               dho = max(0.0,rrholf*(qb(i,j) - fx0o(i,j)))

c AY (02/06/04) : change in sea-ice height
               dthsic(i,j) = real(sica(i,j)*dhsic 
     1                     + (1-sica(i,j))*dho)

c AY (02/06/04) : change in sea-ice area
               dtareasic(i,j) = real(max(0.0,rhmin*dho*(1-sica(i,j))))

               if(sich(i,j).gt.1e-12) then
                  dtareasic(i,j) = real(dtareasic(i,j)
     1                 + min(0.0,0.5*sica(i,j)*sica(i,j)
     2                 * dhsic/sich(i,j)))
               endif

c-----------------------------------------------------------------------
c              set up new sea-ice albedo
c-----------------------------------------------------------------------
               albice(i,j) = real(albsic)

c-----------------------------------------------------------------------
c              set up surface roughness
c-----------------------------------------------------------------------
               rough_net(i,j) = real(z0);

            endif
         enddo
      enddo

c ======================================================================
c Write out surflux fields
c ======================================================================

c AY (05/04/04) : call routine to output surface flux fields (in the
c                 same way as a restart file)
      if(mod(istep,iwstp).eq.0)then
         ext=conv_surfocnsic(mod(iw,10))
         print*,'Writing SURF_OCN_SIC output file at time',istep
         call check_unit(12,__LINE__,__FILE__)
         open(12,file=outdir_name(1:lenout)//lout//'.sfx.'//ext,
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         rewind 12
c AY (12/07/05) : removed surplus argument to function
         call outm_surf_ocn_sic(2,
     :        otemp, osaln,
     :        atemp, ashum, apres, 
     :        sich, sica, tice,
     :        windspdxu_atm, windspdyu_atm,
     :        net_sw, net_lw,
     :        oalbd, albice,
     :        stressxu_ocn, stressyu_ocn, usurf,
     :        fxlho, fxsho, fxswo, fxlwo, evap_net,
     :        fxlha, fxsha, evap_atm,
     :        dthsic, dtareasic)
         close(12,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         print*
      endif

c+DJL Finally, update seaice energy from this timestep, and make adjustment
c       to seaice energy.
      if (mod(istep,conserv_per).eq.0) then
      diff_netsolar=0.0
      diff_netlong=0.0
      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax) then
              diff_netsolar=diff_netsolar+(fxswo(i,j)-net_sw(i,j))*
     :           weight_ocn(i,j)
              diff_netlong=diff_netlong+(fxlwo(i,j)-net_lw(i,j))*
     :           weight_ocn(i,j)
            endif
         enddo
      enddo
      diff_netsolar=diff_netsolar*3600.0*24.0*yearlen*rsc*rsc*4.0*pi/
     :               real(nyear)
      diff_netlong=diff_netlong*3600.0*24.0*yearlen*rsc*rsc*4.0*pi/
     :               real(nyear)
      test_energy_seaice=real(
     :              test_energy_seaice-diff_latent-diff_sensible-
     :              diff_netsolar-diff_netlong)
      endif
c-DJL


c ======================================================================
c end of surflux.F
c ======================================================================

      end

* ======================================================================
* conv function (called within surflux.F)
* ======================================================================

      character*3 function conv_surfocnsic(i)
      integer     i,itemp,i1,i2,i3
      character*1 a,b,c
      if(i.lt.10)then
        a=char(i+48)
        conv_surfocnsic=a//'  '
      else if(i.lt.100)then
        i1=i/10
        i2=i-i1*10
        a=char(i1+48)
        b=char(i2+48)
        conv_surfocnsic=a//b//' '
      else
        i1=i/100
        itemp=i-100*i1
        i2=itemp/10
        i3=itemp-10*i2
        a=char(i1+48)
        b=char(i2+48)
        c=char(i3+48)
        conv_surfocnsic=a//b//c
      endif
      end

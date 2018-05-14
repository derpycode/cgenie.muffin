!Does the sea ice things in surflux
!Called when plasim is coupled

      subroutine surflux_goldstein_seaice(
!input
     & sich,sica,
     & otemp,osaln,
     & atemp,ashum,apres,
     & solar,longwave, !incoming used over ice
     & net_heat_flux,  !net used over open ocean - could this be removed from the coupling?
     & windspeed, !??is windspeed needed any more? transfer coefficients are passed in
     & evap_ocn, !used to get consistent moisture with latent heat over ice ?? need to work on evap
     & latent_coeff, !these come from plasim for consistency in energy
     & sensible_coeff, !these come from plasim for consistency in energy
!input and output (inputs over ice and ocean, outputs over ocean only)
     & latent_ocn,sensible_ocn,netsolar_ocn,netlong_ocn,
!output
     & dthsic,dtareasic,ticeout,albiceout,
     & dflux !energy and mositure conservation. Not yet used in PLASIM, hoping to get them negligible...
     & )

#include "seaice.cmn"

!IO declararations
      real
!input
     &  sich(imax,jmax),
     &  sica(imax,jmax),
     &  otemp(imax,jmax),
     &  osaln(imax,jmax),
     &  atemp(imax,jmax),
     &  ashum(imax,jmax),
     &  apres(imax,jmax),
     &  solar(imax,jmax),
     &  longwave(imax,jmax),
     &  net_heat_flux(imax,jmax),
     &  windspeed(imax,jmax),
     &  evap_ocn(imax,jmax),
     &  latent_coeff(imax,jmax),
     &  sensible_coeff(imax,jmax),
     &  latent_ocn(imax,jmax),
     &  sensible_ocn(imax,jmax),
     &  netsolar_ocn(imax,jmax),
     &  netlong_ocn(imax,jmax),
!output
     &  dthsic(imax,jmax),
     &  dtareasic(imax,jmax),
     &  ticeout(imax,jmax),
     &  albiceout(imax,jmax),
!flux "corrections" to pass back to PLASIM
!1 longwave, 2 sensible, 3 latent, 4 evaporation
     &  dflux(4,imax,jmax)   

!local declarations
!variables
      real fxswsic,fxlwsic,fxsensic,fx0sic,qsatsic
      real tieqn,dtieq
      real qb,tsfreez,salt,dho,dhsic,evapsic
      real ticold
      real tice0,evapsic0 !for flux correction - needs to be driven from ice temp at previous timestep
      real tv1,tv2,tv3,tv4,tv5
!constants
      real saln0,rrholf,rsictscsf,cpo_ice,rhoair,rhoao,hls,hlv
      real emo,consic
      real const1,const2,const3,const4,const5
      real tol
!ocean layer thickness (these should be done in an initialisation
!routine, along with other constants)
      real ez0,z1,dz(kmax),dza(kmax),dzz
!open ocean fluxes
      real qsato,evapo,netsolo,netlongo,sensibleo,latento,netheato
      real debug(10)

      real fx0t,albtemp !for energy conservation check

      integer i,j,k
      integer itice,iter 

      tol=1.0e-5
      itice=100

      dflux=0.0

!DEFINE CONSTANTS
! for saturation specific humidity. No distinction made between ice and
! water here for consistency with PLASIM.
      const1=3.7983e-3  !PLASIM VALUE = gascon*ra1/rv
      const2=17.2693882 !PLASIM VALUE = RA2_LIQUID
      const3=237.29     !PLASIM VALUE = RA4_LIQUID
      const4=17.2693882 !PLASIM VALUE = RA2_LIQUID
      const5=237.29     !PLASIM VALUE = RA4_LIQUID
!stefan boltzmann, value consistent with plasim
      emo=0.98*5.67e-8
!constant ice conductivity (W/m/K)
      consic=2.166
! latent heat of vaporisation - PLASIM VALUE
      hlv=2.5008e6
!latent heat of vaporisation - PLASIM VALUE
      hls=2.8345e6
!latent heat of fusion
      hlf=hls-hlv

! ocean surface layer thickness (dimensionless)
      ez0 = 0.1
      z1 = ez0*((1.0 + 1/ez0)**(1.0/kmax) - 1.0)
      tv4 = ez0*((z1/ez0+1)**0.5-1)
      tv2 = 0
      tv1 = 0
      do k=1,kmax
         if(ez0.gt.0)then
            tv3 = ez0*((z1/ez0+1)**k-1)
            dz(kmax-k+1) = tv3 - tv2
            tv2 = tv3
            tv5 = ez0*((z1/ez0+1)**(k+0.5)-1)
            if(k.lt.kmax) dza(kmax-k) = tv5 - tv4
            tv4 = tv5
            tv1 = tv1 + dz(kmax-k+1)
c tv3 is the depth of the kth w level from the top
c tv5 is the depth of the k+1th density level from the top
         else
            dz(k) = real(1d0/kmax)
            dza(k) = real(1d0/kmax)
         endif
       enddo

      dzz = dz(kmax)*dza(kmax-1)/2

!representative water density (kg/m3)
      rho0=1.0e3
!representative air density (kg/m3)
      rhoair=1.25
      rhoao=rhoair/rho0
!specific heat of sea water under ice
      cpo_ice=4044.
!reference salinity
      saln0=34.9
!melting factor
      rrholf=1.0/(rhoice*hlf)
!useful constant for surface freezing (timescale 17.5 days)
      rsictscsf=dsc*dz(kmax)*rho0*cpo_ice/(17.5*86400.)
      rsictscsf=scale_oheat*rsictscsf

!INITIALISE VARIABLES
      do j=1,jmax
        do i=1,imax
          dthsic(i,j)=0.0
          dtareasic(i,j)=0.0
        enddo
      enddo

      do j=1,jmax
        do i=1,imax
          if(k1(i,j).le.kmax) then
            fx0sic=0.0
            evapsic=0.0
! salinity dependent freezing point. saln0 can go negative (rigid lid
! approximation), so line contains a check to prevent blow up.
            salt=max(saln0+osaln(i,j),0.0)
            tsfreez=salt*(-0.0575+0.0017*sqrt(salt)-0.0002*salt)
!maximum amount of heat available in the first layer
            qb=rsictscsf*(tsfreez-otemp(i,j))

c some terms derived to check energy conservation wrt PLASIM
            tice0=tice(i,j)
!surface pressure used here (GENIE assumes 1)
            qsatsic = const1*exp(const2*tice0         
     1                    /(tice0 + const3))/apres(i,j)
!transfer coefficient supplied by PLASIM
            evapsic0 = max(0.0,(qsatsic - ashum(i,j))
     1                    *latent_coeff(i,j))

            albtemp=albice(i,j) !conservation check needs to apply albedo from previous time step
            albice(i,j)=max(0.5,min(maxalbice,0.5-0.025*atemp(i,j))) !plasim expression except 0.2->0.4
            if(sica(i,j).gt.0.0) then 

! checking energy conservation!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
!CONSERVATION IS PERFECT FOR SHORTWAVE. SENSIBLE, UPWARD
!LONGWAVE AND LATENT HEAT ARE UNDERSTATED IN PLASIM. THIS IS PARTLY
!BECAUSE PLASIM DOES NOT ACCOUNT FOR SEA-ICE LEADS, ALSO BECAUSE OF
!NON-LINEAR LATENT TERMS WHICH ARE NOT ACCOUNTED FOR IN THE TIME-STEP
!AVERAGING
            netsolo=netsolar_ocn(i,j)-solar(i,j)*(1.0-albtemp)*sica(i,j)
            if(sica(i,j).lt.1.) then 
             netsolo=netsolo/(1.-sica(i,j))
            else
             netsolo=0.0
            endif
!ocean fluxes 
            netlongo=longwave(i,j)-emo*(otemp(i,j)+273.15)**4
            sensibleo=sensible_coeff(i,j)*(atemp(i,j)-otemp(i,j))
            qsato=const1*exp(const4*otemp(i,j)/(otemp(i,j)+const5))/apres(i,j)
            evapo=-max(0.0,(qsato-ashum(i,j))*latent_coeff(i,j))
            latento=rho0*hlv*evapo
            netheato=netsolo+netlongo+sensibleo+latento
!sea ice fluxes
            fxswsic=solar(i,j)*(1.0-albtemp)
            fxlwsic = emo*(tice(i,j)+273.15)**4 - longwave(i,j)
            fxsensic = sensible_coeff(i,j)*(tice(i,j) - atemp(i,j))
            qsatsic = const1*exp(const2*tice(i,j)      
     1                  /(tice(i,j) + const3))/apres(i,j)
            evapsic = max(0.0,(qsatsic - ashum(i,j))
     1                      *latent_coeff(i,j))
            fx0sic = fxswsic -fxsensic
     1                      - fxlwsic - rho0*hls*evapsic
!total flux
            fx0t=sica(i,j)*fx0sic+(1.0-sica(i,j))*netheato
!output here if desired (compared with total net flux calculated in plasim)
!            write(77,600)i,j,sica(i,j),fx0t,netsolar_ocn(i,j)+
!     1        netlong_ocn(i,j)+sensible_ocn(i,j)+latent_ocn(i,j)
!            write(6,600)i,j,sica(i,j),fx0t-netsolar_ocn(i,j)-
!     1        netlong_ocn(i,j)-sensible_ocn(i,j)-latent_ocn(i,j)
 600    format(2i4,10f12.4)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


              fxswsic=solar(i,j)*(1.0-albice(i,j))
!calculate ice temperature
              do iter=1,itice
                ticold = tice(i,j)

!adjusted for surface pressure for consistency with PLASIM
                qsatsic = const1*exp(const2*ticold         
     1                    /(ticold + const3))/apres(i,j)
!moisture transfer coefficient supplied by PLASIM
                evapsic = max(0.0,(qsatsic - ashum(i,j))
     1                         *latent_coeff(i,j))

!sensible transfer coefficient supplied by PLASIM
                tieqn = sich(i,j)*(fxswsic + longwave(i,j)
     1                 - emo*(ticold+273.15)**4  - sensible_coeff(i,j)*(ticold 
     2                 - atemp(i,j)) - rho0*hls*evapsic)
     3                 + consic*(tsfreez-ticold)

                dtieq = sich(i,j)*( 
     1                 - 4.0*emo*(ticold+273.15)**3 -sensible_coeff(i,j) 
     1                 - hls*rho0*latent_coeff(i,j)*qsatsic*const2
     2                 *const3/((ticold + const3)**2)
     4                 *0.5*(1.0 + sign(1.0,qsatsic - ashum(i,j))) )
     5                 - consic

                tice(i,j) = ticold - tieqn/dtieq

                if(abs(tice(i,j) - ticold).lt.tol) goto 10   
              enddo

!end loop for ice temperature
              print*,"sea ice did not converge",tice(i,j),ticold
              tice(i,j)=0.5*(tice(i,j)+ticold)
              print*,i,j
              print*,"qsatsic",qsatsic
              print*,"evapsic",evapsic
              print*,"tieqn",tieqn
              print*,"dtieq",dtieq
              print*,"apres",apres(i,j)
              print*,"ashum",ashum(i,j)
              print*,"latent_coeff",latent_coeff(i,j)
              print*,"sich",sich(i,j)
              print*,"fxwsic",fxswsic
              print*,"longwave",longwave(i,j)
              print*,"emo",emo
              print*,"sensible_coeff",sensible_coeff(i,j)
              print*,"atemp",atemp(i,j)

   10         tice(i,j)=min(tice(i,j),0.0)

c recalc everything in case of resetting of tice
              fxlwsic = emo*(tice(i,j)+273.15)**4 - longwave(i,j)
              fxsensic = sensible_coeff(i,j)*(tice(i,j) - atemp(i,j))
              qsatsic = const1*exp(const2*tice(i,j)      
     1                  /(tice(i,j) + const3))/apres(i,j)
              evapsic = max(0.0,(qsatsic - ashum(i,j))
     1                      *latent_coeff(i,j))
              fx0sic = fxswsic -fxsensic
     1                      - fxlwsic - rho0*hls*evapsic

            else ! no ice
!want something defined in case there is new ice
              tice(i,j)=tsfreez
            endif

!outputs
            ticeout(i,j)=tice(i,j)
            albiceout(i,j)=albice(i,j)

!calculate fluxes over open ocean
            netsolo=netsolar_ocn(i,j)-solar(i,j)*(1.0-albice(i,j))*sica(i,j)
!if sica=1 then netsolo below is not correct, but does not matter as flux scaled by (1-sica) in calculation of dthsic
            if(sica(i,j).lt.1.) netsolo=netsolo/(1.-sica(i,j))
            netlongo=longwave(i,j)-emo*(otemp(i,j)+273.15)**4
            sensibleo=sensible_coeff(i,j)*(atemp(i,j)-otemp(i,j))
            qsato=const1*exp(const4*otemp(i,j)/(otemp(i,j)+const5))/apres(i,j)
            evapo=-max(0.0,(qsato-ashum(i,j))*latent_coeff(i,j))
            latento=rho0*hlv*evapo
            netheato=netsolo+netlongo+sensibleo+latento

!now calculate sea ice change rates
            dho=max(0.0,rrholf*(qb-netheato))
            dhsic=rrholf*(qb-fx0sic)-rhooi*evapsic
            dthsic(i,j)=sica(i,j)*dhsic+(1.-sica(i,j))*dho
            dtareasic(i,j)=max(0.0,rhmin*dho*(1.-sica(i,j)))
            if(sich(i,j).gt.1e-12) then
               dtareasic(i,j) = dtareasic(i,j)
     &          + min(0.0,0.5*sica(i,j)*sica(i,j)*dhsic/sich(i,j))
            endif

! adjust energy fluxes into ocean to account for sea ice. 
            if(sica(i,j).gt.0.0) then 
!solar. Subtract netsolar energy over ice, ensures energy conservation
              netsolar_ocn(i,j)=netsolar_ocn(i,j)-
     1         solar(i,j)*(1.0-albice(i,j))*sica(i,j)
!following terms do not ensure conservation. dflux is the error. This is
!passed back to plasim. As at 5/9/14 this is not used in plasim so
!energy and moisture not perfectly conserved
!longwave.
              dflux(1,i,j)=netlong_ocn(i,j)-
     1         (1.0-sica(i,j))*netlongo-
     2         sica(i,j)*(longwave(i,j)-emo*(tice0+273.15)**4)
              netlong_ocn(i,j)=(1.0-sica(i,j))*netlongo
!sensible
              dflux(2,i,j)=sensible_ocn(i,j)-
     1         (1-sica(i,j))*sensibleo+
     2         sica(i,j)*sensible_coeff(i,j)*(tice0-atemp(i,j))
              sensible_ocn(i,j)=(1.0-sica(i,j))*sensibleo
!latent (atmosphere)
              dflux(3,i,j)=latent_ocn(i,j)-
     1         (1.0-sica(i,j))*latento+
     2         sica(i,j)*rho0*hls*evapsic0
!add in latent from sea ice
              latent_ocn(i,j)=(1.0-sica(i,j))*latento+
     1                         sica(i,j)*qb+
     2                         (1.0-sica(i,j))*max(0.0,qb-netheato)
!evaporation (evap_ocn unchnaged for moisture conservation)
!NB m/s as this is going to PLASIM
              dflux(4,i,j)=evap_ocn(i,j)-
     1         (1.0-sica(i,j))*evapo+
     2         sica(i,j)*evapsic0
             endif

          endif !end ocean only
        enddo
      enddo

!flip sign of flux correction because *into* atmosphere
      dflux=-dflux

      end subroutine surflux_goldstein_seaice

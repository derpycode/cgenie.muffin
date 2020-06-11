* ======================================================================
* goldstein.F
* ======================================================================
*
* AY (01/12/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
*
* RM (16/5/05) : edited for variable sin(lat) resolution
*                ldeg=true/false switches degree/sin lat grid
* 
*                 this code takes pieces from mains.F

      subroutine goldstein(istep,
     :     latent_ocn,sensible_ocn,
     :     netsolar_ocn,netlong_ocn,
     :     fx0sic_ocn,
     :     evap_ocn,pptn_ocn,runoff_ocn,
     :     fwsic_ocn,
     :     stressxu_ocn,stressyu_ocn,
     :     stressxv_ocn,stressyv_ocn,
     :     tsval_ocn,ssval_ocn,
     :     usval_ocn,vsval_ocn,
     :     albedo_ocn,
     :     test_energy_ocean,test_water_ocean,
     :     koverall,
     :     go_ts,go_ts1,
     :     go_cost,go_u,go_tau,
     :     go_psi,
     :     go_mldta,
     :     go_rho,
     :     go_diffv,
     :     go_dzrho)

      use genie_util, ONLY: check_unit, check_iostat

#include "ocean.cmn"

      include 'netcdf_grid.cmn'

c ======================================================================
c Declarations
c ======================================================================

c AY (02/12/03)
c Declare variables passed into this subroutine
c nre 'time' to be added
      integer istep, koverall

      logical flag_igcmatmos
      parameter(flag_igcmatmos=.false.)

      real time,
     :     latent_ocn(imax,jmax),
     :     sensible_ocn(imax,jmax),
     :     netsolar_ocn(imax,jmax),
     :     netlong_ocn(imax,jmax),
     :     fx0sic_ocn(imax,jmax),
     :     evap_ocn(imax,jmax),
     :     pptn_ocn(imax,jmax),
     :     runoff_ocn(imax,jmax),
     :     fwsic_ocn(imax,jmax),
     :     stressxu_ocn(imax,jmax),
     :     stressyu_ocn(imax,jmax),
     :     stressxv_ocn(imax,jmax),
     :     stressyv_ocn(imax,jmax),
     :     tsval_ocn(imax,jmax),
     :     ssval_ocn(imax,jmax),
     :     usval_ocn(imax,jmax),
     :     vsval_ocn(imax,jmax),
     :     albedo_ocn(imax,jmax)
   
c Dummy variables to be passed to/from BIOGEM via genie.F
      real go_ts(1:maxl,1:maxi,1:maxj,1:maxk)
      real go_ts1(1:maxl,1:maxi,1:maxj,1:maxk)
      real go_cost(1:maxi,1:maxj)
      real go_u(1:3,1:maxi,1:maxj,1:maxk)
      real go_tau(1:2,1:maxi,1:maxj)
      real go_psi(0:maxi,0:maxj)
      real go_mldta(1:maxi,1:maxj)
      real go_rho(1:maxi,1:maxj,1:maxk)
      real go_diffv(1:maxi,1:maxj,1:maxk)
      real go_dzrho(1:maxi,1:maxj,1:maxk)

c AY (02/12/03)
c Local variables
      integer i, j, k, l, isl, itv, iout, ios
c      integer isol

      real avn, avs, sums(8*maxl)
c      real tmp1, tmp2, tmp3
c AY (23/01/04) : 'tv' variable needs to be global, not local
c     real tv(maxl,maxi,maxj,maxk), rms
      real rms

c KICO mld variables
      real tv4, tv2, tv3

c Stream function variables
      real opsi(0:maxj,0:maxk)
      real opsia(0:maxj,0:maxk), omina, omaxa
      real opsip(0:maxj,0:maxk), ominp, omaxp
      integer iposa(2)

c Surface flux variables
      real fx0neto(imax,jmax),fwfxneto(imax,jmax)
c      real pme(imax, jmax)

#ifdef clock
c AY (09/03/04) : "time" variables for Dan
      integer it1, it2, it3, it4
      real    rt1, rt2
c AY (17/03/04) : heat and freshwater flux averaging
      real    fx(5), fw(5)
c AY (31/03/04) : wind stress averaging
      real    ws(5)
#endif

c AY (02/04/04) : extra fields for flux and wind stress passing
      real    fx0flux(5,maxi,maxj), fwflux(4,maxi,maxj)
      real    wstress(4,maxi,maxj)

c AY (17/03/04) : extra netCDF variable
      real    work((maxi+1)*(maxj+1)*(maxk+1))

c AY (29/11/04) : heat and freshwater time-series fluxes
      real    fw_flx(5), fx_flx(5), t_area(5), r_itstp

      character ext*3, conv*3

c     FOR THE ENERGY CALCULATIONS.....
      real    tot_energy,tot_water
      real    ini_energy,ini_water
      save ini_energy,ini_water
      real vsc
      integer ifirst
      data ifirst/1/
      save ifirst
      real test_energy_ocean,test_water_ocean

c ======================================================================
c DJL
c This bit just calculates the inital diagnostic of the total ocean 
c   thermal energy and water.  It is simply the temperature/density of 
c   each ocean gridbox multiplied by its vertical depth. 
c The water bit has units of kg
c Note the -ve sign in the water part!!
c The energy bit has units of J

c      First of all, set up this constant ....
       if (ifirst.eq.1) then 
crma       vsc = ds*dphi*rsc*rsc
       vsc = dphi*rsc*rsc
       ini_energy=0.0
       ini_water=0.0
       do k=1,kmax
         do j=1,jmax
           do i=1,imax
crma             ini_energy=ini_energy+ts(1,i,j,k)*dz(k)
crma             ini_water=ini_water-ts(2,i,j,k)*dz(k)
             ini_energy=ini_energy+ts(1,i,j,k)*dz(k)*ds(j)
             ini_water=ini_water-ts(2,i,j,k)*dz(k)*ds(j)
           enddo
         enddo
       enddo
c      The m2mm is because internally, goldstein uses m, 
c        but genie uses mm.
       ini_energy=ini_energy*vsc*dsc*rh0sc*cpsc
       ini_water=m2mm*ini_water*vsc*dsc/saln0
       ifirst=0
       endif
c ======================================================================

c ======================================================================
c Input field modifications
c ======================================================================

c AR -- 14/08/29
c pass back in BIOGEM-reset running count of cost
      do j=1,jmax
         do i=1,imax
            cost(i,j) = go_cost(i,j)
         enddo
      enddo 

c AY (02/12/03)
c Insert any changes to units, etc. of incoming fields here

c e.g. non-dimensionalisation
c e.g. scl_drg modification to wind fields

c     if (flag_igcmatmos.or.istep.eq.1) then
c     if (istep.eq.1) then

c AY (19/12/03) : need to calculate wind stresses first
c                 code below taken from gseto.F
         do j=1,jmax
            do i=1,imax

               dztau(1,i,j) = scf*stressxu_ocn(i,j)
     1              /(rh0sc*dsc*usc*fsc)/dzz
               dztau(2,i,j) = scf*stressyu_ocn(i,j)
     1              /(rh0sc*dsc*usc*fsc)/dzz
               dztav(1,i,j) = scf*stressxv_ocn(i,j)
     1              /(rh0sc*dsc*usc*fsc)/dzz
               dztav(2,i,j) = scf*stressyv_ocn(i,j)
     1              /(rh0sc*dsc*usc*fsc)/dzz
               tau(1,i,j) = dztau(1,i,j)*dzz
               tau(2,i,j) = dztav(2,i,j)*dzz

c AY (02/04/04) : stresses copied for output routines
               wstress(1,i,j) = stressxu_ocn(i,j)
               wstress(2,i,j) = stressyu_ocn(i,j)
               wstress(3,i,j) = stressxv_ocn(i,j)
               wstress(4,i,j) = stressyv_ocn(i,j)
            enddo
         enddo

c KICO wind calculations needed for mld
      if(imld.eq.1)then
c Following code taken from genie-embm/src/fortan/surflux.F
c but min(j,jmax-1): avoids effect of N Pole singularity
        do j=1,jmax
           tv3 = 0.
           do i=1,imax
              if(i.eq.1) then
                 tv4 = (tau(1,i,j)+tau(1,imax,j))*0.5
              else
                 tv4 = (tau(1,i,j)+tau(1,i-1,j))*0.5
              endif
              if(j.eq.1) then
                 tv2 = tau(2,i,j)*0.5
              else
                 tv2 = (tau(2,i,min(j,jmax-1))+
     1                    tau(2,i,j-1))*0.5
              endif

c KICO: wind stress used for mld calcs is consistent with
c that for ocean advection, NOT for surface heat fluxes.
c Only matters if physically interpreting mldketaucoeff
              mldketau(i,j) = mldketaucoeff*(sqrt(sqrt(
     1                         tv4**2 + tv2**2)))**3
              tv3 = tv3 + mldketau(i,j)
           enddo
           do i=1,imax
              if(j.le.2.or.j.ge.jmax-1)mldketau(i,j)
     1                                = tv3/imax
           enddo
        enddo
      endif

c     endif

c RMA (3/5/06)
crma call new routine for extra freshwater forcing (hosing)

      call get_hosing(istep)

c AY (10/12/03) : implemented
crma combine individual components of heat and freshwater fluxes
crma into net fluxes needed by tstepo/tstipo, using lines scavanged
crma from old surflux.F:

c AY (10/12/03) : sea-ice updating not complete - should really be
c                 done in sea-ice module, not here
c modify heat and salinity fluxes into ocean according to sea-ice update
c prevent <0%, >100% fractional area A and set A=H=0 if H<Hmin
c in which case add an amount -H of ice, hence need to add appropriate
c heat and freshwater fluxes.

      do j=1,jmax
         do i=1,imax

c *** Heat flux ***

            fx0neto(i,j) = netsolar_ocn(i,j) + sensible_ocn(i,j)
     1           + netlong_ocn(i,j) + latent_ocn(i,j)
     2           + fx0sic_ocn(i,j)

c AY (02/04/04) : fluxes copied for output routines
            fx0flux(1,i,j) = netsolar_ocn(i,j)
            fx0flux(2,i,j) = sensible_ocn(i,j)
            fx0flux(3,i,j) = netlong_ocn(i,j)
            fx0flux(4,i,j) = latent_ocn(i,j)
            fx0flux(5,i,j) = fx0sic_ocn(i,j)

c *** Freshwater flux ***
c AY (21/01/04) : now need to add in fluxes from sea ice
c RMA (3/05/06) : added optional hosing
c RMA (10/08/06) : added optional FW flux anomalies from CMIP model

            fwfxneto(i,j) = pptn_ocn(i,j) + evap_ocn(i,j)
     1           + runoff_ocn(i,j)
     2           + fwsic_ocn(i,j)
     3           + fw_hosing(i,j)
     4           + fw_anom(i,j)
c AY (23/07/04) : convert freshwater fluxes from mm/s to m/s
            fwfxneto(i,j) = fwfxneto(i,j) * mm2m

c AY (02/04/04) : fluxes copied for output routines
            fwflux(1,i,j) = pptn_ocn(i,j)
            fwflux(2,i,j) = evap_ocn(i,j)
            fwflux(3,i,j) = runoff_ocn(i,j)
            fwflux(4,i,j) = fwsic_ocn(i,j)

c AY (10/12/03) : put these fluxes into correct array location for
c                 tstepo/tstipo

            ts(1,i,j,kmax+1) = - fx0neto(i,j)*rfluxsc
            ts(2,i,j,kmax+1) = fwfxneto(i,j)*rpmesco
            ts1(1,i,j,kmax+1) = ts(1,i,j,kmax+1)
            ts1(2,i,j,kmax+1) = ts(2,i,j,kmax+1)
            
         enddo
      enddo
      
c     From biogem.....
c     input biogeochemical tracers arrays
c     don't let anyone else change Temperature and Salinity !!
c     NO - let BIOGEM change Temperature and Salinity
c     it will do it ever so nicely and gently ... ;)
      do k=1,kmax
         do j=1,jmax
            do i=1,imax
               do l=1,lmax
                  ts(l,i,j,k)  = go_ts(l,i,j,k)
                  ts1(l,i,j,k) = go_ts1(l,i,j,k)
               enddo
            enddo
         enddo
      enddo 
c     create b.c. [12/01/2014]
c     NOTE: BIOGEM only passes in/out the core grid array
c           (no boundaries) and does not update tracers at boundaries
      do j=1,jmax
         do k=k1(0,j),kmax
            do l=1,lmax
               ts1(l,0,j,k) = ts(l,imax,j,k)
            enddo
         enddo
         do k=k1(imax+1,j),kmax
            do l=1,lmax
               ts1(l,imax+1,j,k) = ts(l,1,j,k)
            enddo
         enddo
      enddo

c ======================================================================
c GOLDSTEIN model timestep
c ======================================================================

c AY (09/03/04) : Write out current time (days, years, etc.) if required
#ifdef clock
 120  format(a26,i8,a6,i6,a5,f9.4)
 121  format(a12,a4,e14.6,a4,e14.6,a4,e14.6,a4,e14.6,a4,e14.6)
 122  format(a12,a4,e14.6,a4,e14.6,a4,e14.6,a4,e14.6)

      it1 = istep - 1
      it2 = mod(it1, nyear)
      it3 = it1 - it2
      it4 = it3 / nyear
      rt1 = yearlen/nyear
      rt2 = real(it2)*rt1
      write(*,120) 'GOLDSTEIN time : iteration',istep,', year',it4,
     +     ', day',rt2

c AY (17/03/04) : add in flux averaging to assist model diagnosis
      do l = 1,5
         fx(l) = 0.
         fw(l) = 0.
         ws(l) = 0.
      enddo
c
      do j=1,jmax
         do i=1,imax
c *** Heat flux ***
            fx(1) = fx(1) + netsolar_ocn(i,j)
            fx(2) = fx(2) + sensible_ocn(i,j)
            fx(3) = fx(3) + netlong_ocn(i,j)
            fx(4) = fx(4) + latent_ocn(i,j)
            fx(5) = fx(5) + fx0sic_ocn(i,j)
c *** Freshwater flux ***
            fw(1) = fw(1) + pptn_ocn(i,j)
            fw(2) = fw(2) + evap_ocn(i,j)
            fw(3) = fw(3) + runoff_ocn(i,j)
            fw(4) = fw(4) + fwsic_ocn(i,j) 
c *** Wind stress ***
            ws(1) = ws(1) + stressxu_ocn(i,j)
            ws(2) = ws(2) + stressyu_ocn(i,j)
            ws(3) = ws(3) + stressxv_ocn(i,j)
            ws(4) = ws(4) + stressyv_ocn(i,j)
         enddo
      enddo
c     
      write(*,121) 'HT fluxes : ',
     +     'Sol',fx(1)/(imax*jmax),'Sen',fx(2)/(imax*jmax),
     +     'NLo',fx(3)/(imax*jmax),'Lat',fx(4)/(imax*jmax),
     +     'Ice',fx(5)/(imax*jmax)
      write(*,122) 'FW fluxes : ',
     +     'Ppt',fw(1)/(imax*jmax),'Evp',fw(2)/(imax*jmax),
     +     'Run',fw(3)/(imax*jmax),'Ice',fw(4)/(imax*jmax)
      write(*,122) 'Winds     : ',
     +     'U-X',ws(1)/(imax*jmax),'U-Y',ws(2)/(imax*jmax),
     +     'V-X',ws(3)/(imax*jmax),'V-Y',ws(4)/(imax*jmax)
#endif

c AY (02/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------

c Ocean momentum
c ----------------------------------------------------------------------

c AY (03/12/03) : This next section executes routines that calculate
c                 wind-driven circulation.  If the winds are fixed,
c                 as they are with the EMBM, this only needs be run
c                 just prior to the first iteration.  If the winds
c                 are time-variant, as they are with the IGCM, this
c                 will need to be run every time-step.  Hence the
c                 use of flags.

c AY (05/02/04) : code now runs these routines regardless (doesn't
c	          seem to slow things up too much when the EMBM is
c	          being used)

         call wind
         call jbar
         call ubarsolv(ub,psi)

c find island path integral due to wind and jbar terms 

         do isl=1,isles
            call island(ub,erisl(isl,isles+1),isl,1)
         enddo

c solve system of simultaneous equations. Zero division here might 
c suggest not enough islands in the .psiles file
c rma changes (introducing call matmult) made 20/9/05
c to artificially set flows around islands to zero set psibc() to zero
         if (isles > 1) then
            call matmult(isles,erisl,erisl(1,isles+1))
            do isl=1,isles
               psibc(isl) = - erisl(isl,isles+1)
            enddo
         elseif (isles > 0) then
            psibc(1) = - erisl(1,2)/erisl(1,1)
         else
c NO ISLANDS!!
         endif

         do j=1,jmax
            do i=0,imax+1
               do isl=1,isles
                  ub(1,i,j) = ub(1,i,j) + ubisl(1,i,j,isl)*psibc(isl)
                  ub(2,i,j) = ub(2,i,j) + ubisl(2,i,j,isl)*psibc(isl)
               enddo
            enddo
         enddo
c
c update diagnostic psi, not always necessary
c
         do j=0,jmax
            do i=0,imax
               do isl=1,isles
                  psi(i,j) = psi(i,j) + psisl(i,j,isl)*psibc(isl)
               enddo
            enddo
         enddo
         
c     endif

c update velocities

      call velc

c AY (12/07/05) : removed surplus input arguments to tstipo/tstepo
#ifdef dimpo 
      call tstipo
#else 
      call tstepo
#endif

c ======================================================================
c Ocean diagnostics and output
c ======================================================================

c     t = istep*dt(kmax) + t0
      
c "Healthcheck" and model rate of change
c ----------------------------------------------------------------------
      if(mod(istep,npstp).eq.0)then
         if (debug_loop) print*,'step ',istep 
         if (debug_loop) 
     & print*,'psi on islands ',(psibc(isl),isl=1,isles)
         if (debug_loop) call diag
         if(mod(istep,npstp).eq.0)then
            do k=1,kmax
               do j=1,jmax
                  do i=1,imax
                     do l=1,lmax
                        ts_store(l,i,j,k) = ts(l,i,j,k)
                     enddo
                  enddo
               enddo
            enddo
         else if(mod(istep,npstp).eq.1.and.istep.gt.1)then
c        open(7,file='../results/tmp.1')
            rms = 0.
            do j=1,jmax
               do i=1,imax
                  do k=1,kmax
                     do l=1,lmax
                        rms = rms + (ts_store(l,i,j,k) - ts(l,i,j,k))**2
                     enddo
                  enddo
               enddo
            enddo
            rms = sqrt(rms/lmax/ntot/dt(kmax)/dt(kmax))
            if (debug_loop) print*,'r.m.s. r.o.c.',rms
         endif
         if (debug_loop) print*
      endif

c Model restart (ASCII and netCDF)
c ----------------------------------------------------------------------
c+DJL 31/8/2004
      if (lnetout) then
           call outm_netcdf(istep)
      endif
c-DJL 31/8/2004

      if(mod(istep,iwstp).eq.0)then

         ext=conv(mod(iw,10))
c write GOLDSTEIN restart file
c AY (03/12/03)

c+DJL 31/8/2004
        if (lascout) then
         if (debug_loop) 
     + print*,'Writing GOLDSTEIN restart file at time',istep,
     + '(koverall',koverall,')'
         call check_unit(12,__LINE__,__FILE__)
         open(12,file=outdir_name(1:lenout)//lout//'.'//ext,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         rewind 12
         call outm(12)
         close(12,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
        endif
c-DJL 31/8/2004

c Oscillating streamfunction
c ----------------------------------------------------------------------
         if(.not.flat)then
c AY (03/12/03)
c           open(2,file='../results/'//lout//'.psi.'//ext)
            if (debug_loop) 
     & print*,'Writing GOLDSTEIN oscillating streamfunction ',
     +          'file at time',istep
            open(2,file=outdir_name(1:lenout)//lout//
     1           '.psi.'//ext,iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)
            do j=0,jmax
               do i=0,imax
                  if (debug_loop) write(2,*,iostat=ios)psi(i,j)
                  call check_iostat(ios,__LINE__,__FILE__)
               enddo
            enddo   
            close(2,iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)
         endif
         iw = iw + 1
         if (debug_loop) print*
      endif

c Time-series outputs (T, S, THC, FW flux)
c ----------------------------------------------------------------------
c AY (17/03/04) : format statements for time-series files of average 
c                 T & S and overturning streamfunctions 
 100  format(5e14.6,2i5)
 110  format(11e14.6)
 120  format(11e14.6)

      if(mod(istep,itstp).eq.0)then
c AY (05/05/04) : temporary "time" variable (until we sort it out at the
c                 genie.F level)
         time = real(real(istep)/real(nyear))
c AY (08/12/03) : files opened so that data can be appended to the end
c                 of them (need f90 compilation for this)
         if (debug_loop) 
     & print*,'Writing GOLDSTEIN time-series files at time',istep
         call check_unit(4,__LINE__,__FILE__)
         open(4,file=outdir_name(1:lenout)//lout//'.'//'t',
     1        status='old',position='append',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         open(14,file=outdir_name(1:lenout)//lout//'.'//'s',
     1        status='old',position='append',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         open(40,file=outdir_name(1:lenout)//lout//'.'//'opsit',
     1        status='old',position='append',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         call diag2(sums,avn,avs)
         if (debug_loop) 
     1 write(4,110,iostat=ios)time,(sums(i),i=1,8),avn,avs
         call check_iostat(ios,__LINE__,__FILE__)
         if (debug_loop) 
     1 write(14,110,iostat=ios)time,(sums(i),i=9,16),avn,avs
         call check_iostat(ios,__LINE__,__FILE__)
         call diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip,iposa)
c AY (04/03/04) : OPSISC factor scales streamfunctions into Sv
c AY (05/05/04) : location of Atlantic max overturning now recorded
         if (debug_loop) 
     1 write(40,100,iostat=ios)time,ominp*opsisc,omaxp*opsisc,
     2        omina*opsisc,omaxa*opsisc,iposa(1),iposa(2)
         call check_iostat(ios,__LINE__,__FILE__)
         close(4,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         close(14,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         close(40,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         if (debug_loop) print*
      endif

c AY (29/11/04) : adding in FW (and heat) flux (global, Atlantic, Pac)
c                 note : averaged over itstp timesteps
      if (istep.eq.1) then
         do l=1,5
            fw_flx(l) = 0.
            fx_flx(l) = 0.
            t_area(l) = 0.
         enddo
      endif
c
      do j=1,jmax
         do i=1,imax
c           Ocean cells only
            if (k1(i,j).le.kmax) then
c              Global ocean
               fw_flx(1) = fw_flx(1) + (fwfxneto(i,j)*asurf(j))
               fx_flx(1) = fx_flx(1) + (fx0neto(i,j)*asurf(j))
               t_area(1) = t_area(1) + asurf(j)
c              Atlantic basin cells
               if (j.gt.jsf.and.i.ge.ias(j).and.i.le.iaf(j)) then
                  fw_flx(2) = fw_flx(2) + (fwfxneto(i,j)*asurf(j))
                  fx_flx(2) = fx_flx(2) + (fx0neto(i,j)*asurf(j))
                  t_area(2) = t_area(2) + asurf(j)
c              Pacific basin cells
               elseif (j.gt.jsf.and.i.ge.ips(j).and.i.le.ipf(j)) then
                  fw_flx(3) = fw_flx(3) + (fwfxneto(i,j)*asurf(j))
                  fx_flx(3) = fx_flx(3) + (fx0neto(i,j)*asurf(j))
                  t_area(3) = t_area(3) + asurf(j)
c              Indian basin cells
               elseif (j.gt.jsf.and.(i.lt.ips(j).or.i.gt.iaf(j))) then
                  fw_flx(4) = fw_flx(4) + (fwfxneto(i,j)*asurf(j))
                  fx_flx(4) = fx_flx(4) + (fx0neto(i,j)*asurf(j))
                  t_area(4) = t_area(4) + asurf(j)
c              Southern basin cells
               else
                  fw_flx(5) = fw_flx(5) + (fwfxneto(i,j)*asurf(j))
                  fx_flx(5) = fx_flx(5) + (fx0neto(i,j)*asurf(j))
                  t_area(5) = t_area(5) + asurf(j)
               endif
            endif
         enddo
      enddo
c
      if(mod(istep,itstp).eq.0)then
c
c Open flux file
         call check_unit(41,__LINE__,__FILE__)
         open(41,file=outdir_name(1:lenout)//lout//'.'//'flux',
     1        status='old',position='append',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c
c Create real version of itstp
         r_itstp = real(itstp)
c
c Write out regional areas (m2) if this is the first timestep
         if(istep.eq.itstp) then
            if (debug_loop) write(41,120,iostat=ios) 0.0,
     :           t_area(1)/r_itstp, t_area(2)/r_itstp,
     :           t_area(3)/r_itstp, t_area(4)/r_itstp,
     :           t_area(5)/r_itstp,
     :           t_area(1)/r_itstp, t_area(2)/r_itstp,
     :           t_area(3)/r_itstp, t_area(4)/r_itstp,
     :           t_area(5)/r_itstp
            call check_iostat(ios,__LINE__,__FILE__)
         endif
c
c Write out freshwater and heat fluxes
         if (debug_loop) write(41,120,iostat=ios) time, 
     :        fw_flx(1)/(1.e6*r_itstp), fw_flx(2)/(1.e6*r_itstp),
     :        fw_flx(3)/(1.e6*r_itstp), fw_flx(4)/(1.e6*r_itstp),
     :        fw_flx(5)/(1.e6*r_itstp),
     :        fx_flx(1)/(1.e15*r_itstp), fx_flx(2)/(1.e15*r_itstp), 
     :        fx_flx(3)/(1.e15*r_itstp), fx_flx(4)/(1.e15*r_itstp), 
     :        fx_flx(5)/(1.e15*r_itstp)
         call check_iostat(ios,__LINE__,__FILE__)
c Average heat flux (W/m2)
c    :        fx_flx(1)/t_area(1), fx_flx(2)/t_area(2), 
c    :        fx_flx(3)/t_area(3), fx_flx(4)/t_area(4),
c    :        fx_flx(5)/t_area(5)
         close(41,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c
c Empty averaging arrays
         do l=1,5
            fw_flx(l) = 0.
            fx_flx(l) = 0.
            t_area(l) = 0.
         enddo
c
      endif

c Averaging of model state
c ----------------------------------------------------------------------
      if (dosc) then
c average the last nyear steps in every ianav steps (if ianav>nyear)

      itv = mod(istep+nyear-1,ianav)
      if(itv.lt.nyear)then
         ext=conv(mod(iav,10))
         if(istep.ge.nyear.and.itv.eq.nyear-1)then
            iout = 1
         else
            iout = 0
         endif
         if (debug_loop) 
     :      call diagosc(istep, iout, ext, fx0flux, fwflux, wstress)
      endif
      endif

c Defunct model output (instantaneous version of averaging above)
c ----------------------------------------------------------------------
c AY (17/03/04) : netCDF writing code (from Paul and Dan)
      if (lnetout) then
      if(mod(istep,iwstp).eq.0) then
         if (debug_loop) 
     : print*,'Writing GOLDSTEIN netCDF file at time',istep
c
c        get streamfunction data
         call diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip,iposa)
c
c        do netCDF stuff ...
         call ini_netcdf_ocn(istep,1)
c
         call write_netcdf_ocn(imax, jmax, kmax, k1, ncdepth1,
     :        opsi, opsia, opsip,
     :        ts, u, rho, 
     :        fx0flux, fwflux,
     :        work,
     :        dsc, usc, rsc, saln0,
     :        maxi, maxj, maxk, maxl, 1)
c
         call end_netcdf_ocn(1)
         if (debug_loop) print*
      endif
      endif

c Output arguments
c ----------------------------------------------------------------------

c AY (09/12/03) : These arguments are required in other modules
c           Sea surface temperature [-> surface fluxes]
c           Sea surface salinity [-> surface fluxes]
c           Surface velocity (u component) [-> sea-ice]
c           Surface velocity (v component) [-> sea-ice]
c           Ocean (planetary) albedo (calculated in initialise_ocean.F)
c
c AY (13/07/04) : The argument list has been altered
c           Albedo is now handled via the surf_ocn_sic.F routine
c           "Average" temperature (ocean and sea-ice) is calculated
c           here for the IGCM3's long-wave calculations

      do j=1,jmax
         do i=1,imax
            usval_ocn(i,j) = real(u(1,i,j,kmax))
            vsval_ocn(i,j) = real(u(2,i,j,kmax))
            if (k1(i,j).le.kmax) then
               tsval_ocn(i,j) = real(ts(1,i,j,kmax))
               ssval_ocn(i,j) = real(ts(2,i,j,kmax))
c AY (10/09/04) : still need albedo output however
            albedo_ocn(i,j) = real(albocn)
c
c AY (10/09/04) : all of the following code excised - calculating
c                 average albedo and "average" temperature moved to
c                 genie.F
c
c-----------------------------------------------------------------------
c              set up average albedo
c-----------------------------------------------------------------------
c              albedo_ocn(i,j) = sica(i,j)*alb_sic(i,j) + 
c    :              (1-sica(i,j))*alb_ocn(i,j)
c
c-----------------------------------------------------------------------
c           set up weighted average temperature
c-----------------------------------------------------------------------
c              tmp1 = (ts1(1,i,j,kmax)+zeroc)**4
c              tmp2 = (tice(i,j)+zeroc)**4
c              tmp3 = ((1 - sica(i,j))*tmp1) + (sica(i,j) * tmp2)
c              temp_net(i,j) = (tmp3**0.25) - zeroc

            else
               tsval_ocn(i,j) = 0.
               ssval_ocn(i,j) = 0.
               albedo_ocn(i,j) = 0.
            endif
         enddo
      enddo

c Set values of dummy variables destined for BIOGEM
c NOTE: convert precision between GOLDSTEIn (real*8) and GENIE 
c       (which can be real*4 or real* depending on the IGCM)
      do j=1,jmax
         do i=1,imax
            do k=1,kmax
               do l=1,lmax
                  go_ts(l,i,j,k)  = real(ts(l,i,j,k))
                  go_ts1(l,i,j,k) = real(ts1(l,i,j,k))
               enddo
               go_u(1,i,j,k) = real(u(1,i,j,k))
               go_u(2,i,j,k) = real(u(2,i,j,k))
               go_u(3,i,j,k) = real(u(3,i,j,k))
c SG > Passing rho to ents
               go_rho(i,j,k) = real(rho(i,j,k))
               go_diffv(i,j,k) = real(diffv_test(i,j,k))
               go_dzrho(i,j,k) = real(dzrho_test(i,j,k))
c SG <
            enddo
            go_cost(i,j)  = real(cost(i,j))
            go_tau(1,i,j) = real(tau(1,i,j))
            go_tau(2,i,j) = real(tau(2,i,j))
            go_mldta(i,j) = real(-dsc*mld(i,j))
ccSR

         enddo
      enddo 
      do j=0,jmax
         do i=0,imax
            go_psi(i,j) = opsisc*real(psi(i,j))
         enddo
      enddo 

c ======================================================================
c DJL
c This bit just calculates the diagnostic of the total ocean 
c   thermal energy and water.  It is simply the temperature/density of 
c   each ocean gridbox multiplied by its vertical depth. 
c The water bit has units of kg, relative to an initial value.
c Note the -ve sign in the water part!!
c The energy bit has units of J, relative to an initial value.

       if (mod(istep,conserv_per).eq.0) then
crma       vsc = ds*dphi*rsc*rsc
       vsc = dphi*rsc*rsc
       tot_energy=0.0
       tot_water=0.0
       do k=1,kmax
         do j=1,jmax
           do i=1,imax
crma             tot_energy=tot_energy+ts(1,i,j,k)*dz(k)
crma             tot_water=tot_water-ts(2,i,j,k)*dz(k)
             tot_energy=tot_energy+ts(1,i,j,k)*dz(k)*ds(j)
             tot_water=tot_water-ts(2,i,j,k)*dz(k)*ds(j)
           enddo
         enddo
       enddo
c      The m2mm is because internally, goldstein uses m, 
c        but genie uses mm.
       tot_energy=tot_energy*vsc*dsc*rh0sc*cpsc
       tot_water=m2mm*tot_water*vsc*dsc/saln0
       test_energy_ocean=real(tot_energy-ini_energy)
       test_water_ocean=real(tot_water-ini_water)
       if (debug_loop) 
     :    print*,'Goldstein energy diagnostic: ',test_energy_ocean
       if (debug_loop) 
     :    print*,'Goldstein water diagnostic: ',test_water_ocean
       endif
c ======================================================================

* ======================================================================
* end of goldstein.F
* ======================================================================

      end

* ======================================================================
* conv function (called within goldstein.F)
* ======================================================================

      character*3 function conv(i)

      character*1 a,b,c
      integer i, i1, i2, i3, itemp
      
      if(i.lt.10)then
        a=char(i+48)
        conv=a//'  '
      else if(i.lt.100)then
        i1=i/10
        i2=i-i1*10
        a=char(i1+48)
        b=char(i2+48)
        conv=a//b//' '
      else
        i1=i/100
        itemp=i-100*i1
        i2=itemp/10
        i3=itemp-10*i2
        a=char(i1+48)
        b=char(i2+48)
        c=char(i3+48)
        conv=a//b//c
      endif
      end



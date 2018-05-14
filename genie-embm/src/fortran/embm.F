* ======================================================================
* embm.F
* ======================================================================
*
* mains.F main program for thermocline equation model goldstein
* variable depth 3/5/95 
* extra outputs for global version 19/4/00
* #ifdef's added so that mains.f -> mains.F 6/5/2
* #ifdef's added for BioGeM 16/09/03 ajr
* #ifdefs added for land 25/09/03 pph
* 
* AY (04/12/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
* 
*                 this code takes pieces from mains.F

c nre time to be added to argument list, but type defined in embm.cmn

      subroutine embm(istep,
     :     latent_atm,sensible_atm,netsolar_atm,netlong_atm,
     :     evap_atm,pptn_atm,
     :     stressxu_atm,stressyu_atm,
     :     stressxv_atm,stressyv_atm,
     :     tstar_atm,qstar_atm,
     :     koverall,
     :     torog_atm,                        !< surface air temperature adjusted for orography
     :     surf_orog_atm,                    !< orography (m)
     :     flag_ents,
     :     lowestlu2_atm,                    !< zonal component of wind speed on u grid
     :     lowestlv3_atm                     !< meridional component of wind speed on v grid
     :     )

      use genie_util, ONLY: check_unit, check_iostat

#include "embm.cmn"
      include 'netcdf_grid.cmn'

c ======================================================================
c Declarations
c ======================================================================

c AY (04/12/03)
c Declare variables passed into this subroutine
      real
     :     latent_atm(imax,jmax),
     :     sensible_atm(imax,jmax),
     :     netsolar_atm(imax,jmax),
     :     netlong_atm(imax,jmax),
     :     pptn_atm(imax,jmax),
     :     evap_atm(imax,jmax),
     :     stressxu_atm(imax,jmax),
     :     stressyu_atm(imax,jmax),
     :     stressxv_atm(imax,jmax),
     :     stressyv_atm(imax,jmax),
     :     tstar_atm(imax,jmax),
     :     qstar_atm(imax,jmax)

      integer istep, koverall

c AY (02/12/03)
c Local variables
      integer i, j, itv, iout, ios

      real fx0neta, fwfxneta

c AY (03/05/04) : time-series variables
      real sum1(4), sum2(4)
      integer isum1(4), isum2(4)

c AY (09/01/04) : testing variables
c      real avgFX

c AY (09/03/04) : "time" variables for Dan
#ifdef clock
      integer it1, it2, it3, it4
      real    rt1, rt2
#endif
      real    t

c AY (29/04/04) : extra fields for flux passing
      real    fx0flux(4,maxi,maxj), fwflux(2,maxi,maxj)

c AP (03/08/06) : dummy qdry field for call to write_netcdf_embm
      real    qdrydum(maxi,maxj)

c AY (22/03/04) : extra netCDF variable
      real work((maxi+1)*(maxj+1))

c AY (09/01/04) : added ext, removed lout, changed conv to conv_embm
      character ext*3, conv_embm*3

c KICO (07/12/06)
      real wateratm

      real::tatm
      real,intent(out)::torog_atm(imax,jmax)
      real,intent(in)::surf_orog_atm(imax,jmax)

c ENTS functionality
      logical flag_ents
      real deltq

      real qsat

c zonal and meridional wind speed components
      real,dimension(maxi,maxj),intent(inout)::lowestlu2_atm,
     &     lowestlv3_atm

c ======================================================================
c Input field modifications
c ======================================================================

c AY (04/12/03)
c Insert any changes to units, etc. of incoming fields here

c AY (05/02/04) : iteration number's meaning has changed - now refers
c	          to genie.f iteration number, rather than the ocean
c	          timestep that c-GOLDSTEIN uses
c AY (16/12/03) : combine individual components of heat and freshwater
c	          fluxes into net fluxes needed by tstepa/tstipa,
c	          using lines scavanged from old surflux.F

      do j=1,jmax
         do i=1,imax

c reset internal atmospheric wind fields
            uatm(1,i,j) = lowestlu2_atm(i,j)/usc
            uatm(2,i,j) = lowestlv3_atm(i,j)/usc
   
c *** Heat flux ***

c net heat flux into atmosphere
c
            fx0neta = netsolar_atm(i,j) + latent_atm(i,j)
     1           + sensible_atm(i,j) + netlong_atm(i,j)

c AY (29/04/04) : fluxes copied for output routines
            fx0flux(1,i,j) = netsolar_atm(i,j)
            fx0flux(2,i,j) = sensible_atm(i,j)
            fx0flux(3,i,j) = netlong_atm(i,j)
            fx0flux(4,i,j) = latent_atm(i,j)

c *** Freshwater flux ***

c AY (27/01/04) : need to remove precip from this calculation to check
c	          that it's not the reason for the persistent difference
c	          between c-GOLDSTEIN and GENIE - precip is now handled
c	          by an explicit change to tq and tq1 in SURFLUX.
c AY (02/02/04) : above change reversed (then reversed again!)
c           fwfxneta = evap_atm(i,j) + pptn_atm(i,j)
            fwfxneta = evap_atm(i,j)

c AY (23/07/04) : convert freshwater fluxes from mm/s to m/s
            fwfxneta = fwfxneta * mm2m

c AY (29/04/04) : fluxes copied for output routines (note : leaving in
c                 precip for now, even though it's not used here)
            fwflux(1,i,j) = pptn_atm(i,j)
            fwflux(2,i,j) = evap_atm(i,j)

c non-dimensionalize surface fluxes for use in tstepa:

            tqa(1,i,j) = (fx0neta * rfluxsca)
            tqa(2,i,j) = (fwfxneta * rpmesca)
         enddo
      enddo

c ======================================================================
c EMBM model timestep
c ======================================================================

c AY (09/03/04) : Write out current time (days, years, etc.) if required
#ifdef clock
 120  format(a26,i8,a6,i6,a5,f9.4)

      it1 = istep - 1
      it2 = mod(it1, (nyear*ndta))
      it3 = it1 - it2
      it4 = it3 / (nyear*ndta)
      rt1 = yearlen /(nyear*ndta)
      rt2 = real(it2)*rt1
      if (debug_loop)
     &  write(*,120) 'EMBM time : iteration',istep,', year',it4,
     +     ', day',rt2

#endif

c AY (04/12/03) : previously in mains.F ...
c ----------------------------------------------------------------------

c EMBM update 1-layer atmosphere

c AY (04/12/03) : remove looping here (happens in genie.f)
c        do natm = 1,ndta
#ifdef dimpa
      call tstipa
#else 
      call tstepa
#endif
c

c     Diagnostic fields of precipitation-adjusted humidity (i.e.,
c     humidity after precipitation)
c     calculate saturation humidity in line with 'surflux.F' for
c     diagnostics ('surflux.F' will update it again) and calculate
c     specific and relative humidity after adjustment for precipitation
c     has been made
      do j=1,jmax
         do i=1,imax
      if(orogswitch.ge.1)then
         tatm=tq(1,i,j)+(lapse_rate*surf_orog_atm(i,j))
      else
         tatm=tq(1,i,j)
      endif
            if((orogswitch.lt.2).and.(flag_ents)) then
               qsat = const1*exp(const4*tatm
     1              /(tatm+const5))
            else
               qsat = const1*exp(const4*tq(1,i,j)
     1              /(tq(1,i,j)+const5))
            endif
            if (flag_ents) then
               deltq = lambdapptn*(tq(2,i,j)-(rmax*qsat))
               q_pa(i,j) = min(tq(2,i,j),
     &              tq(2,i,j)-deltq)
            else
               q_pa(i,j) = min(tq(2,i,j),
     &              rmax*qsat)
            endif
            rq_pa(i,j) = q_pa(i,j)/qsat
         enddo
      enddo

      if(mod(istep,(npstp*ndta)).lt.1) then
         if (debug_loop) call diaga
         if (debug_loop) print*
      endif
c        enddo
        
c Atmosphere diagnostics and output
c ----------------------------------------------------------------------

c AY (16/12/03) : required in diagosc (previously from mains.F)
c                 this should really be passed to relevant routines
c     t = ((istep*dt(kmax))/real(ndta)) + t0

c AY (05/10/04) : netCDF output added
      if (lnetout) then
           call outm_netcdf_embm(istep)
      endif

c write EMBM restart file
      if(mod(istep,(iwstp*ndta)).eq.0)then
         if (lascout) then
            ext=conv_embm(mod(iw,10))
            if (debug_loop) print*,'Writing EMBM restart file at time',
     +           istep/real(ndta),'(koverall',koverall,')'
            call check_unit(2,__LINE__,__FILE__)
            open(2,file=outdir_name(1:lenout)//lout//'.'//ext,
     &           iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)
            rewind 2
            call outm_embm(2)
            close(2,iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)
         endif

         iw = iw + 1
         if (debug_loop) print*
      endif

c AY (29/04/04) : format statements for time-series files of average 
c                 air temperature and specific humidity
 110  format(4e15.6,2i5,1e15.6,2i5)

      if(mod(istep,(itstp*ndta)).eq.0)then
c AY (05/05/04) : temporary "time" variable (until we sort it out at the
c                 genie.F level)
         t = real(istep)/real(nyear*ndta)
c AY (16/12/03) : files opened so that data can be appended to the end
c                 of them (need f90 compilation for this)
         if (debug_loop) print*,'Writing to EMBM time-series files'

         call check_unit(41,__LINE__,__FILE__)
         open(41,file=outdir_name(1:lenout)//lout//'.'//'airt',
     1        status='old',position='append',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

         call check_unit(42,__LINE__,__FILE__)
         open(42,file=outdir_name(1:lenout)//lout//'.'//'q',
     1        status='old',position='append',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

         call diag3(sum1,sum2,isum1,isum2)

         if (debug_loop) 
     & write(41,110,iostat=ios)t,sum1(1),sum1(2),sum1(3),isum1(1),
     +        isum1(2),sum1(4),isum1(3),isum1(4)
         call check_iostat(ios,__LINE__,__FILE__)
         if (debug_loop) 
     & write(42,110,iostat=ios)t,sum2(1),sum2(2),sum2(3),isum2(1),
     +        isum2(2),sum2(4),isum2(3),isum2(4)
         call check_iostat(ios,__LINE__,__FILE__)

         close(41,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         close(42,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

         if (debug_loop) print*
      endif

      if (dosc) then
c average the last nyear steps in every ianav steps (if ianav>nyear)
      itv = mod(istep+(nyear*ndta)-1,(ianav*ndta))
c      if(itv.lt.(nyear*ndta))then
      if(itv.lt.(nyear*ndta) .and. (mod(istep,ndta).lt.1))then
         ext=conv_embm(mod(iav,10))
         if(istep.ge.(nyear*ndta).and.itv.eq.(nyear*ndta)-1)then
            iout = 1
         else
            iout = 0
         endif
         if (debug_loop) call diagosc_embm(istep, iout, ext, fx0flux, fwflux,
     :     wateratm)
c         endif
      endif
      endif

c AY (22/03/04) : netCDF writing code (from Paul and Dan)
      if(mod(istep,(iwstp*ndta)).eq.0) then
         if (debug_loop) print*,'Writing EMBM netCDF file at time',istep
c
c        do netCDF stuff ...
         call ini_netcdf_embm(istep,1)
c
c AY (12/07/05) : removed surplus input arguments to subroutine
c AP (03/08/06) : provide a dummy field for qdry
c also use dummy field for qdry for rq_pa
         qdrydum(:,:)=0.
         call write_netcdf_embm(imax, jmax, k1,
     :        tq, qdrydum, qdrydum,
     :        fx0flux, fwflux, 
     :        work,
     :        maxi, maxj, 1)
c
         call end_netcdf_embm(1)
         if (debug_loop) print*
      endif

c Output arguments
c ----------------------------------------------------------------------

c AY (16/12/03) : These arguments are required in other modules

      do j=1,jmax
         do i=1,imax
c           Surface air temperature [-> surface fluxes]
            tstar_atm(i,j) = real(tq(1,i,j))
c           Orography-adjusted surface air temperature
            if(orogswitch.ge.1)then
               tatm=tq(1,i,j)+(lapse_rate*surf_orog_atm(i,j))
            else
               tatm=tq(1,i,j)
            endif
            torog_atm(i,j) = real(tatm)
c           Surface specific humidity [-> surface fluxes]
            qstar_atm(i,j) = real(tq(2,i,j))
c AY (19/12/03) : Variables dztau and dztav are renamed here to
c                 separate the unscaled (hence 'us') versions read
c	          in from files, from those scaled versions used
c	          in surflux and GOLDSTEIN.
c AY (23/07/04) : Wind stress components modified to reflect the
c                 new array structure (i.e. separate flat 2D arrays
c                 for u and v points)
c           Wind stress x components [-> ocean, surface fluxes]
            stressxu_atm(i,j) = real(us_dztau(1,i,j))
            stressxv_atm(i,j) = real(us_dztav(1,i,j))
c           Wind stress y components [-> ocean, surface fluxes]
            stressyu_atm(i,j) = real(us_dztau(2,i,j))
            stressyv_atm(i,j) = real(us_dztav(2,i,j))
         enddo
      enddo

* ======================================================================
* end of embm.F
* ======================================================================

      return
      end

* ======================================================================
* conv function (called within embm.F)
* ======================================================================

      character*3 function conv_embm(i)
      implicit none
      integer i,i1,i2,itemp,i3
      character*1 a,b,c
      if(i.lt.10)then
        a=char(i+48)
        conv_embm=a//'  '
      else if(i.lt.100)then
        i1=i/10
        i2=i-i1*10
        a=char(i1+48)
        b=char(i2+48)
        conv_embm=a//b//' '
      else
        i1=i/100
        itemp=i-100*i1
        i2=itemp/10
        i3=itemp-10*i2
        a=char(i1+48)
        b=char(i2+48)
        c=char(i3+48)
        conv_embm=a//b//c
      endif
      end



* ======================================================================
* gold_seaice.F
* ======================================================================
*
* AY (20/01/04) : adding c-GOLDSTEIN sea-ice to GENIE framework
*
*	          this code takes pieces from surflux.F, mains.F
* 
* AY (23/09/04) : upgraded to handle sea-ice albedo

      subroutine gold_seaice(istep,
     +     dhght_sic, dfrac_sic,
     +     ustar_ocn, vstar_ocn,
     +     hght_sic, frac_sic, temp_sic, albd_sic,
     +     sic_FW_ocn, sic_FX0_ocn,
     +     test_energy_seaice,test_water_seaice,
     +     koverall)

      use genie_util, ONLY: check_unit,check_iostat

#include "seaice.cmn"
      include 'netcdf_grid.cmn'

c ======================================================================
c Declarations
c ======================================================================

c AY (20/01/04)
c Declare variables passed into this subroutine
      real
     +     dhght_sic(imax,jmax),dfrac_sic(imax,jmax),
     +     ustar_ocn(imax,jmax),vstar_ocn(imax,jmax),
     +     hght_sic(imax,jmax),frac_sic(imax,jmax),
     +     temp_sic(imax,jmax),albd_sic(imax,jmax),
     +     sic_FW_ocn(imax,jmax),sic_FX0_ocn(imax,jmax)

      integer istep, koverall

c AY (20/01/04)
c Local variables
      integer i, j, l, itv, iout, ios

c nre real t ! already declared
      real fw_delta(imax,jmax), fx_delta(imax,jmax)

c AY (03/05/04) : time-series variables
      real sum1(2), sum2(2), sum3(2), sum4(2)
      integer isum1(2), isum2(2), isum3(2)

c AY (09/03/04) : "time" variables for Dan
#ifdef clock
      integer it1, it2, it3, it4
      real    rt1, rt2
#endif
      real    t

c AY (22/03/04) : extra netCDF variable
      real work((maxi+1)*(maxj+1))

      character ext*3, conv_sic*3

c     FOR THE ENERGY CALCULATIONS.....
      real    tot_energy,tot_water
      real    ini_energy,ini_water
      save ini_energy,ini_water
      real vsc
      integer ifirst
      data ifirst/1/
      save ifirst
      real test_energy_seaice,test_water_seaice
      real tv

c ======================================================================
c DJL
c This bit just calculates the inital diagnostic of the total seaice
c   thermal energy and water.  
c The water bit has units of kg
c The energy bit has units of J

       if (ifirst.eq.1) then 
       vsc = dphi*rsc*rsc
       ini_energy=0.0
       ini_water=0.0
         do j=1,jmax
           do i=1,imax
             ini_water=ini_water+varice(1,i,j)*ds(j)
           enddo
         enddo
c      The m2mm is because internally, goldseaice uses m, 
c        but genie uses mm.
       ini_water=m2mm*ini_water*vsc*rhoio
       ifirst=0
       endif
c ======================================================================


c ======================================================================
c Input field modifications
c ======================================================================

c AY (20/01/04)
c Insert any changes to units, etc. of incoming fields here

      do j=1,jmax
         do i=1,imax
c AY (22/01/04) : set up ice temperature
            tice(i,j) = temp_sic(i,j)
c AY (23/09/04) : set up ice albedo
            albice(i,j) = albd_sic(i,j)
            dtha(1,i,j) = dhght_sic(i,j)
            dtha(2,i,j) = dfrac_sic(i,j)
            u(1,i,j) = ustar_ocn(i,j)
            u(2,i,j) = vstar_ocn(i,j)
            fw_delta(i,j) = 0.
            fx_delta(i,j) = 0.
         enddo
      enddo

c ======================================================================
c Sea-ice model timestep
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
      if (debug_loop) 
     & write(*,120) 'G-SEAICE time : iteration',istep,', year',it4,
     +     ', day',rt2

#endif

c AY (20/01/04) : previously in surflux.F ...
c ----------------------------------------------------------------------

c update ice 
      if (impsic) then
         call tstipsic
c     print*,'implicit sea ice '
      else
         call tstepsic
c     print*,'explicit sea ice '
      endif

c modify heat and salinity fluxes into ocean according to sea-ice update;
c prevent <0%, >100% fractional area A and set A=H=0 if H<Hmin
c in which case add an amount -H of ice, hence need to add appropriate
c heat and freshwater fluxes.

c AY (20/01/04) : This code altered so that instead of modifying net
c                 ocean fluxes (which aren't really used in genie.f
c                 anyway), a new sea-ice flux is calculated and fed
c                 to goldstein.F where it is combined with the other
c                 fluxes to determine the net ocean fluxes.  This is
c                 consistent with the previous calculations below.

      do j=1,jmax
         do i=1,imax
            if(kmax.ge.k1(i,j))then
               fw_delta(i,j) = - rhoio*dtha(1,i,j)
               varice(2,i,j) = max(0.0,min(1.0,varice(2,i,j)))
               if(varice(1,i,j).lt.hmin)then
                  fx_delta(i,j) = - varice(1,i,j)*rhoice*hlf*rdtdim
                  fw_delta(i,j) = fw_delta(i,j)
     +                 + varice(1,i,j)*rhoio*rdtdim
c AY (20/01/04) : global heat excised for now
c                 ghs = ghs - varice(1,i,j)*rhoice*hlf*rdtdim
                  do l=1,2
                     varice(l,i,j) = 0.
                  enddo
               endif

               do l=1,2
                  varice1(l,i,j) = varice(l,i,j)
               enddo

c AY (23/07/04) : convert sea-ice FW flux from m/s to mm/s (for Dan)
               fw_delta(i,j) = fw_delta(i,j) * m2mm

            endif
         enddo
      enddo

c AY (13/07/04) : Note that sea-ice albedo (calculated in the new
c	          surf_ocn_sic.F routine) is not affected by the
c	          above, despite the sea-ice melting.

c Sea-ice diagnostics and output
c ----------------------------------------------------------------------

c AY (04/10/04) : netCDF output added
      if (lnetout) then
           call outm_netcdf_sic(istep)
      endif

      if(mod(istep,iwstp).eq.0)then
c        Write restart file
         if (lascout) then
            ext=conv_sic(mod(iw,10))
            if (debug_loop)
     & print*,'Writing sea-ice restart file at time',istep,
     +           '(koverall',koverall,')'
            call check_unit(22,__LINE__,__FILE__)
            open(22,file=outdir_name(1:lenout)//lout//'.'//ext,
     &           iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)
            rewind 22
            call outm_seaice(22)
            close(22,iostat=ios)
            call check_iostat(ios,__LINE__,__FILE__)
         endif
         
         iw = iw + 1
         if (debug_loop)print*
      endif

c AY (29/04/04) : format statements for time-series files of average 
c                 sea-ice properties
 110  format(6e15.6,2i5,1e15.6,2i5,1e15.6,2i5)

      if(mod(istep,itstp).eq.0)then
c AY (29/04/04) : new time-series output for GOLDSTEIN sea-ice
c AY (05/05/04) : temporary "time" variable (until we sort it out at the
c                 genie.F level)
         t = real(istep)/real(nyear)
         if (debug_loop)print*,'Writing to sea-ice time-series file'
         call check_unit(43,__LINE__,__FILE__)
         open(43,file=outdir_name(1:lenout)//lout//'.'//'sic',
     1        status='old',position='append',iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         call diag4(sum1,sum2,sum3,sum4,isum1,isum2,isum3)
         if (debug_loop)
     & write(43,110,iostat=ios)t,sum1(1),sum1(2),sum2(1),sum2(2),
     +        sum3(1),isum1(1),isum1(2),sum3(2),isum2(1),isum2(2),
     +        sum4(1),isum3(1),isum3(2)
         call check_iostat(ios,__LINE__,__FILE__)
         close(43,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         if (debug_loop)print*
      endif

      if (dosc) then
c average the last nyear steps in every ianav steps (if ianav>nyear)
         
      itv = mod(istep+nyear-1,ianav)
      if(itv.lt.nyear)then
         ext=conv_sic(mod(iav,10))
         if(istep.ge.nyear.and.itv.eq.nyear-1)then
            iout = 1
         else
            iout = 0
         endif
         if (debug_loop) call diagosc_sic(istep, iout, ext, fx_delta, fw_delta)
      endif
      endif

c AY (21/01/04) : write out some interesting sea-ice information
      if(mod(istep,npstp).eq.0)then
         if (debug_loop) call diagsic(istep)
         if (debug_loop) print*
      endif

c AY (22/03/04) : netCDF writing code (from Paul and Dan)
      if(mod(istep,iwstp).eq.0) then
         if (debug_loop) print*,'Writing sea-ice netCDF file at time',istep
c
c        do netCDF stuff ...
         call ini_netcdf_sic(istep,1)
c
c AY (22/10/04) : note that all of the arguments for the write_netCDF
c                 function are in the same pre-cision as the sea-ice
c                 module
         call write_netcdf_sic(imax, jmax, k1,
     :        varice,tice,albice,
     :        dtha, fx_delta, fw_delta,
     :        work,
     :        maxi, maxj, 1)
c
         call end_netcdf_sic(1)
         if (debug_loop) print*
      endif

c Output arguments
c ----------------------------------------------------------------------

c AY (20/01/04) : These arguments are required in other modules

      do j=1,jmax
         do i=1,imax
c           Sea-ice thickness [-> surface fluxes]
            hght_sic(i,j) = real(varice(1,i,j))
c           Sea-ice area      [-> surface fluxes]
            frac_sic(i,j) = real(varice(2,i,j))
c           Freshwater flux   [-> GOLDSTEIN]
            sic_FW_ocn(i,j) = real(fw_delta(i,j))
c           Heat flux         [-> GOLDSTEIN]
            sic_FX0_ocn(i,j) = real(fx_delta(i,j))
         enddo
      enddo

c ======================================================================
c DJL
c This bit just calculates the diagnostic of the total seaice 
c   thermal energy and water.
c The water bit has units of kg, relative to an initial value.
c The energy bit has units of J, relative to an initial value.
c THE ENERGY BIT (HERE AND IN SURF_OCN_SIC) IS WELL-DODGY AND
c IS A COMPLETE FIX.  SEE DJL FOR MORE INFO.
       if (mod(istep,conserv_per).eq.0) then
       tv = 86400.0*yearlen/(nyear*tsc)
       vsc = dphi*rsc*rsc
       tot_energy=0.0
       tot_water=0.0
         do j=1,jmax
           do i=1,imax
             tot_energy=tot_energy-sic_FX0_ocn(i,j)*ds(j)
             tot_water=tot_water+varice(1,i,j)*ds(j)
           enddo
         enddo
c      The m2mm is because internally, goldseaice uses m, 
c        but genie uses mm.
       tot_energy=vsc*tsc*tv*tot_energy
       tot_water=m2mm*vsc*rhoio*tot_water
       test_energy_seaice=real(test_energy_seaice+tot_energy-ini_energy)
       test_water_seaice=real(tot_water-ini_water)
       if (debug_loop) 
     & print*,'GoldSeaice energy diagnostic: ',test_energy_seaice
       if (debug_loop) 
     & print*,'GoldSeaice water diagnostic: ',test_water_seaice
       endif
c ======================================================================


* ======================================================================
* end of gold_seaice.F
* ======================================================================

      return
      end

* ======================================================================
* conv function (called within gold_seaice.F)
* ======================================================================

      character*3 function conv_sic(i)
      implicit none
      integer i,i1,i2,itemp,i3
      character*1 a,b,c
      if(i.lt.10)then
        a=char(i+48)
        conv_sic=a//'  '
      else if(i.lt.100)then
        i1=i/10
        i2=i-i1*10
        a=char(i1+48)
        b=char(i2+48)
        conv_sic=a//b//' '
      else
        i1=i/100
        itemp=i-100*i1
        i2=itemp/10
        i3=itemp-10*i2
        a=char(i1+48)
        b=char(i2+48)
        c=char(i3+48)
        conv_sic=a//b//c
      endif
      end

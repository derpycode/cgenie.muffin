c
c diagosc.f extra diagnostic routine for c-goldstein v2 with seasonal cycle
c calculate average over nyear timesteps. Extra diagnostics to be included
c WHERE INDICATED 
c file created 18/6/3 Neil Edwards
c
c AY (20/01/04) : altered for genie-seaice
c		  references to GOLDSTEIN and EMBM removed
c
c AY (23/09/04) : upgraded to output sea-ice albedo
c
c AY (05/10/04) : upgraded to output annual mean netCDF data

      subroutine diagosc_sic(istep,iout,ext,fx_delta,fw_delta)

      use genie_util, ONLY: check_unit,check_iostat

#include "seaice.cmn"

      integer istep, iout, ios

      character ext*3

      real fx_delta(maxi,maxj), fw_delta(maxi,maxj)

c Local variables
      real rnyear
c      real sum1,sum2,vsc,sum,amin,amax,sum3

      integer i,j,l
c      integer iamax,iamin,jamin,jamax

c AY (06/10/04) : extra netCDF variable
      real work((maxi+1)*(maxj+1))

      rnyear = 1.0/nyear

      do j=1,jmax
         do i=1,imax
            do l=1,2
               haavg(l,i,j) = haavg(l,i,j) + varice(l,i,j)*rnyear
            enddo
            ticeavg(i,j) = ticeavg(i,j) + tice(i,j)*rnyear
            albiceavg(i,j) = albiceavg(i,j) + albice(i,j)*rnyear
c AY (06/10/04) : new fields for averaging
            dthaavg(1,i,j) = dthaavg(1,i,j) + dtha(1,i,j)*rnyear
            dthaavg(2,i,j) = dthaavg(2,i,j) + dtha(2,i,j)*rnyear
            fxdelavg(i,j) = fxdelavg(i,j) + fx_delta(i,j)*rnyear
            fwdelavg(i,j) = fwdelavg(i,j) + fw_delta(i,j)*rnyear
         enddo
      enddo
c
c write time series for oscillation cycle if required
c
c     call aminmax(imax,jmax,varice,amin,amax,iamin,iamax
c    1                  ,jamin,jamax,2,1)
c     print*,'min h ice ',amin,' at ',iamin,jamin
c     print*,'max h ice ',amax,' at ',iamax,jamax
c
c AY (16/12/03) : variable amax hacked out
c     write(50,'(3e15.7)')t,sum,amax
c     write(50,'(3e15.7)')t,sum

      if(iout.eq.1)then
         print*,'Sea-ice : writing averaged data at istep ',istep
c
c write averaged data (a near-copy of outm.f) not a restart
c as such, therefore can write less accurate, more economical output
c
c AY (02/04/04)
c        open(1,file='../results/'//lout//'.avg')
         call check_unit(2,__LINE__,__FILE__)
         open(2,file=outdir_name(1:lenout)//lout//'.osc.'//ext,
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c         do j=1,jmax
c            do i=1,imax
c               do l=1,2
c                  write(2,10,iostat=ios)haavg(l,i,j)
c                  call check_iostat(ios,__LINE__,__FILE__)
c               enddo
c            enddo
c         enddo
         write(2,10,iostat=ios)haavg
         call check_iostat(ios,__LINE__,__FILE__)
c Sea-ice temperature
c         do j=1,jmax
c            do i=1,imax
c               write(2,10,iostat=ios)ticeavg(i,j)
c               call check_iostat(ios,__LINE__,__FILE__)
c            enddo
c         enddo
         write(2,10,iostat=ios)ticeavg
         call check_iostat(ios,__LINE__,__FILE__)
c Sea-ice albedo
c         do j=1,jmax
c            do i=1,imax
c               write(2,10,iostat=ios)albiceavg(i,j)
c               call check_iostat(ios)__LINE__,__FILE__)
c            enddo
c         enddo
         write(2,10,iostat=ios)albiceavg
         call check_iostat(ios,__LINE__,__FILE__)
c Change in sea-ice height
c         do j=1,jmax
c            do i=1,imax
c               write(2,10,iostat=ios)dthaavg(1,i,j)
c               call check_iostat(ios,__LINE__,__FILE__)
c            enddo
c         enddo
         write(2,10,iostat=ios)dthaavg(1,1:imax,1:jmax)
         call check_iostat(ios,__LINE__,__FILE__)
c Change in sea-ice fractional cover
c         do j=1,jmax
c            do i=1,imax
c               write(2,10,iostat=ios)dthaavg(2,i,j)
c               call check_iostat(ios,__LINE__,__FILE__)
c            enddo
c         enddo
         write(2,10,iostat=ios)dthaavg(2,1:imax,1:jmax)
         call check_iostat(ios,__LINE__,__FILE__)
c Sea-ice heat flux to ocean
c         do j=1,jmax
c            do i=1,imax
c               write(2,10,iostat=ios)fxdelavg(i,j)
c               call check_iostat(ios,__LINE__,__FILE__)
c            enddo
c         enddo
         write(2,10,iostat=ios)fxdelavg
         call check_iostat(ios,__LINE__,__FILE__)
c Sea-ice freshwater flux to ocean
c         do j=1,jmax
c            do i=1,imax
c               write(2,10,iostat=ios)fwdelavg(i,j)
c               call check_iostat(ios,__LINE__,__FILE__)
c            enddo
c         enddo
         write(2,10,iostat=ios)fwdelavg
         call check_iostat(ios,__LINE__,__FILE__)
c        write(1,*)t
         close(2,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c     
c AY (22/03/04) : netCDF writing code (from Paul and Dan)
         print*,'Writing sea-ice mean annual netCDF file at time',
     :        istep
c
c        do netCDF stuff ...
         call ini_netcdf_sic(istep,2)
c
         call write_netcdf_sic(imax, jmax, k1,
     :        haavg,ticeavg,albiceavg,
     :        dthaavg, fxdelavg, fwdelavg,
     :        work,
     :        maxi, maxj, 2)
c
         call end_netcdf_sic(2)
         print*
c
c AY (29/04/04) : increment average counter
         iav = iav + 1
c 
c perform diagnostics on averaged data, either by rewriting other diag 
c routines to accept data as argument, or by simply copying code,
c otherwise diagnose by integrating one (short) step from .avg file.
c
c diagnostic code to be inserted here
c
         print*,'Sea-ice : resetting averaged data arrays at step',
     :        istep
         do j=1,jmax
            do i=1,imax
               do l=1,2
                  haavg(l,i,j) = 0. 
               enddo
               ticeavg(i,j) = 0.
               albiceavg(i,j) = 0.
               dthaavg(1,i,j) = 0.
               dthaavg(2,i,j) = 0.
               fxdelavg(i,j) = 0.
               fwdelavg(i,j) = 0.
            enddo
         enddo
         print*
      endif

  10  format(e14.7)
      end

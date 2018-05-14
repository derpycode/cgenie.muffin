c
c diagosc.f extra diagnostic routine for c-goldstein v2 with seasonal cycle
c calculate average over nyear timesteps. Extra diagnostics to be included
c WHERE INDICATED 
c file created 18/6/3 Neil Edwards
c
c AY (01/12/03) : altered for genie-goldstein
c		  references to GOLDSTEIN removed
c		  sea-ice also excised
c
c AY (05/02/04) : altered variable rnyear to account for shorter EMBM
c                 timesteps
c
c AY (29/04/04) : upgraded to output average fluxes
c
c AY (06/10/04) : upgraded to output annual mean netCDF data
c
c AP (03/08/06) : upgraded to calculate annual average error value

      subroutine diagosc_embm(istep,iout,ext,fx0flux,fwflux,
     :     wateratm)

      use genie_util, ONLY: check_unit, check_iostat

#include "embm.cmn"

      integer istep, iout, ios

      real    fx0flux(4,maxi,maxj), fwflux(2,maxi,maxj)

      character ext*3

c Local variables
      real rnyear
      real err3, err4, err_embm
      real watereb, wateratm, vsc
c      real sum1,sum2,vsc,amin,amax,sum3,sum

c AP (03/08/06) : Local variable used to calculate qdryavg
c      real tv2

      integer i,j,l
c      integer iamin,iamax,jamin,jamax

c AY (06/10/04) : extra netCDF variable
      real work((maxi+1)*(maxj+1))

c axes
      real lon(maxi),lat(maxj)

      rnyear=1.0/nyear
      
      do j=1,jmax
         do i=1,imax
            do l=1,2
               tqavg(l,i,j) = tqavg(l,i,j) + tq(l,i,j)*rnyear
            enddo
c variable 'qdryavg' has been superseded by 'q_pa_avg' which is the
c annual mean of 'q_pa' computed in 'genie-embm/src/fortran/embmg.F'
cc AP (03/08/06) : adding avg precipitated atm. humidity
c            tv2 = const1*exp(const4*tq(1,i,j)
c     +                     /(tq(1,i,j)+const5))
c            qdryavg(i,j) = qdryavg(i,j) +
c     +                     min(tq(2,i,j),rmax*tv2)*rnyear
c update annual-average fields for precipitation-adjusted humidity
c (i.e., humidity after precipitation)
            q_pa_avg(i,j) = q_pa_avg(i,j) + q_pa(i,j)*rnyear
            rq_pa_avg(i,j) = rq_pa_avg(i,j) + rq_pa(i,j)*rnyear
c AY (29/04/04) : adding heat flux, FW flux and wind stress averages
            do l=1,4
               fx0avg(l,i,j) = fx0avg(l,i,j) + fx0flux(l,i,j)*rnyear
            enddo
            do l=1,2
               fwavg(l,i,j) = fwavg(l,i,j) + fwflux(l,i,j)*rnyear
            enddo
         enddo
      enddo
c
c AY (29/04/04) : commented out
c
c write time series for oscillation cycle if required
c choose N. hemi surface air temp. as example
c
c     sum = 0.
c     do i=1,imax
c        do j=jmax/2+1,jmax
c           sum = sum + tq(1,i,j)
c        enddo
c     enddo
c     sum = sum/(imax*jmax*0.5)
c     
c AY (16/12/03) : variable amax hacked out
c     write(50,'(3e15.7)')time,sum,amax
c     write(50,'(3e15.7)')time,sum

      if(iout.eq.1)then
         print*,'EMBM : writing averaged data at istep ',
     +        istep/real(ndta)
c
c write averaged data (a near-copy of outm.f) not a restart
c as such, therefore can write less accurate, more economical output
c
c AY (29/04/04)
c        open(1,file='../results/'//lout//'.avg')
         call check_unit(2,__LINE__,__FILE__)
         open(2,file=outdir_name(1:lenout)//lout//'.osc.'//ext,
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

c EMBM
         do j=1,jmax
            do i=1,imax
               do l=1,2
c                  write(2,10,iostat=ios)tqavg(l,i,j)
c                  call check_iostat(ios,__LINE__,__FILE__)
               enddo
            enddo
         enddo
c Heat fluxes       
         do l=1,4
            do j=1,jmax
               do i=1,imax
c                  write(2,10,iostat=ios)fx0avg(l,i,j)
c                  call check_iostat(ios,__LINE__,__FILE__)
               enddo
            enddo
         enddo
c Freshwater fluxes
         do l=1,2
            do j=1,jmax
               do i=1,imax
c                  write(2,10,iostat=ios)fwavg(l,i,j)
c                  call check_iostat(ios,__LINE__,__FILE__)
               enddo
            enddo
         enddo
        write(2,10,iostat=ios)tqavg
        call check_iostat(ios,__LINE__,__FILE__)
        write(2,10,iostat=ios)(((fx0avg(l,i,j),i=1,imax),
     &       j=1,jmax),l=1,4)
        call check_iostat(ios,__LINE__,__FILE__)
        write(2,10,iostat=ios)(((fwavg(l,i,j),i=1,imax),j=1,jmax),l=1,2)
        call check_iostat(ios,__LINE__,__FILE__)
        close(2,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c     
c AY (22/03/04) : netCDF writing code (from Paul and Dan)
         print*,'Writing EMBM mean annual netCDF file at time',istep
c
c        do j=1,jmax
c           do i=1,imax
c              fxlatavg(i,j) = fx0avg(1,i,j)
c              fxsenavg(i,j) = fx0avg(2,i,j)
c              fxswavg(i,j)  = fx0avg(3,i,j)
c              fxlwavg(i,j)  = fx0avg(4,i,j)
c              fwpptavg(i,j) = fwavg(1,i,j)
c              fwevpavg(i,j) = fwavg(2,i,j)
c           enddo
c        enddo
c
c        do netCDF stuff ...
         call ini_netcdf_embm(istep,2)
c
         call write_netcdf_embm(imax, jmax, k1,
     :        tqavg,
c qdryavg,
     :        q_pa_avg,rq_pa_avg,
     :        fx0avg,fwavg,
     :        work,
     :        maxi, maxj, 2)
c
         call end_netcdf_embm(2)
         print*
c
c AY (29/04/04) : increment average counter
         iav = iav + 1
c 
c perform diagnostics on averaged data, either by rewriting other diag 
c routines to accept data as argument, or by simply copying code,
c otherwise diagnose by integrating one (short) step from .avg file.
c
c AP (03/08/06) : Call external error function
      do i=1,imax
         lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
      enddo
      do j=1,jmax
         lat(j)=180.0*asin(s(j))/pi
      enddo
      err3 = err_embm(tqavg(1,1:imax,1:jmax), 1, imax, jmax, indir_name,
     $     lenin, tdatafile, lentdata, tdata_scaling, tdata_offset,
     $     tqinterp, tdata_varname, tdata_missing, lon, lat)
      if (qdata_rhum) then
         err4 = err_embm(
c     qdryavg(1:imax,1:jmax),
     &        rq_pa_avg, 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      else
         err4 = err_embm(
c     qdryavg(1:imax,1:jmax),
     &        q_pa_avg, 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      endif
      print*,'err_embm annual average composite = ',
     &                          sqrt( ((err3**2*imax*jmax) +
     &                                 (err4**2*imax*jmax))
     &                               / ( 2*imax*jmax ) )
c end of diagnostic error calculation

c KICO (22/11/06): calculate the atmosphere water content,
c wateratm, which is passed to ????.f in genie-main
        watereb=0.
        do j=1,jmax
           do i=1,imax
             watereb = watereb+tqavg(2,i,j)*ds(j)
           enddo
        enddo
        vsc = dphi*rsc*rsc*1.0e-12
        wateratm= (watereb*rhoao*hatmbl(2))*vsc

         print*,'EMBM : resetting averaged data arrays at step',
     +        istep/real(ndta)
         do j=1,jmax
            do i=1,imax
               do l=1,2
                  tqavg(l,i,j) = 0.
               enddo
c AP (03/08/06) : reset the precipitated atm. humidity
c               qdryavg(i,j) = 0.
c reset annual-average fields for precipitation-adjusted humidity (i.e.,
c humidity after precipitation)
               q_pa_avg(i,j) = 0.
               rq_pa_avg(i,j) = 0.
c AY (02/04/04) : reset average flux fields 
               do l=1,2
                  fx0avg(l,i,j)  = 0.
                  fwavg(l,i,j)   = 0.
               enddo
               do l=3,4
                  fx0avg(l,i,j)  = 0.
               enddo
            enddo
         enddo
         print*
      endif

  10  format(e14.7)

      end

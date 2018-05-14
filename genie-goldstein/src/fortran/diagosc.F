c
c diagosc.f extra diagnostic routine for c-goldstein v2 with seasonal cycle
c calculate average over nyear timesteps. Extra diagnostics to be included
c WHERE INDICATED 
c file created 18/6/3 Neil Edwards
c
c AY (01/12/03) : altered for genie-goldstein
c		  references to EMBM and sea-ice removed
c
c AY (07/04/04) : altered to average heat fluxes, FW fluxes, wind
c	          stress
c
c AY (07/10/04) : upgraded to output annual mean netCDF data
c
c AP (03/08/06) : upgraded to calculate annual average error value
c
c AP (09/08/06) : error function modified to deal with null values
c                 in the observational data

      subroutine diagosc(istep,iout,ext,fx0flux,fwflux,wstress)

      use genie_util, ONLY: check_unit, check_iostat

#include "ocean.cmn"
      include 'netcdf_grid.cmn'

      integer istep, iout, ios

      real    fx0flux(5,maxi,maxj), fwflux(4,maxi,maxj)
      real    wstress(4,maxi,maxj)

      character ext*3

c Local variables
      real rnyear
      real err1, err2, err_gold
      integer ntot1, ntot2
c      real sum1,sum2,sum3,vsc
c      real amax, amin, sum

      integer i,j,k,l
c      integer iamin,iamax,jamin,jamax

c AY (07/10/04) : extra variables for netCDF output
      real work((maxi+1)*(maxj+1)*(maxk+1))

c Stream function variables
      real opsi(0:maxj,0:maxk)
      real opsia(0:maxj,0:maxk), omina, omaxa
      real opsip(0:maxj,0:maxk), ominp, omaxp
      integer iposa(2)

c      real opsiavg(0:maxj,0:maxk)
c      real opsipavg(0:maxj,0:maxk)
c      real opsiaavg(0:maxj,0:maxk)
      real, save :: opsiavg(0:maxj,0:maxk) = 0.0
      real, save :: opsipavg(0:maxj,0:maxk) = 0.0
      real, save :: opsiaavg(0:maxj,0:maxk) = 0.0

c jdannan 22/07/2008 cutting out the pointwise ascii writes
      real outputfile(lmax+3,kmax,imax,jmax)
      real outfx0(5,imax,jmax)
      real outfw(4,imax,jmax)
      
c axes
      real lon(maxi),lat(maxj),depth(maxk)

c      do j=0,jmax
c         do k=0,kmax
c            opsiavg(j,k)  = 0.
c            opsipavg(j,k) = 0.
c            opsiaavg(j,k) = 0.
c         enddo
c      enddo

      rnyear = 1.0/nyear

      do j=1,jmax
         do i=1,imax
            do k=1,kmax
               do l=1,lmax
                  tsavg(l,i,j,k) = tsavg(l,i,j,k) + ts(l,i,j,k)*rnyear
               enddo
               do l=1,3
                  uavg(l,i,j,k) = uavg(l,i,j,k) + u(l,i,j,k)*rnyear
               enddo
               rhoavg(i,j,k) = rhoavg(i,j,k) + rho(i,j,k)*rnyear
            enddo
c AY (02/04/04) : adding heat flux, FW flux and wind stress averages
            do l=1,5
               fx0avg(l,i,j) = fx0avg(l,i,j) + fx0flux(l,i,j)*rnyear
            enddo
            do l=1,4
               fwavg(l,i,j) = fwavg(l,i,j) + fwflux(l,i,j)*rnyear
            enddo
            do l=1,4
               windavg(l,i,j) = windavg(l,i,j) + 
     :              wstress(l,i,j)*rnyear
            enddo
         enddo
      enddo

crma extra code for averaging streamfunction data (rma, 4/07/06)

      call diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip,iposa)

      do j=0,jmax
         do k=0,kmax
             opsiavg(j,k) = opsiavg(j,k) + opsi(j,k)*rnyear
             opsipavg(j,k) = opsipavg(j,k) + opsip(j,k)*rnyear
             opsiaavg(j,k) = opsiaavg(j,k) + opsia(j,k)*rnyear
         enddo
      enddo
c
      if(iout.eq.1)then
         print*,'GOLD : writing averaged data at istep ',istep
c
c write averaged data (a near-copy of outm.f) not a restart
c as such, therefore can write less accurate, more economical output
c
c AY (02/04/04)
c        open(1,file='../results/'//lout//'.avg')
        outputfile=0.
         call check_unit(2,__LINE__,__FILE__)
         open(2,file=outdir_name(1:lenout)//lout//'.'//'osc.'//ext,
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

         do j=1,jmax
            do i=1,imax
               do k=1,kmax
                  do l=1,lmax  
                     if(k.ge.k1(i,j))then
c                        write(2,10,iostat=ios)tsavg(l,i,j,k)
c                        call check_iostat(ios,__LINE__,__FILE__)
                        outputfile(l,k,i,j)=tsavg(l,i,j,k)
                     else
c                        write(2,10,iostat=ios)0.0
c                        call check_iostat(ios,__LINE__,__FILE__)      
                     endif
                  enddo
                  do l=1,2
c                     write(2,10,iostat=ios)uavg(l,i,j,k)
c                     call check_iostat(ios,__LINE__,__FILE__)
                     outputfile(lmax+l,k,i,j)=uavg(l,i,j,k)
                  enddo
c                  write(2,10,iostat=ios)rhoavg(i,j,k)
c                  call check_iostat(ios,__LINE__,__FILE__)
                     outputfile(lmax+3,k,i,j)=rhoavg(i,j,k)
               enddo
            enddo
         enddo
          write(2,10,iostat=ios)outputfile
          call check_iostat(ios,__LINE__,__FILE__)
c Heat fluxes       
         outfx0=fx0avg(1:5,1:imax,1:jmax)
         do l=1,5
            do j=1,jmax
               do i=1,imax
                  if(k1(i,j).lt.90)then
c                     write(2,10,iostat=ios)fx0avg(l,i,j)
c                     call check_iostat(ios,__LINE__,__FILE__)
                  else
c                     write(2,10,iostat=ios)0.0
c                     call check_iostat(ios,__LINE__,__FILE__)
                     outfx0(l,i,j)=0.0
                  endif
               enddo
            enddo
         enddo
        write(2,10,iostat=ios)(((outfx0(l,i,j),i=1,imax),
     &       j=1,jmax),l=1,5)
        call check_iostat(ios,__LINE__,__FILE__)
c Freshwater fluxes
         outfw=fwavg(1:4,1:imax,1:jmax)
c         do l=1,4
c            do j=1,jmax
c               do i=1,imax
c                  if(k1(i,j).lt.90)then
c                     write(2,10,iostat=ios)fwavg(l,i,j)
c                     call check_iostat(ios,__LINE__,__FILE__)
c                  else
c                     write(2,10,iostat=ios)0.0
c                     call check_iostat(ios,__LINE__,__FILE__)
c                  endif
c               enddo
c            enddo
c         enddo
        write(2,10,iostat=ios)(((outfw(l,i,j),i=1,imax),j=1,jmax),l=1,4)
        call check_iostat(ios,__LINE__,__FILE__)
c Wind stresses (u-point x, u-point y, v-point x, v-point y)
c         do l=1,4
c            do j=1,jmax
c               do i=1,imax
c                  write(2,10,iostat=ios)windavg(l,i,j)
c                  call check_iostat(ios,__LINE__,__FILE__)
c               enddo
c            enddo
c         enddo
        write(2,10,iostat=ios)(((windavg(l,i,j),i=1,imax),j=1,jmax),l=1,4)
        call check_iostat(ios,__LINE__,__FILE__)
        close(2,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)

crma write averaged streamfunction data (rma, 4/07/06)

c        OPAV
         call check_unit(10,__LINE__,__FILE__)
         open(10,file=outdir_name(1:lenout)//lout//'.opav',
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         write(10,10,iostat=ios)((opsiavg(j,k),j=0,jmax),k=0,kmax)
         call check_iostat(ios,__LINE__,__FILE__)
         close(10,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c        OPAVP
         open(10,file=outdir_name(1:lenout)//lout//'.opavp',
     &       iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         write(10,10,iostat=ios)((opsipavg(j,k),j=0,jmax),k=0,kmax)
         call check_iostat(ios,__LINE__,__FILE__)
         close(10,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c        OPAVA
         open(10,file=outdir_name(1:lenout)//lout//'.opava',
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         write(10,10,iostat=ios)((opsiaavg(j,k),j=0,jmax),k=0,kmax)
         call check_iostat(ios,__LINE__,__FILE__)
         close(10,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
c     
c AY (17/03/04) : netCDF writing code (from Paul and Dan)
         print*,'Writing GOLDSTEIN mean annual netCDF file at time',
     :        istep

c        do netCDF stuff ...
         call ini_netcdf_ocn(istep,2)
c
         call write_netcdf_ocn(imax, jmax, kmax, k1, ncdepth1,
     :        opsiavg, opsiaavg, opsipavg,
     :        tsavg, uavg, rhoavg, 
     :        fx0avg, fwavg,
     :        work,
     :        dsc, usc, rsc, saln0,
     :        maxi, maxj, maxk, maxl, 2)
c
         call end_netcdf_ocn(2)
         print*

c AY (02/04/04) : increment average counter
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
      do k=1,kmax
         depth(k)=abs(zro(kmax+1-k)*dsc)
      enddo
      ntot1 = 0
      err1 = err_gold(tsavg(1,1:imax,1:jmax,1:kmax), 1, k1, ntot1, imax,
     $     jmax, kmax, indir_name, lenin, tdatafile, lentdata,
     $     tdata_scaling, tdata_offset, tsinterp, tdata_varname,
     $     tdata_missing, lon, lat, depth)
      ntot2 = 0
      err2 = err_gold(tsavg(2,1:imax,1:jmax,1:kmax), 2, k1, ntot2, imax,
     $     jmax, kmax, indir_name, lenin, sdatafile, lensdata,
     $     sdata_scaling, sdata_offset+saln0, tsinterp, sdata_varname,
     $     sdata_missing, lon, lat, depth)
      print*,'err_gold annual average composite = ',
     &                               sqrt( ((err1**2*ntot1) +
     &                                      (err2**2*ntot2))
     &                                    / ( ntot1 + ntot2) )
c end of diagnostic error calculation
c
c compute total water content of planet (should be numerically const.)

c AY (01/12/03)
c      sum1=0
c      sum2=0
c      sum3=0
c
c      do j=1,jmax
c         do i=1,imax
c            sum1 = sum1 + tqavg(2,i,j)
c            sum2 = sum2 + haavg(1,i,j)
c            do k=1,kmax
c               sum3 = sum3 + tsavg(2,i,j,k)*dz(k)
c            enddo
c         enddo
c      enddo
c      vsc = ds*dphi*rsc*rsc*1e-12
c      print*,'total water (*10^12 m^3)',
c     1    (sum1*rhoao*hatmbl(2) + sum2*rhoio - sum3*dsc/saln0)
c     2     *vsc

         print*,'resetting averaged data arrays at step',istep
         do j=1,jmax
            do i=1,imax
               do k=1,kmax
                  do l=1,lmax
                     tsavg(l,i,j,k) = 0.
                  enddo
                  do l=1,3
                     uavg(l,i,j,k) = 0.
                  enddo
                  rhoavg(i,j,k) = 0.
               enddo
c AY (02/04/04) : reset average flux and wind stress fields 
               do l=1,4
                  fx0avg(l,i,j)  = 0.
                  fwavg(l,i,j)   = 0.
                  windavg(l,i,j) = 0.
               enddo
               fx0avg(5,i,j)     = 0.
            enddo
         enddo
c RMA (04/07/06) : reset average opsi streamfunctions
         do j=0,jmax
            do k=0,kmax
               opsiavg(j,k)  = 0.
               opsipavg(j,k) = 0.
               opsiaavg(j,k) = 0.
            enddo
         enddo
      endif

  10  format(e14.7)

      end

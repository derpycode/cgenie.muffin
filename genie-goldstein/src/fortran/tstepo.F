*
* subroutine tstep.f for program goldstein 
* error in order of co call and rho b.c. corrected 9/2/1
* flux version fully explicit one step second order variable depth
*
c version with isoneutral diffusion 21/5/1
c notes; fe/fn/fa in previous versions could have been scalars
c error in isoneutral coeffs corrected 29/8/2
c upstream weighting included 29/8/2
c variable ds 6/12/4 nre
c 
c AY (12/07/05) : removed surplus input argument to function
c
      subroutine tstepo

      use omp_lib

#include "ocean.cmn"
      
      real ups0
      integer i, j, k, l
      parameter(ups0 = 0.)

c KICO Mixed layer scheme needs:
      real mldtsold(maxl,maxi,maxj,maxk), mldrhoold(maxi,maxj,maxk)
      real mldrhonew(maxi,maxj,maxk)
      real mldtstmp(2), mldrhotmp
      integer mldpk(2,maxi,maxj)

ccc      integer::nthreads,thread_id
ccc      real t1,t2,t3,t4
      real ts_t1(maxl,maxi,maxj,maxk),ts1_t1(maxl,maxi,maxj,maxk)
      real rho_t1(maxi,maxj,maxk)
      real ts_t2(maxl,maxi,maxj,maxk),ts1_t2(maxl,maxi,maxj,maxk)
      real rho_t2(maxi,maxj,maxk)

      if(imld.eq.1)then
c KICO 18/09/07, before calculating fluxes, calculate energy consumed
c or released in mixing surface forcing over top layer. Needed for
c mld calculation, especially if mixed layer is <1 cell thick.
c NOTE: for now, all "energies" in this scheme are calculated in units
c of density x height^2, or (Energy/area)/2*acceleration (due to gravity).
        do j=1,jmax
          do i=1,imax
            mldtstmp(1) = ts(1,i,j,kmax)-ts(1,i,j,kmax+1)
            mldtstmp(2) = ts(2,i,j,kmax)-ts(2,i,j,kmax+1)
            call eos(ec,mldtstmp(1),mldtstmp(2),zro(kmax),ieos,
     1         mldrhotmp)
            mldpelayer1(i,j) = (mldrhotmp-rho(i,j,kmax))
     1                         *z2dzg(kmax,kmax)
          enddo
        enddo
      endif

c **********************************************************************
c **********************************************************************
c **********************************************************************
      do k=1,kmax
         do j=1,jmax
            do i=1,imax
               do l=1,lmax
                  ts_t1(l,i,j,k)  = ts(l,i,j,k)
                  ts1_t1(l,i,j,k) = ts1(l,i,j,k)
                  ts_t2(l,i,j,k)  = ts(l,i,j,k)
                  ts1_t2(l,i,j,k) = ts1(l,i,j,k)
               enddo
               rho_t1(i,j,k)  = rho(i,j,k)
               rho_t2(i,j,k)  = rho(i,j,k)
            enddo
         enddo
      enddo  
ccc      call cpu_time(t1)
ccc      print*,t1
c!$omp parallel private(nthreads,thread_id)
c            call omp_set_num_threads(2)
c            nthreads = omp_get_max_threads()
c            print*,'* # MAX THREADS: ',nthreads
c            nthreads = omp_get_num_threads()
c            print*,'* # ACTUAL THREADS: ',nthreads
c!$OMP SECTIONS
c!$OMP SECTION
ccc      call cpu_time(t2)
c      call tstepo_flux_t(ts_t1(:,:,:,:),ts1_t1(:,:,:,:),rho_t1(:,:,:))
c!$OMP SECTION
ccc      call cpu_time(t3)
c      call tstepo_flux_t(ts_t2(:,:,:,:),ts1_t2(:,:,:,:),rho_t2(:,:,:))
c!$OMP SECTION
      call tstepo_flux()
c!$OMP END SECTIONS
c!$omp end parallel
ccc      call cpu_time(t4)
ccc      print*,'tstepo_flux   : ',1.e6*(t2-t1)
ccc      print*,'tstepo_flux_t1: ',1.e6*(t3-t2)
ccc      print*,'tstepo_flux_t2: ',1.e6*(t4-t3)
c **********************************************************************
c **********************************************************************
c **********************************************************************

       if(imld.eq.1) then
c KICO 18/09/07, remember old T and S only so we can calculate PE 
c change. Code inefficient at present because only surface mixed points
c are compared (energy released due to eg bottom mixed layers is
c excluded, although surface plumes from coshuffle are accounted for),
c but T and S remembered everywhere.
         do i=1,imax
           do j=1,jmax
             if (k1(i,j).le.kmax)then
             do k=1,kmax
               do l=1,lmax
                 mldtsold(l,i,j,k)=ts(l,i,j,k)
               enddo
             enddo
             endif
           enddo
         enddo
c KICO 18/09/07 convection scheme itself not changed, but diagnostic
c for mixed layer depth precursor diagnosed in mldpk(1,:,:). If
c coshuffle used, maxmium depth of surface plume is in mldpk(2,:,:)
        call co(ts,mldpk)
c KICO 18/09/07 calculate PE released and multiply it by efficiency
c of recycling of PE. Need to check non-linearities properly
c represented before any commit??!!
        do i=1,imax
          do j=1,jmax
             if (k1(i,j).le.kmax)then
               mldpeconv(i,j)=0
               k=kmax
               do while(k.gt.0)
                 call eos(ec,mldtsold(1,i,j,k),mldtsold(2,i,j,k),
     1             zro(k),ieos,mldrhoold(i,j,k))
                 call eos(ec,ts(1,i,j,k),ts(2,i,j,k),
     1             zro(k),ieos,mldrhonew(i,j,k))
                 mldpeconv(i,j)=mldpeconv(i,j)+(mldrhonew(i,j,k)
     1                        -mldrhoold(i,j,k))*z2dzg(k,k)
                 k=k-1
               enddo
               mldpebuoy(i,j)=mldpeconv(i,j)+mldpelayer1(i,j)
               if (mldpebuoy(i,j).gt.0.)then
                 mldpebuoy(i,j)=mldpebuoy(i,j)*mldpebuoycoeff
               endif
c Add wind energy
               mldemix(i,j)=mldpebuoy(i,j)+mldketau(i,j)*mlddec(kmax)
             endif
          enddo
        enddo

c KICO 18/09/07  Kraus Turner scheme added. Static instability
c driven convection  has already been done in co, and will differ from
c standard KT if iconv.eq.1. krausturner uses PE released
c in co and KE from the wind to deepen the mixed layer further.
        do i=1,imax
          do j=1,jmax
             if (k1(i,j).le.kmax)then
               if (mldemix(i,j).gt.0.)then
c Apply krausturner
                 call krausturner(ts(1:lmax,i,j,1:kmax),mldpebuoy(i,j),
     1            mldketau(i,j),mldpk(1,i,j),mld(i,j),mldk(i,j),k1(i,j))
               else
c Not enough energy even to homogenise first layer. The first layer *is*
c still homogeneous for all tracers, but an mld shallower than the
c first cell is output as a diagnostic
                 mldk(i,j)=kmax
                 if(mldpelayer1(i,j).lt.0)then
                  mld(i,j)=zw(kmax-1)*(1-mldemix(i,j)/mldpelayer1(i,j))
                 else
                   mld(i,j)=zw(kmax-1)
                 endif
               endif
             endif
           enddo
         enddo
         
c diagnose only (replace original values of T,S etc.)
         if(ctrl_diagmld)then
         do i=1,imax
           do j=1,jmax
             if (k1(i,j).le.kmax)then
             do k=1,kmax
               do l=1,lmax
                 ts(l,i,j,k)=mldtsold(l,i,j,k)
               enddo
             enddo
             endif
           enddo
         enddo
         call co(ts,mldpk)
         endif

      else
c Not applying mixed layer scheme. Just call convection scheme
        call co(ts,mldpk)
      endif

c KICO, if thermobaricity is on, make sure rho calculation is vertically local
      if(ieos.ne.0)then
         do i=1,imax
           do j=1,jmax
             if (k1(i,j).le.kmax)then
               do k=1,kmax
                 call eos(ec,ts(1,i,j,k),ts(2,i,j,k),zro(k),
     1              ieos,rho(i,j,k))
               enddo
             endif
           enddo
         enddo
      endif

c periodic b.c. for rho (required at wet points)
c isoneutral code also needs ts1 bc.

      do j=1,jmax
         do k=k1(0,j),kmax
            rho(0,j,k) = rho(imax,j,k)
            do l=1,lmax
               ts1(l,0,j,k) = ts(l,imax,j,k)
            enddo
         enddo
         do k=k1(imax+1,j),kmax
            rho(imax+1,j,k) = rho(1,j,k)
            do l=1,lmax
               ts1(l,imax+1,j,k) = ts(l,1,j,k)
            enddo
         enddo
      enddo

      do 10 k=1,kmax
         do 10 j=1,jmax
            do 10 i=1,imax
               do 10 l=1,lmax
                  if(k.ge.k1(i,j)) ts1(l,i,j,k) = ts(l,i,j,k)
   10 continue

      end

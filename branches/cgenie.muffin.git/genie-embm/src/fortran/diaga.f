c
c diag.f diagnostics for EMBM atmosphere and sea ice
c last change  12/8/02 nre 
c
c AY (04/12/03) : calculations involving the ocean commented out.
c                 However, it does leave the model without a
c                 means to calculate total Earth water/heat.  This
c                 will need to be resolved.
c AY (16/12/03) : sea-ice diagnostics removed
c                 evap and precip diagnostics added

      subroutine diaga

#include "embm.cmn"

      real amin,amax,sum1
c      real sum2,sum3,vsc
      real area
c     real pptn(imax,jmax), evap(imax,jmax), pme(imax,jmax)
      real pme(maxi,maxj)

      integer i,j,iamin,iamax,jamin,jamax

      print*

      call aminmax(imax,jmax,tq(1,1,1),amin,amax,iamin,iamax
     1                  ,jamin,jamax,2,1)
      print*,'min atm T ',amin,' at ',iamin,jamin
      print*,'max atm T ',amax,' at ',iamax,jamax

      call aminmax(imax,jmax,tq(1,1,1),amin,amax,iamin,iamax
     1                  ,jamin,jamax,2,2)
      print*,'min atm q ',1e3*amin,' at ',iamin,jamin
      print*,'max atm q ',1e3*amax,' at ',iamax,jamax

c     call aminmax(imax,jmax,varice,amin,amax,iamin,iamax
c    1                  ,jamin,jamax,2,1)
c     print*,'min h ice ',amin,' at ',iamin,jamin
c     print*,'max h ice ',amax,' at ',iamax,jamax

c     call aminmax(imax,jmax,varice,amin,amax,iamin,iamax
c    1                  ,jamin,jamax,2,2)
c     print*,'min A ice ',amin,' at ',iamin,jamin
c     print*,'max A ice ',amax,' at ',iamax,jamax

      call aminmax(imax,jmax,pptn(1,1),amin,amax,iamin,iamax
     1                  ,jamin,jamax,1,1)
      print*,'min pptn  ',amin,' at ',iamin,jamin
      print*,'max pptn  ',amax,' at ',iamax,jamax

      call aminmax(imax,jmax,evap(1,1),amin,amax,iamin,iamax
     1                  ,jamin,jamax,1,1)
      print*,'min evap  ',amin,' at ',iamin,jamin
      print*,'max evap  ',amax,' at ',iamax,jamax

c AY (16/12/03) : need to calculate PME
      do i=1,imax
         do j=1,jmax
            pme(i,j) = pptn(i,j) - evap(i,j)
         enddo
      enddo

      call aminmax(imax,jmax,pme(1,1),amin,amax,iamin,iamax
     1                  ,jamin,jamax,1,1)
      print*,'min P-E   ',amin,' at ',iamin,jamin
      print*,'max P-E   ',amax,' at ',iamax,jamax

c write out SAT

      sum1 = 0
      area = 0.
      do j=1,jmax
         do i=1,imax
            sum1 = sum1 + tq(1,i,j)*ds(j)
            area = area + ds(j)
         enddo
      enddo
crma      sum1 = sum1/(imax*jmax)
      sum1 = sum1/area
      write(6,*)'average SAT',sum1

c AY (04/12/03) : commented out for now, but it does mean that total
c                 water content of the Earth is no longer calculated
c                 anywhere.  We'll need to reinstate this sort of
c                 calculation somewhere in genie.f
c compute total water content of planet (should be numerically const.)
c
c     sum1=0
c     sum2=0
c     sum3=0
c
c     do j=1,jmax
c        do i=1,imax
c           sum1 = sum1 + tq(2,i,j) 
c           sum2 = sum2 + varice(1,i,j) 
c           do k=1,kmax
c              sum3 = sum3 + ts(2,i,j,k)*dz(k)
c           enddo
c        enddo
c     enddo
c
c     vsc = ds*dphi*rsc*rsc*1e-12
c     print*,'total water (*10^12 m^3)',
c    1    (sum1*rhoao*hatmbl(2) + sum2*rhoio - sum3*dsc/saln0)
c    2     *vsc
c     print*,sum1*rhoao*hatmbl(2)*vsc,sum2*rhoio*vsc,-sum3*dsc*vsc/saln0

c AY (04/12/03) : commented out for now, but it does mean that total
c                 heat content of the Earth is no longer calculated
c                 anywhere.  We'll need to reinstate this sort of
c                 calculation somewhere in genie.f
c compute total heat content of planet (should be numerically const.)
c
c     sum1=0
c     sum2=0
c
c     do j=1,jmax
c        do i=1,imax
c           sum1 = sum1 + tq(1,i,j) 
c           do k=1,kmax
c              sum2 = sum2 + ts(1,i,j,k)*dz(k)
c           enddo
c        enddo
c     enddo
c
c     vsc = ds*dphi*rsc*rsc*1e-21
c     print*,'total heat (W*10^21)', vsc*
c    1 (sum1*rhoair*cpa*hatmbl(1) + sum2*dsc*rh0sc*cpsc 
c    2 - ghs*dt(kmax)*tsc)
c    2 ,sum1*rhoair*cpa*hatmbl(1)*vsc,sum2*dsc*rh0sc*cpsc*vsc
c    3 ,-ghs*vsc*dt(kmax)*tsc

      end

      subroutine aminmax(imax,jmax,a,amin,amax,iamin,iamax
     1                  ,jamin,jamax,lmax,l)

      implicit none

      integer i,j,imax,jmax,iamin,iamax,jamin,jamax,lmax,l
      real amin,amax,a(lmax,imax,jmax)

      amin = a(l,1,1)
      amax = a(l,1,1)
      iamin = 1
      iamax = 1
      jamin = 1
      jamax = 1

      do j=1,jmax
         do i=1,imax
            if(a(l,i,j).lt.amin)then
               amin = a(l,i,j) 
               iamin = i
               jamin = j
            endif
            if(a(l,i,j).gt.amax)then
               amax = a(l,i,j) 
               iamax = i
               jamax = j
            endif
         enddo
      enddo

      end


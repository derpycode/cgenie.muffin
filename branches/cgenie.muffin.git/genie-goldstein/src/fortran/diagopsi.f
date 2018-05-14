*
* diagopsi.f subroutine for goldstein to calculate overturning 
* streamfunctions nre 27/11/2
* hacked to find Atlantic extrema in lower half of domain nre 11/6/3
* 
* AY (05/05/04) : above hack unhacked and replaced with explicit
*                 parameter controlling the depth from which max
*                 or min overturning calculated from (overdep)
*
      subroutine diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip,
     +     iposa)

#include "ocean.cmn"

      real opsi(0:maxj,0:maxk), ou(maxj,maxk)
      real opsia(0:maxj,0:maxk), omina, omaxa
      real opsip(0:maxj,0:maxk), ominp, omaxp
      integer iposa(2)

      integer i, j, k

c Calculate meridional overturning streamfunction opsi on C grid only

      do j=0,jmax
         do k=0,kmax
            opsi(j,k) = 0
            opsia(j,k) = 0
            opsip(j,k) = 0
         enddo
      enddo

      do 70 j=1,jmax-1
         do 80 k=1,kmax-1
            ou(j,k) = 0
            do 90 i=1,imax
               ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
   90       continue
            opsi(j,k) = opsi(j,k-1) - dz(k)*ou(j,k)
   80    continue
   70 continue
c
c Pacific and Atlantic overturning streamfunctions
c
      ominp = 0
      omaxp = 0
      do j=jsf+1,jmax-1
         do k=1,kmax-1
            ou(j,k) = 0
            do i=ips(j),ipf(j)
               ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
            enddo    
            opsip(j,k) = opsip(j,k-1) - dz(k)*ou(j,k)
            if(opsip(j,k).lt.ominp.and.k.le.overdep)
     +           ominp = opsip(j,k)
            if(opsip(j,k).gt.omaxp.and.k.le.overdep)
     +           omaxp = opsip(j,k)
         enddo   
      enddo    

      omina = 0
      omaxa = 0
      do j=jsf+1,jmax-1
         do k=1,kmax-1
            ou(j,k) = 0
c conditionality to take care of "split Atlantic" (rma, 5/10/05)
            if(ias(j).gt.iaf(j)) then
               do i=ias(j),imax
                  ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
               enddo
               do i=1,iaf(j)
                  ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
               enddo
            else
               do i=ias(j),iaf(j)
                  ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
               enddo
            endif
            opsia(j,k) = opsia(j,k-1) - dz(k)*ou(j,k)
c           if(opsia(j,k).lt.omina)omina = opsia(j,k)
c           if(opsia(j,k).gt.omaxa)omaxa = opsia(j,k)
c AY (05/05/04)
c           if(opsia(j,k).lt.omina.and.k.le.kmax/2)omina = opsia(j,k)
c           if(opsia(j,k).gt.omaxa.and.k.le.kmax/2)omaxa = opsia(j,k)
            if(opsia(j,k).lt.omina.and.k.le.overdep)
     +           omina = opsia(j,k)
            if(opsia(j,k).gt.omaxa.and.k.le.overdep) then
               omaxa = opsia(j,k)
c AY (05/05/04) : position of Atlantic maximum overturning now recorded
               iposa(1) = j
               iposa(2) = k
            endif
         enddo   
      enddo    

      end

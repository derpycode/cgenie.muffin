* 
* invert.f subroutine to invert matrix for barotropic streamfunction
* c grid version variable depth last change 20/10/94
* version for non-trivial coastlines started 10/4/00
* wind part removed to wind.f 9/2/1
* variable drag 7/5/2
* variable ds 6/12/4 nre
*
      subroutine invert
#include "ocean.cmn"

      integer i,j,k,l,n,m,im

      real tv,tv1,rat

      n = imax
      m = jmax + 1

      do 710 i=1,n*m
         do 710 j=1,2*n+3
            gap(i,j)=0
 710  continue

c Set equation at Psi points, assuming periodic b.c. in i.
c Cannot solve at both i=0 and i=imax as periodicity => would
c have singular matrix. At dry points equation is trivial.

      do 650 i=1,imax
         do 650 j=0,jmax
            k=i + j*n
            if(max(k1(i,j),k1(i+1,j),k1(i,j+1),k1(i+1,j+1)).le.kmax)then
               tv = (s(j+1)*rh(1,i,j+1) - s(j)*rh(1,i,j))
     &              /(2.*dsv(j)*dphi)
               tv1 = (sv(j)*rh(2,i+1,j) - sv(j)*rh(2,i,j))
     &              /(2.*dphi*dsv(j))

               gap(k,2) = drag(1,i,j)*c(j)*c(j)*rh(1,i,j)/(ds(j)*dsv(j))
     &                  + tv1

               l=n+1
c for periodic boundary in i
               if(i.eq.1)l=2*n+1

               gap(k,l) = drag(2,i,j)*rcv(j)*rcv(j)*rdphi*rdphi
     &             *rh(2,i,j) - tv

               gap(k,n+2) = - (drag(2,i,j)*rh(2,i,j) 
     1             + drag(2,i+1,j)*rh(2,i+1,j))/(cv(j)*cv(j)*dphi
     1             *dphi) - (drag(1,i,j)*c(j)*c(j)*rh(1,i,j)/ds(j)
     1         + drag(1,i,j+1)*c(j+1)*c(j+1)*rh(1,i,j+1)/ds(j+1))/dsv(j)

               l=n+3
c for periodic boundary in i
               if(i.eq.imax)l=3

               gap(k,l) = drag(2,i+1,j)*rh(2,i+1,j)
     &                  /(cv(j)*cv(j)*dphi*dphi) + tv
               gap(k,2*n+2) = drag(1,i,j+1)*c(j+1)*c(j+1)*rh(1,i,j+1)
     &                      /(ds(j+1)*dsv(j)) - tv1

            else
               gap(k,n+2) = 1 
            endif
 650  continue

* *
* now invert the thing
* *
      do 720 i=1,n*m-1
         im=min(i+n+1,n*m)
         do 725 j=i+1,im
            rat=gap(j,n+2-j+i)/gap(i,n+2)
            ratm(j,j-i)=rat
            if(rat.ne.0)then
               do 730 k=n+2-j+i,2*n+3-j+i
                  gap(j,k)=gap(j,k) - rat*gap(i,k+j-i)
 730           continue
            endif
 725     continue
 720  continue

      end

c
c jbar.f subroutine to calculate jbar forcing for streamfunction
c variable depth last change 21/10/94
c adds time dependent pressure torque term to forcing in psi eqn
c error corrected 14/6/97, split in two 9/2/1
c altered for non-trivial islands 17/2/2, should give same answers
c slower with old code (not yet checked)
c uninitialised bottom pressures where k1=kmax could cause loss of
c significance, although they exactly cancel, hence bp now defined 
c at all wet points 16/6/3
c updated for generalised grid (RMA, 10/5/05)
c
      subroutine jbar
#include "ocean.cmn"

      logical getj(maxi,maxj)
      common /lars/getj

      integer i,j,k,l,n,ip1
c      integer m

      real tv1, tv2, tv3, tv4

      n = imax
c      m = jmax + 1

c calculate easy part of double p integral

      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax)then
               do k=k1(i,j)+1,kmax
                  bp(i,j,k) = bp(i,j,k-1) - (rho(i,j,k) + rho(i,j,k-1))
     1                        *dza(k-1)*0.5
               enddo
            endif
         enddo
      enddo

c sbp now defined if mk.gt.0 ie all wet points 

      do j=1,jmax
         do i=1,imax
            if(mk(i,j).gt.0)then
               sbp(i,j) = 0
               do k=mk(i,j)+1,kmax
                  sbp(i,j) = sbp(i,j) + bp(i,j,k)*dz(k)
               enddo
            endif
         enddo
      enddo

c periodic b.c. required for Antarctic island integral
c (if bp was defined everywhere would not need ifs) 

      do j=1,jmax
         if(k1(1,j).lt.kmax)then
            do k=k1(1,j),kmax
               bp(imax+1,j,k) = bp(1,j,k)
            enddo
         endif
         if(k1(1,j).le.kmax)then
            sbp(imax+1,j) = sbp(1,j)
         endif
      enddo

c
c calc tricky bits and add to source term for Psi, ip1 copes with
c periodicity in i, j bdy points are ignored assuming no flow out
c of north or south of domain.
c
      do 70 j=1,jmax-1
         do 70 i=1,imax
            ip1=mod(i,imax) + 1
            l = i + j*n
            if(getj(i,j))then
               tv1 = 0
               do k=ku(2,ip1,j),mk(ip1,j+1)
                  tv1 = tv1 + bp(ip1,j+1,k)*dz(k)
               enddo
               tv2 = 0
               do k=ku(2,ip1,j),mk(ip1,j)
                  tv2 = tv2 + bp(ip1,j,k)*dz(k)
               enddo
               tv3 = 0
               do k=ku(2,i,j),mk(i,j+1)
                  tv3 = tv3 + bp(i,j+1,k)*dz(k)
               enddo
               tv4 = 0
               do k=ku(2,i,j),mk(i,j)
                  tv4 = tv4 + bp(i,j,k)*dz(k)
               enddo
               gb(l) = gbold(l) + ((tv3 + sbp(i,j+1) - tv4 - sbp(i,j))
     1                 *rh(2,i,j)
     2                - (tv1 + sbp(ip1,j+1) - tv2 - sbp(ip1,j))
     3                 *rh(2,ip1,j))*rdphi*rdsv(j)
               tv1 = 0
               do k=ku(1,i,j+1),mk(ip1,j+1)
                  tv1 = tv1 + bp(ip1,j+1,k)*dz(k)
               enddo
               tv2 = 0
               do k=ku(1,i,j),mk(ip1,j)
                  tv2 = tv2 + bp(ip1,j,k)*dz(k)
               enddo
               tv3 = 0
               do k=ku(1,i,j+1),mk(i,j+1)
                  tv3 = tv3 + bp(i,j+1,k)*dz(k)
               enddo
               tv4 = 0
               do k=ku(1,i,j),mk(i,j)
                  tv4 = tv4 + bp(i,j,k)*dz(k)
               enddo
               gb(l) = gb(l) + ((tv1 + sbp(ip1,j+1) - tv3 - sbp(i,j+1))
     1                 *rh(1,i,j+1)
     1               - (tv2 + sbp(ip1,j) - tv4 - sbp(i,j))*rh(1,i,j))
     2                 *rdphi*rdsv(j)
            else
               gb(l) = gbold(l)
            endif
   70 continue
c don't currently need the following lines as gb already 
c reset to zero at these boundaries by mains
c     do i=1,imax
c        gb(i) = gbold(i)
c        gb(i+jmax*imax) = gbold(i+imax*jmax)
c     enddo
         
      end

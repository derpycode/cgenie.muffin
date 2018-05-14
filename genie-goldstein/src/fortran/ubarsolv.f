c
c ubarsolv.f subroutine to calculate barotropic velocity on c grid
c variable depth last change 21/10/94
c adds time dependent pressure torque term to forcing in psi eqn
c error corrected 14/6/97, domain extended 9/2/1
c updated for generalised grid (RMA, 10/5/05)
c
      subroutine ubarsolv(ubloc,psiloc)
#include "ocean.cmn"

      integer i,j,k,n,m,km,im

      real ubloc(2,0:maxi+1,0:maxj)
      real psiloc(0:maxi,0:maxj)

      n = imax
      m = jmax + 1
c
c solve Psi equation
c
      do 10 i=1,n*m-1
         im=min(i+n+1,n*m)
         do 10 j=i+1,im
            gb(j)=gb(j) - ratm(j,j-i)*gb(i)
   10 continue
      gb(n*m)=gb(n*m)/gap(n*m,n+2)
      do 20 i=n*m-1,1,-1
         km=min(n+1,n*m-i)
         do 30 k=1,km
            gb(i)=gb(i) - gap(i,n+2+k)*gb(i+k)
   30    continue
         gb(i)=gb(i)/gap(i,n+2)
   20 continue
c
c write to Psi for convenience (useful if ACC)
c
      do j=0,jmax
         do i=1,imax
            k = i + j*n
            psiloc(i,j) = gb(k)
         enddo
         psiloc(0,j) = psiloc(imax,j)
      enddo
c
c calculate barotropic vely where Psi (and ub) defined
c
      do j=1,jmax
         do i=1,imax
            ubloc(1,i,j) = -rh(1,i,j)*c(j)*(psiloc(i,j) - psiloc(i,j-1))
     2                     *rds(j)
         enddo
      enddo

      do j=1,jmax-1
         do i=1,imax
            ubloc(2,i,j) = rh(2,i,j)*(psiloc(i,j) - psiloc(i-1,j))
     2                     *rcv(j)*rdphi
         enddo
      enddo

c set velocity to zero at N and S boundaries
      do i=1,imax
         ubloc(2,i,jmax) = 0.
         ubloc(2,i,0) = 0.
      enddo

c periodic b.c. for ub(2) required only for island integral

      do j=1,jmax
         ubloc(2,imax+1,j) = ubloc(2,1,j)
         ubloc(1,0,j) = ubloc(1,imax,j)
         ubloc(1,imax+1,j) = ubloc(1,1,j)
         ubloc(2,0,j) = ubloc(2,imax,j)
      enddo
      ubloc(2,imax+1,0) = ubloc(2,1,0)
      ubloc(2,0,0) = ubloc(2,imax,0)
         
      end

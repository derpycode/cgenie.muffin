*
* subroutine tstipsic.f for c-goldstein
* JGS iterative implicit version 2/10/00
* cimp=1 fully implicit, cimp=0 explicit
* coeffs for iterative implicit scheme are defined at cell faces. 
* eg flux across east face = cie(i)*T(i+1) + ciw(i)*T(i)
* converted from ocean to ice 28/10/04 yka, edited nre
*
      subroutine tstipsic

#include "seaice.cmn"

      real tv, ups, ups0, pec, cimp, centre

      real cie(0:maxi,0:maxj),ciw(0:maxi,0:maxj),
     +     cin(0:maxi,0:maxj),cis(0:maxi,0:maxj)
      real varice2(0:maxi+1,0:maxj+1)

c iterations to solve timestep

      integer iits, nii

c implicit
      parameter (nii=4, ups0=0.0, cimp=0.5)
c     parameter (nii=4, ups0=0.8, cimp=1.0)
c recover old explicit 
c     parameter (nii=8, ups0=0.0, cimp=0.0)

      integer i, j, l

      logical correct

      parameter(correct=.true. )


c set b.c's on local variables

      do i=0,imax
         cin(i,0) = 0.
         cis(i,0) = 0.
         varice2(i,0) = 0.
         cin(i,jmax) = 0.
         cis(i,jmax) = 0.
         varice2(i,jmax+1) = 0.
      enddo

      do j=1,jmax
         do i=1,imax
c flux to east
            if(kmax.lt.max(k1(i,j),k1(i+1,j)))then
               cie(i,j) = 0
               ciw(i,j) = 0
            else
c              cie(i,j) = uice(1,i,j)*rc(j)*0.5*rdphi
               cie(i,j) = u(1,i,j)*rc(j)*0.5*rdphi
               tv = rc(j)*rc(j)*rdphi*diffsic*rdphi
c recover old explicit 
c              ups = sign(ups0, uice(1,i,j))
c              ups = sign(ups0, u(1,i,j))
c              pec = uice(1,i,j)*dphi/diffsic
               pec = u(1,i,j)*dphi/diffsic
               ups = pec / (2.0 + abs(pec))
               ciw(i,j) = cie(i,j)*(1+ups) + tv
               cie(i,j) = cie(i,j)*(1-ups) - tv
            endif
c flux to north
            if(kmax.lt.max(k1(i,j),k1(i,j+1)))then
               cin(i,j) = 0
               cis(i,j) = 0
            else
c              cin(i,j) = cv(j)*uice(2,i,j)*0.5
               cin(i,j) = cv(j)*u(2,i,j)*0.5
               tv = cv(j)*cv(j)*rdsv(j)*diffsic
c recover old explicit 
c              ups = sign(ups0, uice(2,i,j))
c              ups = sign(ups0, u(2,i,j))
c              pec = uice(2,i,j)*dsv(j)/diffsic
               pec = u(2,i,j)*dsv(j)/diffsic
               ups = pec / (2.0 + abs(pec))
               cis(i,j) = cin(i,j)*(1+ups) + tv
               cin(i,j) = cin(i,j)*(1-ups) - tv
            endif
         enddo
      enddo
      do j=1,jmax
         cie(0,j) = cie(imax,j)
         ciw(0,j) = ciw(imax,j)
      enddo

c loop for ice tracers (Hice, Aice)
      do l=1,2

c iterate to solve timestep

         do iits=1,nii
            do j=1,jmax
               do i=1,imax
                  varice2(i,j) = cimp*varice(l,i,j)
     1                         + (1.0 - cimp)*varice1(l,i,j)
               enddo
            enddo
            do j=1,jmax
               varice2(0,j) = varice2(imax,j)
               varice2(imax+1,j) = varice2(1,j)
            enddo
            do j=1,jmax
               do i=1,imax
                 if(kmax.ge.k1(i,j))then
                    centre = dtsic*(ciw(i,j) - cie(i-1,j)
     1                              + (cis(i,j) - cin(i,j-1))*rds(j))
                    varice(l,i,j) = (varice1(l,i,j)
     &                       *(1.0 - (1.0-cimp)*centre) - dtsic*(
     1                        - dtha(l,i,j)*tsc +
     2                          cie(i,j)  *varice2(i+1,j)
     3                        - ciw(i-1,j)*varice2(i-1,j)
     4                       + (cin(i,j)  *varice2(i,j+1)
     5                        - cis(i,j-1)*varice2(i,j-1))*rds(j)))/
     8                        (1 + cimp*centre)
                 endif
               enddo
            enddo
         enddo
         if(correct)then
            do j=1,jmax
               do i=1,imax
                  varice2(i,j) = 0.5*(varice2(i,j) + cimp*varice(l,i,j)
     1                            + (1.0 - cimp)*varice1(l,i,j))
               enddo
            enddo
            do j=1,jmax
               varice2(0,j) = varice2(imax,j)
               varice2(imax+1,j) = varice2(1,j)
            enddo
            do j=1,jmax
               do i=1,imax
                  if(kmax.ge.k1(i,j))then
c
c explicit and conservative corrector step 
c
                     varice(l,i,j) =  varice1(l,i,j)- dtsic*(
     1                           - dtha(l,i,j)*tsc +
     1                             cie(i,j)  *varice2(i+1,j)
     2                           - ciw(i-1,j)*varice2(i-1,j)
     3                          + (cin(i,j)  *varice2(i,j+1)
     4                           - cis(i,j-1)*varice2(i,j-1))*rds(j))
     7                           - dtsic*varice2(i,j)*(
     8                             ciw(i,j) - cie(i-1,j) 
     9                          + (cis(i,j) - cin(i,j-1))*rds(j))
                 endif
               enddo
            enddo
         endif
c calculate dynamical and thermal component of ice evolution
c this is purely diagnostic
         do j=1,jmax
            do i=1,imax
               if(kmax.ge.k1(i,j))then
                  variceth(l,i,j) = tsc*dtsic*dtha(l,i,j)
                  varicedy(l,i,j) = varice(l,i,j) - varice1(l,i,j)
     &            - variceth(l,i,j)
              endif
            enddo
         enddo
      enddo
      end
